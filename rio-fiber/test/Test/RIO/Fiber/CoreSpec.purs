module Test.RIO.Fiber.CoreSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Core" do
  describe "synchronous core" do
    it "pure returns its argument" do
      out <- runAff (pure 42 :: F.RIO () () Int) {}
      assertSuccess out 42

    it "map composes through bind" do
      out <- runAff (map (_ + 1) (pure 41) :: F.RIO () () Int) {}
      assertSuccess out 42

    it "bind threads results in order" do
      let
        prog :: F.RIO () () Int
        prog = do
          a <- pure 10
          b <- pure 20
          pure (a + b)
      out <- runAff prog {}
      assertSuccess out 30

    it "liftEffect runs synchronous effects" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Int
        prog = do
          _ <- F.liftEffect (Ref.write 7 ref)
          F.liftEffect (Ref.read ref)
      out <- runAff prog {}
      assertSuccess out 7

    it "fail surfaces a typed failure" do
      let
        prog :: F.RIO () (boom :: String) Int
        prog = F.fail (Variant.inj (Proxy :: _ "boom") "kaboom")
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "kaboom"
        _ -> fail "expected typed failure"

    it "catchAll recovers from a typed failure" do
      let
        raised :: F.RIO () (boom :: String) Int
        raised = F.fail (Variant.inj (Proxy :: _ "boom") "nope")

        recovered :: F.RIO () () Int
        recovered = F.catchAll
          ( \v ->
              (Variant.case_ # Variant.on (Proxy :: _ "boom") (\_ -> pure 99)) v
          )
          raised
      out <- runAff recovered {}
      assertSuccess out 99

    it "ask returns the environment record" do
      let
        prog :: F.RIO (greet :: String) () String
        prog = F.asks _.greet
      out <- runAff prog { greet: "hello" }
      assertSuccess out "hello"

  describe "async" do
    it "resumes from a synchronous callback" do
      let
        prog :: F.RIO () () Int
        prog = F.async \cb -> do
          cb (Right 42)
          pure (pure unit)
      out <- runAff prog {}
      assertSuccess out 42

    it "resumes from an asynchronous callback" do
      let
        prog :: F.RIO () () Int
        prog = F.async \cb -> do
          scheduleResume (cb (Right 7))
          pure (pure unit)
      out <- runAff prog {}
      assertSuccess out 7

    it "surfaces a typed failure from async" do
      let
        prog :: F.RIO () (boom :: String) Int
        prog = F.async \cb -> do
          cb (Left (Variant.inj (Proxy :: _ "boom") "from-async"))
          pure (pure unit)
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "from-async"
        _ -> fail "expected typed failure"

  describe "fork / join" do
    it "fork-then-join returns the child's result" do
      let
        child :: F.RIO () () Int
        child = pure 21

        prog :: F.RIO () () Int
        prog = do
          f <- F.fork child
          a <- F.join f
          pure (a + a)
      out <- runAff prog {}
      assertSuccess out 42

    it "join awaits an asynchronous child" do
      let
        child :: F.RIO () () Int
        child = F.async \cb -> do
          scheduleResume (cb (Right 100))
          pure (pure unit)

        prog :: F.RIO () () Int
        prog = do
          f <- F.fork child
          F.join f
      out <- runAff prog {}
      assertSuccess out 100

  describe "sleep" do
    it "suspends the fiber for approximately the requested duration" do
      let
        readMs :: Effect Number
        readMs = map (unwrap <<< unInstant) now

        prog :: F.RIO () () Number
        prog = do
          t0 <- F.liftEffect readMs
          F.sleep (Milliseconds 50.0)
          t1 <- F.liftEffect readMs
          pure (t1 - t0)
      out <- runAff prog {}
      case out of
        Success ms
          | ms >= 40.0 -> pure unit
          | otherwise -> fail ("slept too briefly: " <> show ms <> "ms")
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "preemption" do
    it "yields after the tick budget so a sibling fiber can run" do
      ref <- liftEffect (Ref.new 0)
      let
        -- A sibling that writes once.
        sibling :: F.RIO () () Unit
        sibling = F.liftEffect (Ref.write 1 ref)

        -- A long synchronous chain that should exceed the default
        -- TICK_BUDGET (currently 2048). Without preemption the
        -- parent would drive its whole chain to completion before
        -- the sibling ever ran, so the ref read at the end would
        -- be 0. With preemption, the parent yields mid-chain,
        -- the sibling runs, and the read sees 1.
        busy :: Int -> F.RIO () () Unit
        busy 0 = pure unit
        busy n = pure unit *> busy (n - 1)

        prog :: F.RIO () () Int
        prog = do
          _ <- F.fork sibling
          busy 5000
          F.liftEffect (Ref.read ref)
      out <- runAff prog {}
      assertSuccess out 1

  describe "finalizers" do
    it "ensuring runs the finalizer after success" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Int
        prog = F.ensuring (F.liftEffect (Ref.write 1 ref)) (pure 42)
      out <- runAff prog {}
      assertSuccess out 42
      finVal <- liftEffect (Ref.read ref)
      finVal `shouldEqual` 1

    it "ensuring runs the finalizer after a typed failure and re-raises" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () (boom :: String) Int
        prog = F.ensuring
          (F.liftEffect (Ref.write 1 ref))
          (F.fail (Variant.inj (Proxy :: _ "boom") "x"))
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)
      finVal <- liftEffect (Ref.read ref)
      finVal `shouldEqual` 1

    it "ensuring runs the finalizer when the action is interrupted" do
      finRef <- liftEffect (Ref.new 0)
      let
        -- An action that gets interrupted while sleeping.
        action :: F.RIO () () Int
        action = F.ensuring
          (F.liftEffect (Ref.write 1 finRef))
          (F.sleep (Milliseconds 100.0) *> pure 0)

        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork action
          F.sleep (Milliseconds 10.0)
          F.interrupt f
          _ <- F.join f
          pure unit
      out <- runAff prog {}
      case out of
        Interrupted -> pure unit
        other -> fail ("expected Interrupted, got " <> describeOutcome other)
      finVal <- liftEffect (Ref.read finRef)
      finVal `shouldEqual` 1

    it "nested ensuring runs inner finalizer first" do
      events <- liftEffect (Ref.new ([] :: Array String))
      let
        record :: String -> F.RIO () () Unit
        record s = F.liftEffect $
          Ref.modify_ (\xs -> xs <> [ s ]) events

        prog :: F.RIO () () Unit
        prog =
          F.ensuring (record "outer")
            ( F.ensuring (record "inner")
                (record "body")
            )
      out <- runAff prog {}
      assertSuccess out unit
      seq <- liftEffect (Ref.read events)
      seq `shouldEqual` [ "body", "inner", "outer" ]

    it "bracket runs release on a successful use" do
      events <- liftEffect (Ref.new ([] :: Array String))
      let
        record :: String -> F.RIO () () Unit
        record s = F.liftEffect $
          Ref.modify_ (\xs -> xs <> [ s ]) events

        prog :: F.RIO () () Int
        prog = F.bracket
          (record "acquire" *> pure 42)
          (\_ -> record "release")
          (\n -> record "use" *> pure (n + 1))
      out <- runAff prog {}
      assertSuccess out 43
      seq <- liftEffect (Ref.read events)
      seq `shouldEqual` [ "acquire", "use", "release" ]

    it "bracket runs release on a failing use" do
      events <- liftEffect (Ref.new ([] :: Array String))
      let
        record :: forall e. String -> F.RIO () e Unit
        record s = F.liftEffect $
          Ref.modify_ (\xs -> xs <> [ s ]) events

        prog :: F.RIO () (boom :: String) Int
        prog = F.bracket
          (record "acquire" *> pure 42)
          (\_ -> record "release")
          ( \_ -> record "use" *>
              F.fail (Variant.inj (Proxy :: _ "boom") "nope")
          )
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)
      seq <- liftEffect (Ref.read events)
      seq `shouldEqual` [ "acquire", "use", "release" ]

  describe "race" do
    it "returns the result of the side that finishes first" do
      let
        winner :: F.RIO () () Int
        winner = pure 1

        loser :: F.RIO () () Int
        loser = F.sleep (Milliseconds 50.0) *> pure 2
      out <- runAff (F.race winner loser) {}
      assertSuccess out 1

    it "is symmetric: right side wins when it finishes first" do
      let
        winner :: F.RIO () () Int
        winner = pure 2

        loser :: F.RIO () () Int
        loser = F.sleep (Milliseconds 50.0) *> pure 1
      out <- runAff (F.race loser winner) {}
      assertSuccess out 2

    it "interrupts the loser so its finalizer runs" do
      loserFinalized <- liftEffect (Ref.new false)
      let
        winner :: F.RIO () () Int
        winner = pure 1

        -- A loser that suspends forever (no resume), with a
        -- finalizer that should fire when race interrupts it.
        loser :: F.RIO () () Int
        loser = F.ensuring
          (F.liftEffect (Ref.write true loserFinalized))
          (F.async \_cb -> pure (pure unit))

        prog :: F.RIO () () Int
        prog = do
          r <- F.race winner loser
          -- give the loser microtask a chance to run its finalizer
          F.sleep (Milliseconds 20.0)
          pure r
      out <- runAff prog {}
      assertSuccess out 1
      finalized <- liftEffect (Ref.read loserFinalized)
      finalized `shouldEqual` true

    it "propagates a typed failure from the winner" do
      let
        loud :: F.RIO () (boom :: String) Int
        loud = F.fail (Variant.inj (Proxy :: _ "boom") "lost")

        slow :: F.RIO () (boom :: String) Int
        slow = F.sleep (Milliseconds 50.0) *> pure 0
      out <- runAff (F.race loud slow) {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "lost"
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "raceAll" do
    it "returns the fastest of many" do
      let
        slow :: Milliseconds -> Int -> F.RIO () () Int
        slow ms n = F.sleep ms *> pure n

        prog :: F.RIO () () Int
        prog = F.raceAll
          [ slow (Milliseconds 50.0) 1
          , slow (Milliseconds 10.0) 2
          , slow (Milliseconds 30.0) 3
          ]
      out <- runAff prog {}
      assertSuccess out 2

    it "interrupts every loser" do
      finalizedA <- liftEffect (Ref.new false)
      finalizedC <- liftEffect (Ref.new false)
      let
        winner :: F.RIO () () Int
        winner = pure 99

        loser :: Ref.Ref Boolean -> F.RIO () () Int
        loser ref = F.ensuring
          (F.liftEffect (Ref.write true ref))
          (F.async \_cb -> pure (pure unit))

        prog :: F.RIO () () Int
        prog = do
          r <- F.raceAll [ loser finalizedA, winner, loser finalizedC ]
          F.sleep (Milliseconds 20.0)
          pure r
      out <- runAff prog {}
      assertSuccess out 99
      a <- liftEffect (Ref.read finalizedA)
      c <- liftEffect (Ref.read finalizedC)
      a `shouldEqual` true
      c `shouldEqual` true

  describe "timeout" do
    it "returns Just when the action finishes before the timeout" do
      let
        prog :: F.RIO () () (Maybe Int)
        prog = F.timeout (Milliseconds 100.0) (pure 42)
      out <- runAff prog {}
      assertSuccess out (Just 42)

    it "returns Nothing when the timeout wins" do
      let
        prog :: F.RIO () () (Maybe Int)
        prog = F.timeout (Milliseconds 10.0)
          (F.sleep (Milliseconds 200.0) *> pure 42)
      out <- runAff prog {}
      assertSuccess out Nothing

    it "interrupts the action when the timeout fires" do
      finalized <- liftEffect (Ref.new false)
      let
        slow :: F.RIO () () Int
        slow = F.ensuring
          (F.liftEffect (Ref.write true finalized))
          (F.sleep (Milliseconds 200.0) *> pure 0)

        prog :: F.RIO () () (Maybe Int)
        prog = do
          r <- F.timeout (Milliseconds 10.0) slow
          F.sleep (Milliseconds 30.0)
          pure r
      out <- runAff prog {}
      assertSuccess out Nothing
      fin <- liftEffect (Ref.read finalized)
      fin `shouldEqual` true

  describe "parTraverse" do
    it "runs all in parallel and collects results in order" do
      let
        prog :: F.RIO () () (Array Int)
        prog = F.parTraverse (\n -> pure (n + 1)) [ 1, 2, 3 ]
      out <- runAff prog {}
      assertSuccess out [ 2, 3, 4 ]

    it "returns an empty array for an empty input" do
      let
        prog :: F.RIO () () (Array Int)
        prog = F.parTraverse (\n -> pure (n + 1)) []
      out <- runAff prog {}
      assertSuccess out []

    it "fails fast when one branch fails and interrupts the rest" do
      siblingFinalized <- liftEffect (Ref.new false)
      let
        f :: Int -> F.RIO () (boom :: String) Int
        f 2 = F.fail (Variant.inj (Proxy :: _ "boom") "two")
        f _ = F.ensuring
          (F.liftEffect (Ref.write true siblingFinalized))
          (F.async \_cb -> pure (pure unit))

        prog :: F.RIO () (boom :: String) (Array Int)
        prog = F.parTraverse f [ 1, 2, 3 ]
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "two"
        other -> fail ("expected Fail, got " <> describeOutcome other)
      -- the surviving siblings should have been interrupted; their
      -- finalizers ran.
      finalized <- liftEffect (Ref.read siblingFinalized)
      finalized `shouldEqual` true

  describe "zipPar" do
    it "runs both branches concurrently and pairs the results" do
      let
        prog :: F.RIO () () (Tuple Int String)
        prog = F.zipPar (pure 1) (pure "two")
      out <- runAff prog {}
      assertSuccess out (Tuple 1 "two")

  describe "interrupt" do
    it "interrupting a suspended forked fiber fires its canceller and propagates Interrupted" do
      ref <- liftEffect (Ref.new false)
      let
        neverResume :: F.RIO () () Int
        neverResume = F.async \_cb ->
          pure (Ref.write true ref)

        yieldOnce :: F.RIO () () Unit
        yieldOnce = F.async \cb -> do
          scheduleResume (cb (Right unit))
          pure (pure unit)

        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork neverResume
          -- yield so neverResume gets scheduled, runs, and suspends.
          yieldOnce
          F.interrupt f
          _ <- F.join f
          pure unit
      out <- runAff prog {}
      case out of
        Interrupted -> do
          cancelled <- liftEffect (Ref.read ref)
          cancelled `shouldEqual` true
        other -> fail ("expected Interrupted, got " <> describeOutcome other)

assertSuccess
  :: forall e a
   . Eq a
  => Show a
  => Outcome e a
  -> a
  -> Aff Unit
assertSuccess (Success a) expected = a `shouldEqual` expected
assertSuccess other _ = fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"

foreign import scheduleResume :: Effect Unit -> Effect Unit
