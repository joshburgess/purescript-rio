module Test.RIO.Fiber.CoreSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
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
