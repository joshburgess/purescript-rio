module Test.RIO.Fiber.CoreSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff, delay, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Now (now)
import Effect.Ref as Ref
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
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

  describe "forkInline" do
    it "drives a sync-bodied child to completion before the parent's next op" do
      ref <- liftEffect (Ref.new [])
      let
        push :: String -> Effect Unit
        push s = Ref.modify_ (\xs -> xs <> [ s ]) ref

        child :: F.RIO () () Unit
        child = F.liftEffect (push "child")

        prog :: F.RIO () () (Array String)
        prog = do
          _ <- F.forkInline child
          F.liftEffect (push "parent")
          F.liftEffect (Ref.read ref)
      out <- runAff prog {}
      assertSuccess out [ "child", "parent" ]

    it "regular fork leaves the parent's next op observable before the child runs" do
      ref <- liftEffect (Ref.new [])
      let
        push :: String -> Effect Unit
        push s = Ref.modify_ (\xs -> xs <> [ s ]) ref

        child :: F.RIO () () Unit
        child = F.liftEffect (push "child")

        prog :: F.RIO () () (Array String)
        prog = do
          f <- F.fork child
          F.liftEffect (push "parent")
          _ <- F.join f
          F.liftEffect (Ref.read ref)
      out <- runAff prog {}
      assertSuccess out [ "parent", "child" ]

    it "join on an inline-completed child resolves without suspending" do
      let
        child :: F.RIO () () Int
        child = pure 21

        prog :: F.RIO () () Int
        prog = do
          f <- F.forkInline child
          a <- F.join f
          pure (a + a)
      out <- runAff prog {}
      assertSuccess out 42

    it "hands back a live handle when the child suspends" do
      let
        child :: F.RIO () () Int
        child = F.async \cb -> do
          scheduleResume (cb (Right 7))
          pure (pure unit)

        prog :: F.RIO () () Int
        prog = do
          f <- F.forkInline child
          F.join f
      out <- runAff prog {}
      assertSuccess out 7

  describe "forkAll / joinAll" do
    it "forks every body and joins the results in order" do
      let
        bodies :: Array (F.RIO () () Int)
        bodies = map pure [ 1, 2, 3, 4, 5 ]

        prog :: F.RIO () () (Array Int)
        prog = do
          fibs <- F.forkAll bodies
          F.joinAll fibs
      out <- runAff prog {}
      assertSuccess out [ 1, 2, 3, 4, 5 ]

    it "joinAll returns [] for an empty fiber array" do
      let
        prog :: F.RIO () () (Array Int)
        prog = F.joinAll []
      out <- runAff prog {}
      assertSuccess out []

    it "joinAll awaits children that suspend on async" do
      let
        body :: Int -> F.RIO () () Int
        body n = F.async \cb -> do
          scheduleResume (cb (Right (n * 10)))
          pure (pure unit)

        prog :: F.RIO () () (Array Int)
        prog = do
          fibs <- F.forkAll (map body [ 1, 2, 3 ])
          F.joinAll fibs
      out <- runAff prog {}
      assertSuccess out [ 10, 20, 30 ]

    it "joinAll propagates the first failure it observes" do
      let
        ok :: Int -> F.RIO () (boom :: String) Int
        ok n = pure n

        bad :: F.RIO () (boom :: String) Int
        bad = F.fail (Variant.inj (Proxy :: _ "boom") "nope")

        prog :: F.RIO () (boom :: String) (Array Int)
        prog = do
          fibs <- F.forkAll [ ok 1, bad, ok 3 ]
          F.joinAll fibs
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "nope"
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "forkAllInline / joinAll" do
    it "forks every body and joins the results in order" do
      let
        bodies :: Array (F.RIO () () Int)
        bodies = map pure [ 10, 20, 30, 40 ]

        prog :: F.RIO () () (Array Int)
        prog = do
          fibs <- F.forkAllInline bodies
          F.joinAll fibs
      out <- runAff prog {}
      assertSuccess out [ 10, 20, 30, 40 ]

    it "returns [] on an empty array" do
      let
        prog :: F.RIO () () (Array Int)
        prog = do
          fibs <- F.forkAllInline ([] :: Array (F.RIO () () Int))
          F.joinAll fibs
      out <- runAff prog {}
      assertSuccess out []

    it "drives sync-bodied children synchronously before returning" do
      ref <- liftEffect (Ref.new [])
      let
        body :: Int -> F.RIO () () Int
        body n = do
          F.liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) ref)
          pure n

        prog :: F.RIO () () (Array Int)
        prog = do
          fibs <- F.forkAllInline (map body [ 1, 2, 3 ])
          F.joinAll fibs
      out <- runAff prog {}
      assertSuccess out [ 1, 2, 3 ]
      seen <- liftEffect (Ref.read ref)
      seen `shouldEqual` [ 1, 2, 3 ]

    it "still suspends on async bodies" do
      let
        body :: Int -> F.RIO () () Int
        body n = F.async \cb -> do
          scheduleResume (cb (Right (n * 100)))
          pure (pure unit)

        prog :: F.RIO () () (Array Int)
        prog = do
          fibs <- F.forkAllInline (map body [ 1, 2, 3 ])
          F.joinAll fibs
      out <- runAff prog {}
      assertSuccess out [ 100, 200, 300 ]

    it "propagates a sync failure from any child via joinAll" do
      let
        ok :: Int -> F.RIO () (boom :: String) Int
        ok n = pure n

        bad :: F.RIO () (boom :: String) Int
        bad = F.fail (Variant.inj (Proxy :: _ "boom") "nope")

        prog :: F.RIO () (boom :: String) (Array Int)
        prog = do
          fibs <- F.forkAllInline [ ok 1, bad, ok 3 ]
          F.joinAll fibs
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "nope"
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "Functor (opMap)" do
    it "satisfies identity: map identity = identity" do
      out <- runAff (map identity (pure 7) :: F.RIO () () Int) {}
      assertSuccess out 7

    it "satisfies composition: map (f <<< g) = map f <<< map g" do
      let
        f = (_ * 3)
        g = (_ + 1)
        lhs = map (f <<< g) (pure 4 :: F.RIO () () Int)
        rhs = map f (map g (pure 4 :: F.RIO () () Int))
      lOut <- runAff lhs {}
      rOut <- runAff rhs {}
      assertSuccess lOut 15
      assertSuccess rOut 15

    it "maps through a suspending op" do
      let
        body :: F.RIO () () Int
        body = F.async \cb -> do
          scheduleResume (cb (Right 6))
          pure (pure unit)
      out <- runAff (map (_ * 7) body) {}
      assertSuccess out 42

    it "propagates failure through map" do
      let
        prog :: F.RIO () (boom :: String) Int
        prog = map (_ + 1)
          (F.fail (Variant.inj (Proxy :: _ "boom") "kaboom"))
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "kaboom"
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "Apply (opApply)" do
    it "applies a pure function to a pure value" do
      out <- runAff (pure (_ + 1) <*> pure 41 :: F.RIO () () Int) {}
      assertSuccess out 42

    it "satisfies identity: pure identity <*> v = v" do
      out <- runAff (pure identity <*> pure 7 :: F.RIO () () Int) {}
      assertSuccess out 7

    it "threads through two suspending ops in left-to-right order" do
      ref <- liftEffect (Ref.new [])
      let
        tag :: String -> Int -> F.RIO () () Int
        tag s n = F.async \cb -> do
          Ref.modify_ (\xs -> xs <> [ s ]) ref
          scheduleResume (cb (Right n))
          pure (pure unit)

        prog :: F.RIO () () Int
        prog = (tag "f" 0 *> pure (_ + 1)) <*> tag "a" 41
      out <- runAff prog {}
      assertSuccess out 42
      seen <- liftEffect (Ref.read ref)
      seen `shouldEqual` [ "f", "a" ]

    it "propagates failure from the function arg" do
      let
        prog :: F.RIO () (boom :: String) Int
        prog = F.fail (Variant.inj (Proxy :: _ "boom") "nope") <*> pure 1
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "nope"
        other -> fail ("expected Fail, got " <> describeOutcome other)

    it "propagates failure from the value arg" do
      let
        prog :: F.RIO () (boom :: String) Int
        prog = pure (_ + 1) <*>
          F.fail (Variant.inj (Proxy :: _ "boom") "nope")
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "nope"
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "forEach" do
    it "runs the body for each element and collects results in order" do
      let
        prog :: F.RIO () () (Array Int)
        prog = F.forEach (\n -> pure (n * 10)) [ 1, 2, 3, 4 ]
      out <- runAff prog {}
      assertSuccess out [ 10, 20, 30, 40 ]

    it "returns [] on an empty array" do
      out <- runAff (F.forEach (\(n :: Int) -> pure n) [] :: F.RIO () () (Array Int)) {}
      assertSuccess out []

    it "runs sequentially: side effects observe earlier iterations" do
      ref <- liftEffect (Ref.new [])
      let
        body :: Int -> F.RIO () () Int
        body n = do
          F.liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) ref)
          pure n
      _ <- runAff (F.forEach body [ 1, 2, 3 ]) {}
      seen <- liftEffect (Ref.read ref)
      seen `shouldEqual` [ 1, 2, 3 ]

    it "suspends and resumes through async bodies" do
      let
        body :: Int -> F.RIO () () Int
        body n = F.async \cb -> do
          scheduleResume (cb (Right (n + 100)))
          pure (pure unit)
      out <- runAff (F.forEach body [ 1, 2, 3 ]) {}
      assertSuccess out [ 101, 102, 103 ]

    it "propagates the first failure and discards later iterations" do
      ref <- liftEffect (Ref.new [])
      let
        body :: Int -> F.RIO () (boom :: String) Int
        body n = do
          F.liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) ref)
          if n == 2 then F.fail (Variant.inj (Proxy :: _ "boom") "stop")
          else pure n
      out <- runAff (F.forEach body [ 1, 2, 3, 4 ]) {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "stop"
        other -> fail ("expected Fail, got " <> describeOutcome other)
      seen <- liftEffect (Ref.read ref)
      seen `shouldEqual` [ 1, 2 ]

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
        -- TICK_BUDGET (currently 4096). Without preemption the
        -- parent would drive its whole chain to completion before
        -- the sibling ever ran, so the ref read at the end would
        -- be 0. With preemption, the parent yields mid-chain,
        -- the sibling runs, and the read sees 1.
        --
        -- The body deliberately uses an APPLY shape (`*>` desugars
        -- to `apply (map (const id) lhs) rhs`). With the OP_MAP /
        -- OP_APPLY smart-constructor fusions the `map (const id)`
        -- side collapses into the inner Sync, but the surrounding
        -- APPLY stays put (neither side is Pure), so every iteration
        -- still pays an outer-loop dispatch. A pure `pure unit *> ...`
        -- would fuse all the way down to a single `pure unit`, and a
        -- pure `bind` chain would chain through the BIND fast paths
        -- without consuming ticks. APPLY is the shape that genuinely
        -- exercises preemption.
        busy :: Int -> F.RIO () () Unit
        busy 0 = pure unit
        busy n = F.liftEffect (pure unit) *> busy (n - 1)

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

    it "ensuringWith hands the success branch to the handler" do
      ref <- liftEffect (Ref.new ([] :: Array String))
      let
        prog :: F.RIO () () Int
        prog = F.ensuringWith (pure 42) \result -> F.liftEffect $
          case result of
            Right a -> Ref.modify_ (\xs -> xs <> [ "ok:" <> show a ]) ref
            Left _ -> Ref.modify_ (\xs -> xs <> [ "fail" ]) ref
      out <- runAff prog {}
      assertSuccess out 42
      seq <- liftEffect (Ref.read ref)
      seq `shouldEqual` [ "ok:42" ]

    it "ensuringWith hands the failure cause to the handler and re-raises" do
      ref <- liftEffect (Ref.new ([] :: Array String))
      let
        prog :: F.RIO () (boom :: String) Int
        prog = F.ensuringWith
          (F.fail (Variant.inj (Proxy :: _ "boom") "no"))
          \result -> F.liftEffect $ case result of
            Right _ -> Ref.modify_ (\xs -> xs <> [ "ok" ]) ref
            Left c ->
              let tag = case Cause.firstFailure c of
                    Just v -> Variant.case_ # Variant.on (Proxy :: _ "boom") identity $ v
                    Nothing -> "no-failure"
              in Ref.modify_ (\xs -> xs <> [ "fail:" <> tag ]) ref
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)
      seq <- liftEffect (Ref.read ref)
      seq `shouldEqual` [ "fail:no" ]

    it "ensuringWith hands the interrupt cause to the handler" do
      ref <- liftEffect (Ref.new ([] :: Array String))
      let
        action :: F.RIO () () Int
        action = F.ensuringWith
          (F.sleep (Milliseconds 100.0) *> pure 0)
          \result -> F.liftEffect $ case result of
            Right _ -> Ref.modify_ (\xs -> xs <> [ "ok" ]) ref
            Left c ->
              if Cause.isInterrupted c then
                Ref.modify_ (\xs -> xs <> [ "interrupt" ]) ref
              else
                Ref.modify_ (\xs -> xs <> [ "other" ]) ref

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
      seq <- liftEffect (Ref.read ref)
      seq `shouldEqual` [ "interrupt" ]

    it "onExit does not fire on success" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Int
        prog = F.onExit (pure 7) \_ ->
          F.liftEffect (Ref.write 1 ref)
      out <- runAff prog {}
      assertSuccess out 7
      finVal <- liftEffect (Ref.read ref)
      finVal `shouldEqual` 0

    it "onExit fires on typed failure with the failure cause" do
      ref <- liftEffect (Ref.new "init")
      let
        prog :: F.RIO () (boom :: String) Int
        prog = F.onExit
          (F.fail (Variant.inj (Proxy :: _ "boom") "kaput"))
          \c -> F.liftEffect $
            case Cause.firstFailure c of
              Just v ->
                let tag = Variant.case_
                      # Variant.on (Proxy :: _ "boom") identity
                      $ v
                in Ref.write ("got:" <> tag) ref
              Nothing -> Ref.write "no-failure" ref
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)
      finVal <- liftEffect (Ref.read ref)
      finVal `shouldEqual` "got:kaput"

    it "onExit fires on interrupt with an interrupt cause" do
      ref <- liftEffect (Ref.new "init")
      let
        action :: F.RIO () () Int
        action = F.onExit
          (F.sleep (Milliseconds 100.0) *> pure 0)
          \c -> F.liftEffect $
            if Cause.isInterrupted c then Ref.write "interrupt" ref
            else Ref.write "other" ref

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
      finVal <- liftEffect (Ref.read ref)
      finVal `shouldEqual` "interrupt"

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

    it "a single failure waits for the other side; success still wins" do
      let
        loud :: F.RIO () (boom :: String) Int
        loud = F.fail (Variant.inj (Proxy :: _ "boom") "lost")

        slow :: F.RIO () (boom :: String) Int
        slow = F.sleep (Milliseconds 20.0) *> pure 7
      out <- runAff (F.race loud slow) {}
      assertSuccess out 7

    it "if both branches fail, race resumes with both causes composed" do
      let
        l :: F.RIO () (boom :: String) Int
        l = F.fail (Variant.inj (Proxy :: _ "boom") "left")

        r :: F.RIO () (boom :: String) Int
        r = F.sleep (Milliseconds 10.0) *>
          F.fail (Variant.inj (Proxy :: _ "boom") "right")

        prog :: F.RIO () () (Either (Array String) Int)
        prog = do
          ec <- F.causeOf (F.race l r)
          pure case ec of
            Right n -> Right n
            Left cause ->
              Left
                ( map
                    (Variant.case_ # Variant.on (Proxy :: _ "boom") identity)
                    (Cause.failures cause)
                )
      out <- runAff prog {}
      case out of
        Success (Left names) -> do
          (Array.length names) `shouldEqual` 2
          (Array.elem "left" names) `shouldEqual` true
          (Array.elem "right" names) `shouldEqual` true
        Success (Right _) -> fail "expected Left (composed failures), got Right"
        other -> fail ("expected Success, got " <> describeOutcome other)

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

    it "if every branch fails, raceAll composes the causes" do
      let
        boom :: String -> F.RIO () (boom :: String) Int
        boom tag = F.fail (Variant.inj (Proxy :: _ "boom") tag)

        prog :: F.RIO () () (Either (Array String) Int)
        prog = do
          ec <- F.causeOf
            ( F.raceAll
                [ boom "a"
                , F.sleep (Milliseconds 5.0) *> boom "b"
                , F.sleep (Milliseconds 10.0) *> boom "c"
                ]
            )
          pure case ec of
            Right n -> Right n
            Left cause ->
              Left
                ( map
                    (Variant.case_ # Variant.on (Proxy :: _ "boom") identity)
                    (Cause.failures cause)
                )
      out <- runAff prog {}
      case out of
        Success (Left names) -> do
          (Array.length names) `shouldEqual` 3
          (Array.elem "a" names) `shouldEqual` true
          (Array.elem "b" names) `shouldEqual` true
          (Array.elem "c" names) `shouldEqual` true
        Success (Right _) -> fail "expected Left (composed failures), got Right"
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "a single success still wins after siblings fail" do
      let
        boom :: String -> F.RIO () (boom :: String) Int
        boom tag = F.fail (Variant.inj (Proxy :: _ "boom") tag)

        winner :: F.RIO () (boom :: String) Int
        winner = F.sleep (Milliseconds 15.0) *> pure 1

        prog :: F.RIO () (boom :: String) Int
        prog = F.raceAll [ boom "a", winner, boom "b" ]
      out <- runAff prog {}
      assertSuccess out 1

    it "a single-element raceAll runs the inner op directly" do
      let
        prog :: F.RIO () () Int
        prog = F.raceAll [ pure 17 ]
      out <- runAff prog {}
      assertSuccess out 17

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

  describe "awaitOutcome" do
    it "reports Success when the fiber succeeded" do
      let
        prog :: F.RIO () () (Outcome () Int)
        prog = do
          fib <- F.fork (pure 7)
          F.awaitOutcome fib
      out <- runAff prog {}
      case out of
        Success (Success n) -> n `shouldEqual` 7
        other -> fail
          ("expected Success (Success 7), got " <> describeOutcome other)

    it "reports Fail when the fiber raised a typed error (no propagation)" do
      let
        boom :: F.RIO () (boom :: String) Int
        boom = F.fail (Variant.inj (Proxy :: _ "boom") "x")

        prog :: F.RIO () (boom :: String) (Outcome (boom :: String) Int)
        prog = do
          fib <- F.fork boom
          F.awaitOutcome fib
      out <- runAff prog {}
      case out of
        Success (Fail v) ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "x"
        other -> fail
          ("expected Success (Fail ..), got " <> describeOutcome other)

    it "reports Interrupted when the fiber was interrupted" do
      let
        prog :: F.RIO () () (Outcome () Unit)
        prog = do
          fib <- F.fork F.never
          F.interrupt fib
          F.awaitOutcome fib
      out <- runAff prog {}
      case out of
        Success Interrupted -> pure unit
        other -> fail
          ("expected Success Interrupted, got " <> describeOutcome other)

  describe "awaitAllOutcomes" do
    it "reports each fiber's individual outcome" do
      let
        boom :: F.RIO () (boom :: String) Int
        boom = F.fail (Variant.inj (Proxy :: _ "boom") "two")

        ok :: Int -> F.RIO () (boom :: String) Int
        ok n = pure n

        prog
          :: F.RIO () (boom :: String) (Array (Outcome (boom :: String) Int))
        prog = do
          f1 <- F.fork (ok 1)
          f2 <- F.fork boom
          f3 <- F.fork (ok 3)
          F.awaitAllOutcomes [ f1, f2, f3 ]
      out <- runAff prog {}
      case out of
        Success outcomes -> do
          (Array.length outcomes) `shouldEqual` 3
          case Array.index outcomes 0 of
            Just (Success n) -> n `shouldEqual` 1
            _ -> fail "expected Success 1 at index 0"
          case Array.index outcomes 1 of
            Just (Fail v) ->
              (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
                `shouldEqual` "two"
            _ -> fail "expected Fail at index 1"
          case Array.index outcomes 2 of
            Just (Success n) -> n `shouldEqual` 3
            _ -> fail "expected Success 3 at index 2"
        other -> fail
          ("expected Success of outcomes, got " <> describeOutcome other)

    it "returns an empty array for an empty input" do
      let
        prog :: F.RIO () () (Array (Outcome () Int))
        prog = F.awaitAllOutcomes []
      out <- runAff prog {}
      case out of
        Success outcomes -> (Array.length outcomes) `shouldEqual` 0
        other -> fail
          ("expected Success of [], got " <> describeOutcome other)

  describe "zipFiber / zipWithFiber" do
    it "combines two successes via Tuple" do
      let
        prog :: F.RIO () () (Tuple Int String)
        prog = do
          f1 <- F.fork (pure 1)
          f2 <- F.fork (pure "two")
          combined <- F.zipFiber f1 f2
          F.join combined
      out <- runAff prog {}
      assertSuccess out (Tuple 1 "two")

    it "zipWithFiber applies the combining function" do
      let
        prog :: F.RIO () () Int
        prog = do
          f1 <- F.fork (pure 3)
          f2 <- F.fork (pure 4)
          combined <- F.zipWithFiber (+) f1 f2
          F.join combined
      out <- runAff prog {}
      assertSuccess out 7

    it "fails fast when the first source fails" do
      let
        boom :: F.RIO () (boom :: String) Int
        boom = F.fail (Variant.inj (Proxy :: _ "boom") "left")

        prog :: F.RIO () (boom :: String) (Tuple Int Int)
        prog = do
          f1 <- F.fork boom
          f2 <- F.fork (F.sleep (Milliseconds 5.0) *> pure 1)
          combined <- F.zipFiber f1 f2
          F.join combined
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "left"
        other -> fail ("expected Fail, got " <> describeOutcome other)

    it "interrupting the combined fiber leaves the sources untouched" do
      sourceInterrupted <- liftEffect (Ref.new false)
      let
        slow :: F.RIO () () Int
        slow = F.ensuring
          ( F.liftEffect
              ( Ref.write true sourceInterrupted
              )
          )
          (F.sleep (Milliseconds 50.0) *> pure 0)

        prog :: F.RIO () () Unit
        prog = do
          f1 <- F.fork slow
          f2 <- F.fork slow
          combined <- F.zipFiber f1 f2
          F.interrupt combined
          F.sleep (Milliseconds 10.0)
      _ <- runAff prog {}
      -- the source fibers were not interrupted by the combined
      -- fiber's interrupt; they should still be running (and so the
      -- finalizer should not have fired in 10ms).
      seen <- liftEffect (Ref.read sourceInterrupted)
      seen `shouldEqual` false

  describe "die" do
    it "surfaces as a Die outcome with the original error" do
      let
        prog :: F.RIO () () Int
        prog = F.die (error "kaboom")
      out <- runAff prog {}
      case out of
        Die e -> message e `shouldEqual` "kaboom"
        other -> fail ("expected Die, got " <> describeOutcome other)

    it "ensuring runs the finalizer after a defect and re-raises" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Int
        prog = F.ensuring
          (F.liftEffect (Ref.write 1 ref))
          (F.die (error "boom"))
      out <- runAff prog {}
      case out of
        Die _ -> pure unit
        other -> fail ("expected Die, got " <> describeOutcome other)
      finVal <- liftEffect (Ref.read ref)
      finVal `shouldEqual` 1

    it "bracket runs release when use defects" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Int
        prog = F.bracket
          (pure 7)
          (\_ -> F.liftEffect (Ref.modify_ (_ + 1) ref))
          (\_ -> F.die (error "kaboom"))
      out <- runAff prog {}
      case out of
        Die e -> message e `shouldEqual` "kaboom"
        other -> fail ("expected Die, got " <> describeOutcome other)
      n <- liftEffect (Ref.read ref)
      n `shouldEqual` 1

    it "catchAll does not catch defects" do
      let
        prog :: F.RIO () () Int
        prog = F.catchAll (\_ -> pure 99)
          (F.die (error "uncaught") :: F.RIO () () Int)
      out <- runAff prog {}
      case out of
        Die e -> message e `shouldEqual` "uncaught"
        other -> fail ("expected Die, got " <> describeOutcome other)

    it "raceAll on an empty array dies with a defect" do
      let
        prog :: F.RIO () () Int
        prog = F.raceAll []
      out <- runAff prog {}
      case out of
        Die _ -> pure unit
        other -> fail ("expected Die, got " <> describeOutcome other)

  describe "failCause" do
    it "failCause Cause.empty succeeds (no failure)" do
      -- A bind chain is used here rather than `*>` because the opMap
      -- fusion treats every FAIL_CAUSE as a failure for short-circuit
      -- purposes; `do`-binding routes through BIND, which does not
      -- short-circuit on a FAIL_CAUSE leaf.
      let
        prog :: F.RIO () () Int
        prog = do
          _ <- F.failCause Cause.empty :: F.RIO () () Unit
          pure 42
      out <- runAff prog {}
      assertSuccess out 42

    it "failCause with a Fail leaf surfaces the typed failure" do
      let
        prog :: F.RIO () (boom :: String) Int
        prog = F.failCause
          (Cause.fail (Variant.inj (Proxy :: _ "boom") "x"))
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "x"
        other -> fail ("expected Fail, got " <> describeOutcome other)

    it "failCause with a Die leaf surfaces as Die" do
      let
        prog :: F.RIO () () Int
        prog = F.failCause (Cause.die (error "kaboom"))
      out <- runAff prog {}
      case out of
        Die e -> message e `shouldEqual` "kaboom"
        other -> fail ("expected Die, got " <> describeOutcome other)

    it "failCause with a composed cause round-trips through causeOf" do
      let
        c :: Cause (boom :: String)
        c = Cause.both
          (Cause.fail (Variant.inj (Proxy :: _ "boom") "a"))
          (Cause.fail (Variant.inj (Proxy :: _ "boom") "b"))

        prog :: F.RIO () () (Either (Cause (boom :: String)) Int)
        prog = F.causeOf
          (F.failCause c :: F.RIO () (boom :: String) Int)
      out <- runAff prog {}
      case out of
        Success (Left captured) ->
          Array.length (Cause.failures captured) `shouldEqual` 2
        _ -> fail "expected Left with two failures"

  describe "uninterruptible" do
    it "shields the body from a pending interrupt; mask exit observes it" do
      events <- liftEffect (Ref.new ([] :: Array String))
      let
        record :: String -> F.RIO () () Unit
        record s = F.liftEffect
          (Ref.modify_ (\xs -> xs <> [ s ]) events)

        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork
            ( F.uninterruptible do
                record "enter"
                F.sleep (Milliseconds 30.0)
                record "exit"
            )
          F.sleep (Milliseconds 5.0)
          F.interrupt f
          _ <- F.causeOf (F.join f)
          F.sleep (Milliseconds 50.0)
          pure unit
      _ <- runAff prog {}
      seen <- liftEffect (Ref.read events)
      -- The body runs to completion despite the interrupt request.
      seen `shouldEqual` [ "enter", "exit" ]

    it "uninterruptible around acquire so a between-acquire-and-install interrupt cannot leak" do
      -- This is bracket's invariant. We mimic it manually here.
      acquired <- liftEffect (Ref.new false)
      released <- liftEffect (Ref.new false)
      let
        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork
            ( F.uninterruptible do
                F.liftEffect (Ref.write true acquired)
                F.sleep (Milliseconds 20.0)
            )
          F.sleep (Milliseconds 5.0)
          F.interrupt f
          _ <- F.causeOf (F.join f)
          -- After the masked region finishes, release should be safe
          -- to call in our test harness; we just verify acquire ran.
          F.liftEffect (Ref.write true released)
          pure unit
      _ <- runAff prog {}
      a <- liftEffect (Ref.read acquired)
      r <- liftEffect (Ref.read released)
      a `shouldEqual` true
      r `shouldEqual` true

  describe "uninterruptibleMask" do
    it "masks the body but lets a restored sub-action be interrupted" do
      events <- liftEffect (Ref.new ([] :: Array String))
      let
        record :: String -> F.RIO () () Unit
        record s = F.liftEffect
          (Ref.modify_ (\xs -> xs <> [ s ]) events)

        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork
            ( F.uninterruptibleMask \restore -> do
                record "enter"
                _ <- F.causeOf
                  ( restore do
                      record "use-start"
                      F.sleep (Milliseconds 30.0)
                      record "use-end"
                  )
                record "release"
            )
          F.sleep (Milliseconds 5.0)
          F.interrupt f
          _ <- F.causeOf (F.join f)
          F.sleep (Milliseconds 50.0)
          pure unit
      _ <- runAff prog {}
      seen <- liftEffect (Ref.read events)
      -- The "use" phase runs interruptibly: the interrupt fires there
      -- and skips "use-end". "enter" and "release" still both run.
      seen `shouldEqual` [ "enter", "use-start", "release" ]

    it "restore is a no-op when the surrounding context is already masked" do
      events <- liftEffect (Ref.new ([] :: Array String))
      let
        record :: String -> F.RIO () () Unit
        record s = F.liftEffect
          (Ref.modify_ (\xs -> xs <> [ s ]) events)

        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork
            ( F.uninterruptible
                ( F.uninterruptibleMask \restore -> do
                    record "enter"
                    restore do
                      record "use-start"
                      F.sleep (Milliseconds 30.0)
                      record "use-end"
                    record "release"
                )
            )
          F.sleep (Milliseconds 5.0)
          F.interrupt f
          _ <- F.causeOf (F.join f)
          pure unit
      _ <- runAff prog {}
      seen <- liftEffect (Ref.read events)
      -- Outer uninterruptible means restore is a no-op, so use-end runs.
      seen `shouldEqual` [ "enter", "use-start", "use-end", "release" ]

  describe "validatePar" do
    it "partitions a mix of successes and failures into a single cause" do
      let
        f :: Int -> F.RIO () (boom :: String) Int
        f n
          | n `mod` 2 == 0 = pure n
          | otherwise =
              F.fail
                ( Variant.inj (Proxy :: _ "boom")
                    ("odd-" <> show n)
                )

        prog :: F.RIO () () (Either (Cause (boom :: String)) (Array Int))
        prog = F.causeOf (F.validatePar f [ 1, 2, 3, 4, 5 ])
      out <- runAff prog {}
      case out of
        Success (Left captured) -> do
          Array.length (Cause.failures captured) `shouldEqual` 3
        Success (Right _) -> fail "expected Left (composed failures)"
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "succeeds with results in order when no branch fails" do
      let
        prog :: F.RIO () () (Array Int)
        prog = F.validatePar (\n -> pure (n * 10)) [ 1, 2, 3 ]
      out <- runAff prog {}
      assertSuccess out [ 10, 20, 30 ]

  describe "zipWithPar" do
    it "applies the combiner to the two parallel results" do
      let
        prog :: F.RIO () () Int
        prog = F.zipWithPar (+) (pure 1) (pure 41)
      out <- runAff prog {}
      assertSuccess out 42

    it "fails fast when either side fails" do
      let
        prog :: F.RIO () (boom :: String) Int
        prog = F.zipWithPar (+)
          (F.fail (Variant.inj (Proxy :: _ "boom") "nope"))
          (F.sleep (Milliseconds 30.0) *> pure 1)
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "nope"
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "runRIOCallback" do
    let
      -- Bridge `runRIOCallback`'s `Outcome e a` callback into Aff:
      -- capture the outcome, then complete the `makeAff` callback
      -- (`Either Error Unit`) with `Right unit`.
      runViaCallback
        :: forall e a
         . F.RIO () e a
        -> (Outcome e a -> Effect Unit)
        -> Aff Unit
      runViaCallback prog onOutcome = makeAff \cb -> do
        _ <- F.runRIOCallback prog {} \o -> do
          onOutcome o
          cb (Right unit)
        pure nonCanceler

      runViaCallbackCancelling
        :: forall e a
         . F.RIO () e a
        -> (Outcome e a -> Effect Unit)
        -> Aff Unit
      runViaCallbackCancelling prog onOutcome = makeAff \cb -> do
        canceller <- F.runRIOCallback prog {} \o -> do
          onOutcome o
          cb (Right unit)
        canceller
        pure nonCanceler
    it "delivers Success through the callback" do
      ref <- liftEffect (Ref.new Nothing :: Effect (Ref.Ref (Maybe (Outcome () Int))))
      runViaCallback (pure 99 :: F.RIO () () Int) \o ->
        Ref.write (Just o) ref
      result <- liftEffect (Ref.read ref)
      case result of
        Just (Success n) -> n `shouldEqual` 99
        _ -> fail "expected callback Success 99"

    it "delivers Fail through the callback" do
      let
        bad :: F.RIO () (boom :: String) Int
        bad = F.fail (Variant.inj (Proxy :: _ "boom") "x")
      ref <- liftEffect
        (Ref.new Nothing :: Effect (Ref.Ref (Maybe (Outcome (boom :: String) Int))))
      runViaCallback bad \o -> Ref.write (Just o) ref
      result <- liftEffect (Ref.read ref)
      case result of
        Just (Fail v) ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "x"
        _ -> fail "expected callback Fail"

    it "the returned canceller interrupts a running fiber" do
      cancelled <- liftEffect (Ref.new false)
      ref <- liftEffect
        (Ref.new Nothing :: Effect (Ref.Ref (Maybe (Outcome () Int))))
      let
        slow :: F.RIO () () Int
        slow = F.ensuring
          (F.liftEffect (Ref.write true cancelled))
          (F.sleep (Milliseconds 200.0) *> pure 0)
      runViaCallbackCancelling slow \o -> Ref.write (Just o) ref
      delay (Milliseconds 30.0)
      result <- liftEffect (Ref.read ref)
      case result of
        Just Interrupted -> do
          c <- liftEffect (Ref.read cancelled)
          c `shouldEqual` true
        _ -> fail "expected Interrupted"

  describe "async canceller" do
    it "runs the canceller when the fiber is interrupted while suspended" do
      cancellerRan <- liftEffect (Ref.new false)
      let
        body :: F.RIO () () Int
        body = F.async \_cb -> pure
          (Ref.write true cancellerRan)

        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork body
          F.sleep (Milliseconds 10.0)
          F.interrupt f
          _ <- F.causeOf (F.join f)
          pure unit
      _ <- runAff prog {}
      ran <- liftEffect (Ref.read cancellerRan)
      ran `shouldEqual` true

    it "does NOT run the canceller when the async resumes normally" do
      cancellerRan <- liftEffect (Ref.new false)
      let
        body :: F.RIO () () Int
        body = F.async \cb -> do
          scheduleResume (cb (Right 7))
          pure (Ref.write true cancellerRan)

        prog :: F.RIO () () Int
        prog = body
      out <- runAff prog {}
      assertSuccess out 7
      ran <- liftEffect (Ref.read cancellerRan)
      ran `shouldEqual` false

  describe "deeply nested" do
    it "1000-deep catchAll layers do not blow the stack" do
      let
        wrapLayer
          :: Int
          -> F.RIO () (boom :: String) Int
          -> F.RIO () (boom :: String) Int
        wrapLayer _ action =
          F.catchAll
            (\v -> F.fail v)
            action

        wrapped :: F.RIO () (boom :: String) Int
        wrapped = Array.foldl
          (\acc i -> wrapLayer i acc)
          (pure 42 :: F.RIO () (boom :: String) Int)
          (Array.range 1 1000)
      out <- runAff wrapped {}
      assertSuccess out 42

    it "1000-deep ensuring layers all fire in reverse" do
      counter <- liftEffect (Ref.new 0)
      let
        wrap
          :: Int
          -> F.RIO () () Int
          -> F.RIO () () Int
        wrap _ action = F.ensuring
          (F.liftEffect (Ref.modify_ (_ + 1) counter))
          action

        wrapped :: F.RIO () () Int
        wrapped = Array.foldl
          (\acc i -> wrap i acc)
          (pure 0 :: F.RIO () () Int)
          (Array.range 1 1000)
      out <- runAff wrapped {}
      assertSuccess out 0
      -- give the finalizers their turn before reading the counter
      _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
      n <- liftEffect (Ref.read counter)
      n `shouldEqual` 1000

  describe "bracket with interrupted use" do
    it "release runs when use is interrupted" do
      released <- liftEffect (Ref.new false)
      let
        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork
            ( F.bracket
                (pure 7)
                (\_ -> F.liftEffect (Ref.write true released))
                (\_ -> F.sleep (Milliseconds 200.0))
            )
          F.sleep (Milliseconds 10.0)
          F.interrupt f
          _ <- F.causeOf (F.join f)
          pure unit
      _ <- runAff prog {}
      delay (Milliseconds 5.0)
      r <- liftEffect (Ref.read released)
      r `shouldEqual` true

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

  describe "poll" do
    it "returns Nothing while the fiber is still running" do
      let
        prog :: F.RIO () () (Maybe (Outcome () Int))
        prog = do
          f <- F.fork (F.sleep (Milliseconds 50.0) *> pure 1 :: F.RIO () () Int)
          F.poll f
      out <- runAff prog {}
      case out of
        Success Nothing -> pure unit
        _ -> fail "expected Success Nothing while child is running"

    it "returns Just (Success a) once the fiber has completed" do
      let
        prog :: F.RIO () () (Maybe (Outcome () Int))
        prog = do
          f <- F.fork (pure 5 :: F.RIO () () Int)
          _ <- F.join f
          F.poll f
      out <- runAff prog {}
      case out of
        Success (Just (Success n)) -> n `shouldEqual` 5
        _ -> fail "expected Success (Just (Success 5))"

  describe "whenRIO / unlessRIO" do
    it "whenRIO runs the body when the condition action returns true" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Unit
        prog = F.whenRIO (pure true)
          (F.liftEffect (Ref.modify_ (_ + 1) ref))
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      n `shouldEqual` 1

    it "whenRIO skips the body when the condition returns false" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Unit
        prog = F.whenRIO (pure false)
          (F.liftEffect (Ref.modify_ (_ + 1) ref))
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      n `shouldEqual` 0

    it "unlessRIO is the dual of whenRIO" do
      ref <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Unit
        prog = do
          F.unlessRIO (pure false) (F.liftEffect (Ref.modify_ (_ + 1) ref))
          F.unlessRIO (pure true) (F.liftEffect (Ref.modify_ (_ + 10) ref))
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      n `shouldEqual` 1

  describe "iterate / loop" do
    it "iterate stops at the first state for which `cont` is false" do
      let
        prog :: F.RIO () () Int
        prog = F.iterate 0 (_ < 5) (\n -> pure (n + 1))
      out <- runAff prog {}
      assertSuccess out 5

    it "loop collects body results while `cont` holds" do
      let
        prog :: F.RIO () () (Array Int)
        prog = F.loop 0 (_ < 3) (_ + 1) (\s -> pure (s * 10))
      out <- runAff prog {}
      assertSuccess out [ 0, 10, 20 ]

  describe "never" do
    it "stays parked until interrupted; race against success picks the success" do
      let
        prog :: F.RIO () () Int
        prog = F.race F.never (F.sleep (Milliseconds 5.0) *> pure 7)
      out <- runAff prog {}
      assertSuccess out 7

    it "an external interrupt unwinds a parked `never`" do
      finalized <- liftEffect (Ref.new false)
      let
        parked :: F.RIO () () Unit
        parked = F.ensuring (F.liftEffect (Ref.write true finalized)) F.never

        prog :: F.RIO () () Unit
        prog = do
          handle <- F.fork parked
          F.sleep (Milliseconds 5.0)
          F.interrupt handle
      out <- runAff prog {}
      case out of
        Success _ -> pure unit
        other -> fail ("expected Success, got " <> describeOutcome other)
      fin <- liftEffect (Ref.read finalized)
      fin `shouldEqual` true

  describe "yieldNow" do
    it "is observable as a unit result and the fiber resumes" do
      let
        prog :: F.RIO () () Int
        prog = do
          F.yieldNow
          F.yieldNow
          pure 7
      out <- runAff prog {}
      assertSuccess out 7

    it "hands off to a queued sibling before resuming" do
      ref <- liftEffect (Ref.new ([] :: Array String))
      let
        push tag = F.liftEffect (Ref.modify_ (\xs -> xs <> [ tag ]) ref)

        sibling :: F.RIO () () Unit
        sibling = push "sibling"

        prog :: F.RIO () () Unit
        prog = do
          _ <- F.fork sibling
          push "before"
          F.yieldNow
          push "after"
      _ <- runAff prog {}
      trace <- liftEffect (Ref.read ref)
      -- the queued sibling must run between `before` and `after`,
      -- because yieldNow re-enqueues us behind everything already in
      -- the run queue.
      trace `shouldEqual` [ "before", "sibling", "after" ]

  describe "checkInterruptible" do
    it "is true at top level" do
      let
        prog :: F.RIO () () Boolean
        prog = F.checkInterruptible
      out <- runAff prog {}
      assertSuccess out true

    it "is false inside an uninterruptible region" do
      let
        prog :: F.RIO () () Boolean
        prog = F.uninterruptible F.checkInterruptible
      out <- runAff prog {}
      assertSuccess out false

    it "restore inside uninterruptibleMask flips it back to true" do
      let
        prog :: F.RIO () () (Tuple Boolean Boolean)
        prog = F.uninterruptibleMask \restore -> do
          outer <- F.checkInterruptible
          inner <- restore F.checkInterruptible
          pure (Tuple outer inner)
      out <- runAff prog {}
      assertSuccess out (Tuple false true)

  describe "firstSuccessOf" do
    it "returns the first action's result when it succeeds" do
      let
        prog :: F.RIO () () Int
        prog = F.firstSuccessOf [ pure 1, pure 2, pure 3 ]
      out <- runAff prog {}
      assertSuccess out 1

    it "falls back through failures to the first success" do
      let
        boom :: String -> F.RIO () (boom :: String) Int
        boom tag = F.fail (Variant.inj (Proxy :: _ "boom") tag)

        prog :: F.RIO () (boom :: String) Int
        prog = F.firstSuccessOf [ boom "a", boom "b", pure 99, boom "c" ]
      out <- runAff prog {}
      assertSuccess out 99

    it "propagates the last failure when every action fails" do
      let
        boom :: String -> F.RIO () (boom :: String) Int
        boom tag = F.fail (Variant.inj (Proxy :: _ "boom") tag)

        prog :: F.RIO () (boom :: String) Int
        prog = F.firstSuccessOf [ boom "a", boom "b", boom "c" ]
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "c"
        other -> fail ("expected Fail, got " <> describeOutcome other)

    it "raises a defect on an empty array" do
      let
        prog :: F.RIO () () Int
        prog = F.firstSuccessOf []
      out <- runAff prog {}
      case out of
        Die _ -> pure unit
        other -> fail ("expected Die, got " <> describeOutcome other)

    it "short-circuits: later actions are not started after a success" do
      ref <- liftEffect (Ref.new 0)
      let
        bump :: F.RIO () () Unit
        bump = F.liftEffect (Ref.modify_ (_ + 1) ref)

        prog :: F.RIO () () Int
        prog = F.firstSuccessOf
          [ bump *> pure 10
          , bump *> pure 20
          , bump *> pure 30
          ]
      out <- runAff prog {}
      assertSuccess out 10
      n <- liftEffect (Ref.read ref)
      n `shouldEqual` 1

  describe "partition" do
    it "splits mixed outcomes into failures and successes" do
      let
        f :: Int -> F.RIO () (boom :: String) Int
        f n =
          if n `mod` 2 == 0 then pure (n * 10)
          else F.fail (Variant.inj (Proxy :: _ "boom") ("odd:" <> show n))

        prog
          :: F.RIO () ()
               { failures :: Array (Variant.Variant (boom :: String))
               , successes :: Array Int
               }
        prog = F.partition f [ 1, 2, 3, 4, 5 ]
      out <- runAff prog {}
      case out of
        Success { failures, successes } -> do
          successes `shouldEqual` [ 20, 40 ]
          ( map (Variant.case_ # Variant.on (Proxy :: _ "boom") identity)
              failures
          ) `shouldEqual` [ "odd:1", "odd:3", "odd:5" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "runs all branches even when some fail (no fail-fast)" do
      counter <- liftEffect (Ref.new 0)
      let
        bumpAndFail :: Int -> F.RIO () (boom :: String) Int
        bumpAndFail n = do
          F.liftEffect (Ref.modify_ (_ + 1) counter)
          if n == 2 then F.fail (Variant.inj (Proxy :: _ "boom") "two")
          else pure n

        prog
          :: F.RIO () ()
               { failures :: Array (Variant.Variant (boom :: String))
               , successes :: Array Int
               }
        prog = F.partition bumpAndFail [ 1, 2, 3 ]
      _ <- runAff prog {}
      n <- liftEffect (Ref.read counter)
      n `shouldEqual` 3

    it "returns empty halves for an empty input" do
      let
        f :: Int -> F.RIO () (boom :: String) Int
        f n = pure n

        prog
          :: F.RIO () ()
               { failures :: Array (Variant.Variant (boom :: String))
               , successes :: Array Int
               }
        prog = F.partition f []
      out <- runAff prog {}
      case out of
        Success { failures, successes } -> do
          (Array.length failures) `shouldEqual` 0
          (Array.length successes) `shouldEqual` 0
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "forever" do
    it "loops the action until interrupted" do
      counter <- liftEffect (Ref.new 0)
      let
        prog :: F.RIO () () Unit
        prog = do
          f <- F.fork
            (F.forever (F.liftEffect (Ref.modify_ (_ + 1) counter)))
          F.sleep (Milliseconds 10.0)
          F.interrupt f
          _ <- F.join f
          pure unit
      _ <- runAff prog {}
      n <- liftEffect (Ref.read counter)
      (n > 0) `shouldEqual` true

    it "propagates a typed failure raised by the action" do
      let
        prog :: F.RIO () (boom :: String) Unit
        prog = F.forever
          (F.fail (Variant.inj (Proxy :: _ "boom") "stop"))
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "timed" do
    it "returns a non-negative duration and the action's value" do
      let
        prog :: F.RIO () () (Tuple Milliseconds Int)
        prog = F.timed (F.sleep (Milliseconds 5.0) *> pure 42)
      out <- runAff prog {}
      case out of
        Success (Tuple (Milliseconds ms) n) -> do
          n `shouldEqual` 42
          (ms >= 0.0) `shouldEqual` true
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "short-circuits on a typed failure (no result emitted)" do
      let
        prog :: F.RIO () (boom :: String) (Tuple Milliseconds Int)
        prog = F.timed
          (F.fail (Variant.inj (Proxy :: _ "boom") "x"))
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "filterOrFail" do
    it "passes through when the predicate holds" do
      let
        prog :: F.RIO () (bad :: String) Int
        prog = F.filterOrFail
          (_ > 0)
          (\n -> Variant.inj (Proxy :: _ "bad") ("nope:" <> show n))
          (pure 5)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 5
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "raises a typed failure when the predicate fails" do
      let
        prog :: F.RIO () (bad :: String) Int
        prog = F.filterOrFail
          (_ > 0)
          (\n -> Variant.inj (Proxy :: _ "bad") ("nope:" <> show n))
          (pure (-1))
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "bad") identity) v
            `shouldEqual` "nope:-1"
        other -> fail ("expected Fail, got " <> describeOutcome other)

  describe "filterOrDie" do
    it "passes through when the predicate holds" do
      let
        prog :: F.RIO () () Int
        prog = F.filterOrDie (_ > 0) (\_ -> error "nope") (pure 5)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 5
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "raises a defect when the predicate fails" do
      let
        prog :: F.RIO () () Int
        prog = F.filterOrDie (_ > 0)
          (\n -> error ("nope:" <> show n)) (pure (-1))
      out <- runAff prog {}
      case out of
        Die err -> message err `shouldEqual` "nope:-1"
        other -> fail ("expected Die, got " <> describeOutcome other)

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
