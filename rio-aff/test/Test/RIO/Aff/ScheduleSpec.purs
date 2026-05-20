module Test.RIO.Aff.ScheduleSpec (spec) where

import Prelude

import Data.Array (all, snoc) as Array
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Aff (attempt, error)
import Effect.Aff (delay, forkAff) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, die, fail, provideAll, runRIO, runRIO')
import RIO.Aff.Schedule
  ( Schedule
  , Step(..)
  , andThen
  , exponential
  , fibonacci
  , forever
  , intersect
  , jittered
  , mapSchedule
  , once
  , recurs
  , recursUntil
  , recursWhile
  , repeat
  , retry
  , retryOrElse
  , spaced
  , step
  , untilInput
  , untilOutput
  , whileInput
  , whileOutput
  )
import RIO.Aff.Test.Clock (newTestClock)

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Schedule" do
    describe "repeat" do
      it "recurs n runs the action n+1 times" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program = repeat (recurs 3) action

        tc <- newTestClock
        result <- runRIO' (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        result `shouldEqual` 4
        count `shouldEqual` 4

      it "stops on the first typed failure" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () (boom :: Unit) Int
          action = do
            n <- liftEffect (Ref.modify (_ + 1) counter)
            if n >= 2 then fail (Proxy :: Proxy "boom") unit
            else pure n

          program :: RIO (clock :: Clock) (boom :: Unit) Int
          program = repeat (recurs 5) action

        tc <- newTestClock
        result <- runRIO (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        count `shouldEqual` 2

      it "feeds each successful action result into the schedule as input" do
        -- Docstring promise: "The schedule sees each successful
        -- result as input; while it says `Continue`, the runner
        -- sleeps the requested delay and runs the action again."
        -- Every existing `repeat` test pairs it with `recurs N`
        -- (an input-blind schedule), so the input-threading half
        -- of the contract is unexercised: a regression that
        -- forwarded `unit` (or any constant) to the schedule
        -- instead of the action's last result would still pass
        -- every other test. Pin the threading by combining
        -- `repeat` with `whileInput`, which IS input-sensitive:
        -- run an action that returns the call-count and stop
        -- under `whileInput (_ < 3)`. The schedule must observe
        -- the action's 1, 2, 3 sequence and halt the third time
        -- around. Both the `result` (the last successful value)
        -- and the counter must read 3.
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program =
            repeat (whileInput (\n -> n < 3) (recurs 100)) action

        tc <- newTestClock
        result <- runRIO' (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        result `shouldEqual` 3
        count `shouldEqual` 3

    describe "retry" do
      it "recovers after a transient failure" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () (boom :: Unit) Int
          action = do
            n <- liftEffect (Ref.modify (_ + 1) counter)
            if n < 3 then fail (Proxy :: Proxy "boom") unit
            else pure n

          program :: RIO (clock :: Clock) (boom :: Unit) Int
          program = retry (recurs 5) action

        tc <- newTestClock
        result <- runRIO (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        result `shouldEqual` (Right 3 :: Either _ Int)
        count `shouldEqual` 3

      it "surfaces the final failure once retries are exhausted" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () (boom :: Unit) Int
          action = do
            _ <- liftEffect (Ref.modify (_ + 1) counter)
            fail (Proxy :: Proxy "boom") unit

          program :: RIO (clock :: Clock) (boom :: Unit) Int
          program = retry (recurs 2) action

        tc <- newTestClock
        result <- runRIO (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0
        count `shouldEqual` 3

      it "surfaces the MOST RECENT failure (not the first) once retries are exhausted" do
        -- Docstring promise: "When the schedule says `Done`,
        -- surface the most recent failure on the parent's row."
        -- The existing "surfaces the final failure once retries
        -- are exhausted" test fires `fail (Proxy :: Proxy "boom")
        -- unit` on every attempt, so every failure variant is
        -- observationally identical. A regression that kept the
        -- FIRST failure variant in scope and surfaced it after
        -- exhaustion (e.g., bound `v0` from the first iteration
        -- and returned `Left v0` instead of the loop's current
        -- `Left v`) would still pass that test. Pin the "most
        -- recent" half by having each attempt carry the attempt
        -- count in its variant payload; with `recurs 1` (one
        -- retry allowed, two attempts total), the surfaced
        -- payload must be 200, not 100.
        attempts <- liftEffect (Ref.new 0)
        let
          action :: RIO () (code :: Int) Int
          action = do
            n <- liftEffect (Ref.modify (_ + 1) attempts)
            fail (Proxy :: Proxy "code") (n * 100)

          program :: RIO (clock :: Clock) (code :: Int) Int
          program = retry (recurs 1) action

        tc <- newTestClock
        result <- runRIO (provideAll { clock: tc.clock } program)
        callsMade <- liftEffect (Ref.read attempts)
        callsMade `shouldEqual` 2
        case result of
          Left v ->
            (Variant.case_ # Variant.on (Proxy :: Proxy "code") identity $ v)
              `shouldEqual` 200
          Right _ -> 1 `shouldEqual` 0

      it "a defect skips retry and propagates immediately" do
        -- Docstring promise: "Defects (from `die` or any uncaught
        -- `Aff` exception) skip retry and propagate immediately;
        -- sandbox the action if you want a defect to feed back
        -- into the schedule." Pin it: an action that dies on the
        -- first call would, under a typed-failure-style retry,
        -- run up to `recurs 5 + 1 = 6` times; assert it runs
        -- exactly once and the defect surfaces through `attempt`.
        attempts <- liftEffect (Ref.new 0)
        let
          action :: RIO () () Int
          action = do
            _ <- liftEffect (Ref.modify (_ + 1) attempts)
            die (error "kaboom")

          program :: RIO (clock :: Clock) () Int
          program = retry (recurs 5) action

        tc <- newTestClock
        outcome <- attempt
          (runRIO' (provideAll { clock: tc.clock } program))
        callsMade <- liftEffect (Ref.read attempts)
        callsMade `shouldEqual` 1
        case outcome of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

    describe "retryOrElse" do
      it "runs the fallback when retries are exhausted" do
        let
          action :: RIO () (boom :: Unit) Int
          action = fail (Proxy :: Proxy "boom") unit

          fallback :: Variant.Variant (boom :: Unit) -> RIO () () Int
          fallback _ = pure 99

          program :: RIO (clock :: Clock) () Int
          program = retryOrElse (recurs 1) action fallback

        tc <- newTestClock
        result <- runRIO' (provideAll { clock: tc.clock } program)
        result `shouldEqual` 99

      it "runs the fallback immediately when the schedule's first step is Done" do
        -- Docstring promise: "If the schedule's first step is Done
        -- (no retry allowed), the fallback runs immediately on the
        -- first failure." Pin this with `recurs 0` and an action
        -- whose call count tells us how many times the action ran
        -- before the fallback was invoked.
        attempts <- liftEffect (Ref.new 0)
        let
          action :: RIO () (boom :: Unit) Int
          action = do
            _ <- liftEffect (Ref.modify (_ + 1) attempts)
            fail (Proxy :: Proxy "boom") unit

          fallback :: Variant.Variant (boom :: Unit) -> RIO () () Int
          fallback _ = pure 42

          program :: RIO (clock :: Clock) () Int
          program = retryOrElse (recurs 0) action fallback

        tc <- newTestClock
        result <- runRIO' (provideAll { clock: tc.clock } program)
        result `shouldEqual` 42
        callsMade <- liftEffect (Ref.read attempts)
        callsMade `shouldEqual` 1

      it "fallback receives the final failure's variant payload" do
        -- Docstring promise: "the fallback runs with the final
        -- failure." Both pinned `retryOrElse` tests use
        -- `fallback _ = pure X` and discard the variant, so the
        -- "with the final failure" half is unpinned: a
        -- regression that fed a stale, default, or
        -- first-attempt variant to the fallback would still pass
        -- them. Have the action carry an attempt counter inside
        -- the variant payload (attempt n → payload n * 100) and
        -- have the fallback return that payload via
        -- `Variant.on`. With `recurs 1` (one retry allowed, so
        -- the action runs twice before exhaustion), the
        -- fallback must observe `200`, not `100`, proving it
        -- received the actual final-failure variant.
        attempts <- liftEffect (Ref.new 0)
        let
          action :: RIO () (code :: Int) Int
          action = do
            n <- liftEffect (Ref.modify (_ + 1) attempts)
            fail (Proxy :: Proxy "code") (n * 100)

          fallback :: Variant.Variant (code :: Int) -> RIO () () Int
          fallback v =
            pure (Variant.case_ # Variant.on (Proxy :: Proxy "code") identity $ v)

          program :: RIO (clock :: Clock) () Int
          program = retryOrElse (recurs 1) action fallback

        tc <- newTestClock
        result <- runRIO' (provideAll { clock: tc.clock } program)
        callsMade <- liftEffect (Ref.read attempts)
        callsMade `shouldEqual` 2
        result `shouldEqual` 200

    describe "intersect" do
      it "stops as soon as either schedule stops" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program =
            repeat
              (intersect (recurs 2) (recurs 10))
              action

        tc <- newTestClock
        _ <- runRIO' (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        count `shouldEqual` 3

      it "emits the tuple of per-schedule outputs at each step" do
        -- The docstring promises that the output is the tuple of
        -- per-schedule outputs. The existing test only pins the
        -- "stops as soon as either schedule stops" contract; pin
        -- the output shape directly so any silent change to the
        -- combining function is caught.
        let
          sched =
            intersect (recurs 3) (recurs 3)
              :: Schedule () Unit (Tuple Int Int)
        outputs <- runRIO' (collectOutputs 5 sched)
        outputs `shouldEqual`
          [ Tuple 1 1, Tuple 2 2, Tuple 3 3 ]

      it "uses the larger of the two delays at each step" do
        -- The docstring promises that the delay is the larger
        -- of the two so both schedules can keep up. Pair a fast
        -- 50ms schedule against a slower 200ms one and assert
        -- each emitted delay is 200ms, not 50ms or some other
        -- combination.
        let
          sched =
            intersect (spaced (Milliseconds 50.0)) (spaced (Milliseconds 200.0))
              :: Schedule () Unit (Tuple Int Int)
        delays <- runRIO' (collectDelays 3 sched)
        delays `shouldEqual`
          [ Milliseconds 200.0, Milliseconds 200.0, Milliseconds 200.0 ]

    describe "exponential under the test clock" do
      it "drives one step per matching advance" do
        counter <- liftEffect (Ref.new 0)
        tc <- newTestClock
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program =
            repeat
              (intersect (recurs 3) (exponential (Milliseconds 100.0) 2.0))
              action

        _ <- Aff.forkAff
          (runRIO' (provideAll { clock: tc.clock } program))

        Aff.delay (Milliseconds 0.0)
        c0 <- liftEffect (Ref.read counter)
        c0 `shouldEqual` 1

        tc.advance (Milliseconds 100.0)
        Aff.delay (Milliseconds 0.0)
        c1 <- liftEffect (Ref.read counter)
        c1 `shouldEqual` 2

        tc.advance (Milliseconds 200.0)
        Aff.delay (Milliseconds 0.0)
        c2 <- liftEffect (Ref.read counter)
        c2 `shouldEqual` 3

        tc.advance (Milliseconds 400.0)
        Aff.delay (Milliseconds 0.0)
        c3 <- liftEffect (Ref.read counter)
        c3 `shouldEqual` 4

    describe "exponential direct delays" do
      it "emits base, base*factor, base*factor^2, ... at each step" do
        -- The "exponential under the test clock" test pins step-firing
        -- cadence indirectly via the counter; this pins the emitted
        -- delay values themselves so any change to the growth formula
        -- is caught directly.
        let
          sched =
            exponential (Milliseconds 100.0) 2.0
              :: Schedule () Unit Milliseconds
        delays <- runRIO' (collectDelays 4 sched)
        delays `shouldEqual`
          [ Milliseconds 100.0
          , Milliseconds 200.0
          , Milliseconds 400.0
          , Milliseconds 800.0
          ]

    describe "jittered" do
      it "keeps sampled delays inside the band [lo*base, hi*base]" do
        delays <- runRIO' (collectDelays 100 (jittered 0.8 1.2 (spaced (Milliseconds 100.0))))
        let
          inBand ms =
            let
              n = un Milliseconds ms
            in
              n >= 80.0 && n <= 120.0
        delays `shouldSatisfy` Array.all inBand

    describe "spaced" do
      -- The other tests use `spaced` indirectly (as the inner schedule
      -- for `jittered`, or via `forever = spaced 0.0`). These two pin
      -- the bare docstring promise that `spaced ms` is a fixed delay
      -- between firings, forever, with the output as an iteration
      -- counter starting at 1.
      it "emits the supplied delay verbatim at every step" do
        let sched = spaced (Milliseconds 100.0) :: Schedule () Unit Int
        delays <- runRIO' (collectDelays 5 sched)
        delays `shouldEqual`
          [ Milliseconds 100.0
          , Milliseconds 100.0
          , Milliseconds 100.0
          , Milliseconds 100.0
          , Milliseconds 100.0
          ]

      it "outputs an increasing iteration count starting at 1" do
        let sched = spaced (Milliseconds 50.0) :: Schedule () Unit Int
        outputs <- runRIO' (collectOutputs 4 sched)
        outputs `shouldEqual` [ 1, 2, 3, 4 ]

    describe "once" do
      it "runs the action twice under repeat" do
        counter <- liftEffect (Ref.new 0)
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program = repeat once action

        tc <- newTestClock
        result <- runRIO' (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        result `shouldEqual` 2
        count `shouldEqual` 2

    describe "forever" do
      it "never returns Done; emits an increasing iteration count" do
        outputs <- runRIO' (collectOutputs 5 (forever :: Schedule () Unit Int))
        outputs `shouldEqual` [ 1, 2, 3, 4, 5 ]

      it "emits Milliseconds 0.0 at every step (equivalent to spaced 0)" do
        -- Docstring promise: `forever` is "Equivalent to
        -- `spaced (Milliseconds 0.0)`". The existing `forever`
        -- test pins only the iteration-count output side; a
        -- regression that silently introduced any nonzero
        -- delay (e.g., copy-pasted from `spaced 100`) would
        -- still pass that test because `collectOutputs` does
        -- not look at delays. Pin the zero-delay half of the
        -- equivalence directly.
        delays <- runRIO' (collectDelays 4 (forever :: Schedule () Unit Int))
        delays `shouldEqual`
          [ Milliseconds 0.0
          , Milliseconds 0.0
          , Milliseconds 0.0
          , Milliseconds 0.0
          ]

    describe "mapSchedule" do
      it "transforms output while preserving cadence" do
        let sched = mapSchedule (\n -> n * 10) (recurs 3) :: Schedule () Unit Int
        outputs <- runRIO' (collectOutputs 5 sched)
        outputs `shouldEqual` [ 10, 20, 30 ]

      it "preserves the per-step delay of the underlying schedule" do
        -- Docstring promise: "The cadence (number of steps and
        -- per-step delay) is preserved; only the output side
        -- changes." The "transforms output" test above uses
        -- `recurs 3` whose delay is `Milliseconds 0.0`, so a
        -- regression that silently zeroed the delay inside
        -- `mapSchedule` would still satisfy that test. Pin the
        -- per-step-delay half of the promise by mapping over
        -- `spaced (Milliseconds 100.0)` and asserting the
        -- collected delays are unchanged.
        let
          sched =
            mapSchedule (\n -> n * 10) (spaced (Milliseconds 100.0))
              :: Schedule () Unit Int
        delays <- runRIO' (collectDelays 3 sched)
        delays `shouldEqual`
          [ Milliseconds 100.0, Milliseconds 100.0, Milliseconds 100.0 ]

    describe "andThen" do
      it "emits Left outputs from the first schedule, then Right from the second" do
        let
          sched =
            andThen (recurs 2) (recurs 2)
              :: Schedule () Unit (Either Int Int)
        outputs <- runRIO' (collectOutputs 10 sched)
        outputs `shouldEqual` [ Left 1, Left 2, Right 1, Right 2 ]

      it "preserves each phase's per-step delay across the sa->sb transition" do
        -- The docstring example shows "3 fast retries, then back
        -- off forever" via `andThen (recurs 3) (exponential
        -- ...)`. That phrasing implies each phase's cadence is
        -- preserved across the transition, but the only existing
        -- `andThen` test uses two `recurs N` schedules whose
        -- delays are both `Milliseconds 0.0`, so a regression
        -- that zeroed all delays (or stuck on `sa`'s delay even
        -- after the transition) would still pass it. Pin the
        -- per-phase delay-preservation half by stitching a
        -- finite 50ms phase (`intersect (recurs 2) (spaced 50)`,
        -- which bounds at 2 steps and uses 50ms delay) to an
        -- infinite 200ms phase (`spaced 200`). The collected
        -- delays must reflect each phase's cadence: 50, 50, 200,
        -- 200.
        let
          sa :: Schedule () Unit (Tuple Int Int)
          sa = intersect (recurs 2) (spaced (Milliseconds 50.0))

          sb :: Schedule () Unit Int
          sb = spaced (Milliseconds 200.0)

          sched :: Schedule () Unit (Either (Tuple Int Int) Int)
          sched = andThen sa sb
        delays <- runRIO' (collectDelays 4 sched)
        delays `shouldEqual`
          [ Milliseconds 50.0
          , Milliseconds 50.0
          , Milliseconds 200.0
          , Milliseconds 200.0
          ]

    describe "whileInput" do
      it "stops immediately when the predicate is false" do
        let
          sched =
            whileInput (\(n :: Int) -> n < 5) (recurs 10)
              :: Schedule () Int Int
        out0 <- runRIO' (step sched 99)
        case out0 of
          Done -> pure unit
          Continue _ _ _ -> 1 `shouldEqual` 0

      it "delegates to the inner schedule when the predicate holds" do
        let
          sched =
            whileInput (\(n :: Int) -> n < 5) (recurs 3)
              :: Schedule () Int Int
        out0 <- runRIO' (step sched 0)
        case out0 of
          Continue o _ _ -> o `shouldEqual` 1
          Done -> 1 `shouldEqual` 0

      it "re-checks the predicate on every step (not just at construction)" do
        -- The `whileInput` docstring promises that the
        -- "underlying schedule's decision is consulted only
        -- when the predicate holds; if it doesn't, the result
        -- is `Done` immediately." The existing single-step
        -- tests pin the two endpoint cases (predicate-true on
        -- step 0, predicate-false on step 0) but not the
        -- transition: a multi-step run where the predicate
        -- holds for several inputs and then fails. A
        -- regression that snapshotted the predicate result at
        -- construction (or only consulted it on step 0) would
        -- pass the existing tests but fail to stop a run that
        -- becomes invalid mid-stream. Pin the per-step
        -- re-check by stepping the schedule across three
        -- inputs `[1, 2, 3]` against `(\n -> n < 3)`: the
        -- first two must Continue (delegating to `recurs 10`
        -- with outputs 1 and 2), and the third must Done
        -- without consulting the inner schedule.
        let
          sched =
            whileInput (\(n :: Int) -> n < 3) (recurs 10)
              :: Schedule () Int Int
        out1 <- runRIO' (step sched 1)
        case out1 of
          Continue o1 _ next1 -> do
            o1 `shouldEqual` 1
            out2 <- runRIO' (step next1 2)
            case out2 of
              Continue o2 _ next2 -> do
                o2 `shouldEqual` 2
                out3 <- runRIO' (step next2 3)
                case out3 of
                  Done -> pure unit
                  Continue _ _ _ -> 1 `shouldEqual` 0
              Done -> 1 `shouldEqual` 0
          Done -> 1 `shouldEqual` 0

    describe "fibonacci" do
      it "emits delays following the Fibonacci sequence starting from base, base" do
        let
          sched = fibonacci (Milliseconds 100.0) :: Schedule () Unit Milliseconds
        delays <- runRIO' (collectDelays 7 sched)
        delays `shouldEqual`
          [ Milliseconds 100.0
          , Milliseconds 200.0
          , Milliseconds 300.0
          , Milliseconds 500.0
          , Milliseconds 800.0
          , Milliseconds 1300.0
          , Milliseconds 2100.0
          ]

      it "emits its current delay as the per-step output (like exponential)" do
        let
          sched = fibonacci (Milliseconds 100.0) :: Schedule () Unit Milliseconds
        outputs <- runRIO' (collectOutputs 4 sched)
        outputs `shouldEqual`
          [ Milliseconds 100.0
          , Milliseconds 200.0
          , Milliseconds 300.0
          , Milliseconds 500.0
          ]

    describe "untilInput" do
      it "stops the moment the predicate holds" do
        let
          sched =
            untilInput (\(n :: Int) -> n >= 5) (recurs 10)
              :: Schedule () Int Int
        out0 <- runRIO' (step sched 99)
        case out0 of
          Done -> pure unit
          Continue _ _ _ -> 1 `shouldEqual` 0

      it "delegates to the inner schedule until the predicate holds" do
        let
          sched =
            untilInput (\(n :: Int) -> n >= 3) (recurs 10)
              :: Schedule () Int Int
        out1 <- runRIO' (step sched 1)
        case out1 of
          Continue _ _ next1 -> do
            out2 <- runRIO' (step next1 2)
            case out2 of
              Continue _ _ next2 -> do
                out3 <- runRIO' (step next2 3)
                case out3 of
                  Done -> pure unit
                  Continue _ _ _ -> 1 `shouldEqual` 0
              Done -> 1 `shouldEqual` 0
          Done -> 1 `shouldEqual` 0

    describe "whileOutput" do
      it "continues only while the inner schedule's output matches the predicate" do
        let
          sched =
            whileOutput (\(n :: Int) -> n < 3) (recurs 10)
              :: Schedule () Unit Int
        outputs <- runRIO' (collectOutputs 10 sched)
        outputs `shouldEqual` [ 1, 2 ]

    describe "untilOutput" do
      it "stops the moment the inner schedule's output matches the predicate" do
        let
          sched =
            untilOutput (\(n :: Int) -> n >= 3) (recurs 10)
              :: Schedule () Unit Int
        outputs <- runRIO' (collectOutputs 10 sched)
        outputs `shouldEqual` [ 1, 2 ]

    describe "recursWhile / recursUntil" do
      it "recursWhile is forever filtered by the input predicate" do
        let
          sched =
            recursWhile (\(n :: Int) -> n < 3) :: Schedule () Int Int
        out1 <- runRIO' (step sched 1)
        case out1 of
          Continue o1 _ next1 -> do
            o1 `shouldEqual` 1
            out2 <- runRIO' (step next1 2)
            case out2 of
              Continue o2 _ next2 -> do
                o2 `shouldEqual` 2
                out3 <- runRIO' (step next2 3)
                case out3 of
                  Done -> pure unit
                  Continue _ _ _ -> 1 `shouldEqual` 0
              Done -> 1 `shouldEqual` 0
          Done -> 1 `shouldEqual` 0

      it "recursUntil is forever stopped by the input predicate" do
        let
          sched =
            recursUntil (\(n :: Int) -> n >= 3) :: Schedule () Int Int
        out1 <- runRIO' (step sched 1)
        case out1 of
          Continue _ _ next1 -> do
            out2 <- runRIO' (step next1 2)
            case out2 of
              Continue _ _ next2 -> do
                out3 <- runRIO' (step next2 3)
                case out3 of
                  Done -> pure unit
                  Continue _ _ _ -> 1 `shouldEqual` 0
              Done -> 1 `shouldEqual` 0
          Done -> 1 `shouldEqual` 0

collectOutputs
  :: forall o
   . Int
  -> Schedule () Unit o
  -> RIO () () (Array o)
collectOutputs n0 sched0 = go n0 sched0 []
  where
  go k s acc
    | k <= 0 = pure acc
    | otherwise = do
        out <- step s unit
        case out of
          Done -> pure acc
          Continue o _ next -> go (k - 1) next (Array.snoc acc o)

collectDelays
  :: forall o
   . Int
  -> Schedule () Unit o
  -> RIO () () (Array Milliseconds)
collectDelays n0 sched0 = go n0 sched0 []
  where
  go k s acc
    | k <= 0 = pure acc
    | otherwise = do
        out <- step s unit
        case out of
          Done -> pure acc
          Continue _ ms next -> go (k - 1) next (Array.snoc acc ms)
