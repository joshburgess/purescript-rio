module Test.RIO.ScheduleSpec (spec) where

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

import RIO.Clock (Clock)
import RIO.Core (RIO, die, fail, provideAll, runRIO, runRIO')
import RIO.Schedule
  ( Schedule
  , Step(..)
  , andThen
  , exponential
  , forever
  , intersect
  , jittered
  , mapSchedule
  , once
  , recurs
  , repeat
  , retry
  , retryOrElse
  , spaced
  , step
  , whileInput
  )
import RIO.Test.Clock (newTestClock)

spec :: Spec Unit
spec = do
  describe "RIO.Schedule" do
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
