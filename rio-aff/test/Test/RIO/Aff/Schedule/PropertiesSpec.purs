module Test.RIO.Aff.Schedule.PropertiesSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, fail, provideAll, runRIO, runRIO')
import RIO.Aff.Schedule
  ( andThen
  , intersect
  , mapSchedule
  , recurs
  , repeat
  , retry
  , retryOrElse
  , whileInput
  )
import RIO.Aff.Test.Clock (newTestClock)

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

-- Map any `Int` into a small non-negative bound `[0, 20]`. Each
-- sample runs `n + 1` invocations through the runner, so a 50x
-- multiplier on top of 30 samples would dominate the test budget.
smallNat :: Gen Int
smallNat = (\k -> (if k < 0 then -k else k) `mod` 21) <$> arbitrary

spec :: Spec Unit
spec = describe "RIO.Aff.Schedule (property tests)" do
  it "recurs n paired with repeat runs the action exactly n+1 times" do
    -- Docstring promise: "`recurs 3` paired with `repeat` runs the
    -- action 4 times (one initial run plus 3 repeats)." The
    -- existing unit pin uses `recurs 3`; this property generalizes
    -- it to a range of `n` values including the `n = 0` boundary
    -- (one initial run, zero repeats) and small-`n` cases that
    -- are easy to off-by-one.
    forAll smallNat \n -> do
      counter <- liftEffect (Ref.new 0)
      tc <- newTestClock
      let
        action :: RIO () () Int
        action = liftEffect (Ref.modify (_ + 1) counter)

        program :: RIO (clock :: Clock) () Int
        program = repeat (recurs n) action

      result <- runRIO' (provideAll { clock: tc.clock } program)
      count <- liftEffect (Ref.read counter)
      result `shouldEqual` (n + 1)
      count `shouldEqual` (n + 1)

  it "intersect (recurs n) (recurs m) under repeat runs min n m + 1 times" do
    -- Docstring promise: `intersect` says `Done` as soon as either
    -- side does. Paired with `repeat`, that translates to running
    -- the action `min n m + 1` times: one initial run, then the
    -- smaller schedule exhausts first and stops the loop.
    forAll ((/\) <$> smallNat <*> smallNat :: Gen (Int /\ Int))
      \(n /\ m) -> do
        counter <- liftEffect (Ref.new 0)
        tc <- newTestClock
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program = repeat (intersect (recurs n) (recurs m)) action

        result <- runRIO' (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        let expected = min n m + 1
        result `shouldEqual` expected
        count `shouldEqual` expected

  it "mapSchedule preserves invocation count under repeat" do
    -- Docstring promise: "The cadence (number of steps and per-step
    -- delay) is preserved; only the output side changes." Under
    -- `repeat`, the schedule's output value is ignored (repeat
    -- returns the action's last value), so wrapping a schedule in
    -- `mapSchedule f` must not change how many times the action
    -- runs.
    forAll smallNat \n -> do
      counter <- liftEffect (Ref.new 0)
      tc <- newTestClock
      let
        action :: RIO () () Int
        action = liftEffect (Ref.modify (_ + 1) counter)

        program :: RIO (clock :: Clock) () Int
        program =
          repeat (mapSchedule (\k -> k * 100 + 1) (recurs n)) action

      result <- runRIO' (provideAll { clock: tc.clock } program)
      count <- liftEffect (Ref.read counter)
      result `shouldEqual` (n + 1)
      count `shouldEqual` (n + 1)

  it "retry (recurs n) on an always-failing action runs it n + 1 times" do
    -- Dual of `repeat (recurs n)`: `retry` re-runs the action on
    -- typed failures and the schedule sees each failure as input.
    -- For an action that always fails, the runner exhausts the
    -- schedule and surfaces the most recent failure. Total
    -- invocations match `repeat (recurs n)` on a successful
    -- action: `n + 1` (one initial + `n` retries).
    forAll smallNat \n -> do
      counter <- liftEffect (Ref.new 0)
      tc <- newTestClock
      let
        bumpThenFail :: RIO () (boom :: Unit) Int
        bumpThenFail = do
          _ <- liftEffect (Ref.modify (_ + 1) counter)
          fail (Proxy :: Proxy "boom") unit

        program :: RIO (clock :: Clock) (boom :: Unit) Int
        program = retry (recurs n) bumpThenFail

      result <- runRIO (provideAll { clock: tc.clock } program)
      count <- liftEffect (Ref.read counter)
      case result of
        Left v ->
          ( Variant.case_ # Variant.on (Proxy :: Proxy "boom") identity $
              v
          ) `shouldEqual` unit
        Right _ ->
          count `shouldEqual` (-1)
      count `shouldEqual` (n + 1)

  it "whileInput (const true) (recurs n) ≡ recurs n under repeat" do
    -- Docstring promise: `whileInput pred s` delegates to `s`'s
    -- decision whenever `pred` holds. A `const true` predicate is
    -- the identity case: total invocations must match plain
    -- `recurs n` (`n + 1` under repeat). A regression that, e.g.,
    -- decremented one continuation per `whileInput` wrap would
    -- surface as `n` invocations instead of `n + 1`.
    forAll smallNat \n -> do
      counter <- liftEffect (Ref.new 0)
      tc <- newTestClock
      let
        action :: RIO () () Int
        action = liftEffect (Ref.modify (_ + 1) counter)

        program :: RIO (clock :: Clock) () Int
        program =
          repeat (whileInput (\_ -> true) (recurs n)) action

      result <- runRIO' (provideAll { clock: tc.clock } program)
      count <- liftEffect (Ref.read counter)
      result `shouldEqual` (n + 1)
      count `shouldEqual` (n + 1)

  it "whileInput (const false) (recurs n) runs the action exactly once" do
    -- Dual to the `const true` case. The predicate gates every
    -- query, so the schedule says `Done` immediately on the
    -- first consultation. `repeat` runs the action once initially
    -- and then exits without recurring, regardless of `n`.
    forAll smallNat \n -> do
      counter <- liftEffect (Ref.new 0)
      tc <- newTestClock
      let
        action :: RIO () () Int
        action = liftEffect (Ref.modify (_ + 1) counter)

        program :: RIO (clock :: Clock) () Int
        program =
          repeat (whileInput (\_ -> false) (recurs n)) action

      result <- runRIO' (provideAll { clock: tc.clock } program)
      count <- liftEffect (Ref.read counter)
      result `shouldEqual` 1
      count `shouldEqual` 1

  it "retryOrElse on an always-succeeding action skips the fallback" do
    -- Docstring promise: "On exhaustion the fallback runs with
    -- the final failure." A successful action never reaches that
    -- branch, so the fallback must not fire and the action's
    -- value passes through unchanged. Pin across small schedule
    -- budgets so the contract is exercised at `n = 0`
    -- (no-retry-allowed) and small-`n` cases.
    forAll smallNat \n -> do
      counter <- liftEffect (Ref.new 0)
      tc <- newTestClock
      let
        action :: RIO () (boom :: Unit) Int
        action = liftEffect (Ref.modify (_ + 1) counter)

        fallback :: _ -> RIO () () Int
        fallback _ = liftEffect (Ref.modify (_ + 100) counter)

        program :: RIO (clock :: Clock) () Int
        program = retryOrElse (recurs n) action fallback

      result <- runRIO' (provideAll { clock: tc.clock } program)
      count <- liftEffect (Ref.read counter)
      -- The action runs exactly once (success on the first try)
      -- and the fallback (which would have added 100) never runs.
      result `shouldEqual` 1
      count `shouldEqual` 1

  it "andThen (recurs n) (recurs m) under repeat runs n + m + 1 times" do
    -- Docstring promise: `andThen sa sb` runs `sa` to completion
    -- then `sb`. `recurs n` returns `Continue` exactly `n` times
    -- and `andThen` consults `sb` in the same step where `sa`
    -- returns `Done` (the transition does not waste a step), so
    -- the combined schedule yields `n + m` total continuations
    -- and `repeat` runs the action `n + m + 1` times.
    forAll ((/\) <$> smallNat <*> smallNat :: Gen (Int /\ Int))
      \(n /\ m) -> do
        counter <- liftEffect (Ref.new 0)
        tc <- newTestClock
        let
          action :: RIO () () Int
          action = liftEffect (Ref.modify (_ + 1) counter)

          program :: RIO (clock :: Clock) () Int
          program = repeat (andThen (recurs n) (recurs m)) action

        result <- runRIO' (provideAll { clock: tc.clock } program)
        count <- liftEffect (Ref.read counter)
        let expected = n + m + 1
        result `shouldEqual` expected
        count `shouldEqual` expected
