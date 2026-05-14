module Test.RIO.Schedule.PropertiesSpec (spec) where

import Prelude

import Data.Foldable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Clock (Clock)
import RIO.Core (RIO, provideAll, runRIO')
import RIO.Schedule (recurs, repeat)
import RIO.Test.Clock (newTestClock)

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
spec = describe "RIO.Schedule (property tests)" do
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
