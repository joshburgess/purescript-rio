module Test.RIO.Random.PropertiesSpec (spec) where

import Prelude

import Data.Array (range) as Array
import Data.Foldable (all, for_)
import Data.Int (toNumber) as Int
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, provideAll, runRIO')
import RIO.Random (Random, nextInt, nextRange)
import RIO.Test.Random (newTestRandom)

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 20 gen)
  for_ samples prop

-- Map any `Int` into a small non-negative range. We pair these to
-- form `low <= high` intervals across a generous span.
smallInt :: Gen Int
smallInt = (\k -> (if k < 0 then -k else k) `mod` 100) <$> arbitrary

spec :: Spec Unit
spec = describe "RIO.Random (property tests)" do
  -- The unit pin fixes `nextInt 1 6` and `nextRange 2.5 5.5`. A
  -- regression that, for example, swapped `low` and `high` inside
  -- the LCG mapping would still satisfy those specific intervals.
  -- Generalize across arbitrary low/high pairs so the contract is
  -- exercised across a span of intervals, not just one.

  it "nextInt low high stays in [low, high] across arbitrary ranges" do
    forAll ({ a: _, b: _ } <$> smallInt <*> smallInt) \{ a, b } -> do
      let
        low = min a b
        high = max a b
      tr <- newTestRandom (low + high + 1)
      let
        batch :: RIO (random :: Random) () (Array Int)
        batch = traverse (\_ -> nextInt low high) (Array.range 1 30)
      xs <- runRIO' (provideAll { random: tr.random } batch)
      all (\n -> n >= low && n <= high) xs `shouldEqual` true

  it "nextRange min max stays in [min, max) across arbitrary intervals" do
    forAll ({ a: _, b: _ } <$> smallInt <*> smallInt) \{ a, b } -> do
      let
        lo = min a b
        hi = max a b
      -- Skip degenerate intervals: when `lo == hi`, the half-open
      -- contract `[lo, hi)` is empty, so no draw can satisfy it.
      -- `nextRange` is documented as undefined for `max < min` and
      -- by convention also for `max == min`, so we only assert the
      -- contract when the interval has positive width.
      if lo == hi then pure unit
      else do
        let
          loN = Int.toNumber lo
          hiN = Int.toNumber hi
        tr <- newTestRandom (lo + hi + 1)
        let
          batch :: RIO (random :: Random) () (Array Number)
          batch = traverse (\_ -> nextRange loN hiN) (Array.range 1 30)
        xs <- runRIO' (provideAll { random: tr.random } batch)
        all (\n -> n >= loN && n < hiN) xs `shouldEqual` true
