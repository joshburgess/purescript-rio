module Test.RIO.Aff.Hub.PropertiesSpec (spec) where

import Prelude

import Data.Array (length, range) as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Hub (make, publishAll, subscribe, subscriberCount)
import RIO.Aff.Queue (take)

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

-- Bound subscriber-count tests to a small range so the test
-- budget stays predictable; each sample allocates `n` queues.
smallNat :: Gen Int
smallNat = (\k -> (if k < 0 then -k else k) `mod` 11) <$> arbitrary

spec :: Spec Unit
spec = describe "RIO.Aff.Hub (property tests)" do
  -- The existing unit pin for `publishAll` fixes the input to
  -- `[1, 2, 3]`. Generalize across arbitrary `Array Int` inputs
  -- so a regression that only breaks on empty / singleton /
  -- length-2 batches (or batches longer than the unit pin) is
  -- still caught.
  it "publishAll delivers every value to every subscriber in input order" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        program :: RIO () () (Array (Maybe Int))
        program = do
          hub <- liftEffect make
          sub <- subscribe hub
          publishAll hub xs
          sequence (map (\_ -> take sub.queue) xs)
      received <- runRIO' program
      Array.length received `shouldEqual` Array.length xs
      received `shouldEqual` map Just xs

  it "subscriberCount equals the number of live subscribers" do
    -- The hub does not have a dedicated unit pin for
    -- `subscriberCount` (only its behaviour under unsubscribe is
    -- pinned indirectly). Pin the linear contract: after `n`
    -- subscribes the count is `n`; after each subscriber
    -- unsubscribes the count decrements by one, terminating at
    -- zero. Generalize across small `n` so off-by-one regressions
    -- at the empty / singleton boundaries are caught.
    forAll smallNat \n -> do
      hub <- liftEffect (make :: _ (_ Int))
      let
        -- `Array.range 1 0` returns `[1]` (it's an inclusive
        -- range with at-least-one element). Use an explicit empty
        -- list for `n == 0` so the subscribe traversal stays
        -- faithful to the requested count.
        ixs = if n <= 0 then [] else Array.range 1 n
      subs <- runRIO'
        (traverse (\_ -> subscribe hub) ixs :: RIO () () _)
      afterSubscribe <- liftEffect (subscriberCount hub)
      afterSubscribe `shouldEqual` Array.length subs
      for_ subs \sub -> runRIO' sub.unsubscribe
      afterUnsubscribe <- liftEffect (subscriberCount hub)
      afterUnsubscribe `shouldEqual` 0
