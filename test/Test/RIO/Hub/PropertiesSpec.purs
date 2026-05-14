module Test.RIO.Hub.PropertiesSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Hub (make, publishAll, subscribe)
import RIO.Queue (take)

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Hub (property tests)" do
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
