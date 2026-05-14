module Test.RIO.STM.THub.PropertiesSpec (spec) where

import Prelude

import Data.Foldable (for_)
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.STM (atomically)
import RIO.STM.THub
  ( newUnboundedTHub
  , publishTHub
  , subscribeTHub
  , takeSubscription
  )

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.STM.THub (property tests)" do
  -- The unit pin fixes `[1, 2, 3]`. Generalize so empty,
  -- singleton, and larger publish batches are covered.
  it "unbounded THub delivers every value to every subscriber in publish order" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        program :: RIO () () { a :: Array Int, b :: Array Int }
        program = do
          hub <- atomically newUnboundedTHub
          sa <- atomically (subscribeTHub hub)
          sb <- atomically (subscribeTHub hub)
          for_ xs \x -> do
            _ <- atomically (publishTHub hub x)
            pure unit
          a <- traverse (\_ -> atomically (takeSubscription sa)) xs
          b <- traverse (\_ -> atomically (takeSubscription sb)) xs
          pure { a, b }
      r <- runRIO' program
      r.a `shouldEqual` xs
      r.b `shouldEqual` xs
