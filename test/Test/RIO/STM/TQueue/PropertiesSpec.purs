module Test.RIO.STM.TQueue.PropertiesSpec (spec) where

import Prelude

import Data.Array (length) as Array
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
import RIO.STM.TQueue
  ( isEmptyTQueue
  , lengthTQueue
  , newTQueue
  , readTQueue
  , writeTQueue
  )

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.STM.TQueue (property tests)" do
  -- The unit pin fixes the input to `[1, 2, 3, 4]`. Generalize
  -- to arbitrary `Array Int` inputs so empty, singleton, and
  -- larger arrays are also covered.
  it "writeTQueue then readTQueue reproduces the input order" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        program :: RIO () () (Array Int)
        program = do
          q <- atomically newTQueue
          for_ xs \x -> atomically (writeTQueue q x)
          traverse (\_ -> atomically (readTQueue q)) xs
      received <- runRIO' program
      received `shouldEqual` xs

  it "lengthTQueue equals the count of writes so far" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        program :: RIO () () Int
        program = do
          q <- atomically newTQueue
          for_ xs \x -> atomically (writeTQueue q x)
          atomically (lengthTQueue q)
      r <- runRIO' program
      r `shouldEqual` Array.length xs

  it "isEmptyTQueue agrees with `length xs == 0`" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        program :: RIO () () Boolean
        program = do
          q <- atomically newTQueue
          for_ xs \x -> atomically (writeTQueue q x)
          atomically (isEmptyTQueue q)
      r <- runRIO' program
      r `shouldEqual` (Array.length xs == 0)
