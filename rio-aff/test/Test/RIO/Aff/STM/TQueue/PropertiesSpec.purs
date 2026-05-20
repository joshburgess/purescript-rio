module Test.RIO.Aff.STM.TQueue.PropertiesSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.STM (atomically)
import RIO.Aff.STM.TQueue
  ( isEmptyTQueue
  , lengthTQueue
  , newTQueue
  , readTQueue
  , tryReadTQueue
  , writeTQueue
  )

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Aff.STM.TQueue (property tests)" do
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

  it "tryReadTQueue on a drained queue returns Nothing" do
    -- After writing `xs` and then `tryReadTQueue`-ing exactly
    -- `Array.length xs` times, the queue must be empty and the
    -- next `tryReadTQueue` must report `Nothing`. The unit pin
    -- for `tryReadTQueue` covers the non-empty case; pin the
    -- drain-to-empty boundary across arbitrary input batches so
    -- a regression that left a sentinel after the final read
    -- (or one that mis-counted on `lengthTQueue`-based drains)
    -- is caught.
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        program :: RIO () () (Maybe Int)
        program = do
          q <- atomically newTQueue
          for_ xs \x -> atomically (writeTQueue q x)
          _ <- traverse (\_ -> atomically (tryReadTQueue q)) xs
          atomically (tryReadTQueue q)
      r <- runRIO' program
      r `shouldEqual` Nothing
