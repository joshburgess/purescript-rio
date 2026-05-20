module Test.RIO.Aff.STM.TMap.PropertiesSpec (spec) where

import Prelude

import Data.Array (length, nub) as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.STM (atomically)
import RIO.Aff.STM.TMap
  ( deleteTMap
  , insertTMap
  , lookupTMap
  , memberTMap
  , newTMap
  , sizeTMap
  )

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Aff.STM.TMap (property tests)" do
  -- Insert / lookup round-trips at fixed `(1, 100)` are pinned
  -- in the unit suite. Generalize to arbitrary `Int` key/value
  -- pairs so a regression that only manifests at zero, negative,
  -- or large keys is still caught.
  it "after insertTMap k v, lookupTMap k returns Just v" do
    forAll (arbitrary :: Gen { k :: Int, v :: Int }) \{ k, v } -> do
      let
        program :: RIO () () (Maybe Int)
        program = do
          m <- atomically (newTMap :: _ (_ Int Int))
          atomically (insertTMap k v m)
          atomically (lookupTMap k m)
      r <- runRIO' program
      r `shouldEqual` Just v

  it "memberTMap agrees with insertion presence" do
    forAll (arbitrary :: Gen { k :: Int, v :: Int }) \{ k, v } -> do
      let
        program :: RIO () () { before :: Boolean, after :: Boolean }
        program = do
          m <- atomically (newTMap :: _ (_ Int Int))
          before <- atomically (memberTMap k m)
          atomically (insertTMap k v m)
          after <- atomically (memberTMap k m)
          pure { before, after }
      r <- runRIO' program
      r `shouldEqual` { before: false, after: true }

  it "after insertTMap k v then deleteTMap k, lookupTMap k returns Nothing" do
    -- Pin the insert-delete-lookup round trip across arbitrary
    -- key/value pairs. The unit pins for `deleteTMap` cover a
    -- single fixed pair; this property catches a regression that
    -- only manifests at zero, negative, or out-of-range keys.
    forAll (arbitrary :: Gen { k :: Int, v :: Int }) \{ k, v } -> do
      let
        program :: RIO () () (Maybe Int)
        program = do
          m <- atomically (newTMap :: _ (_ Int Int))
          atomically (insertTMap k v m)
          atomically (deleteTMap k m)
          atomically (lookupTMap k m)
      r <- runRIO' program
      r `shouldEqual` Nothing

  it "sizeTMap equals the number of distinct keys inserted" do
    -- Inserting the same key twice replaces the value, so the
    -- size is the count of distinct keys, not the count of
    -- insertions. Pin this against `Array.nub` on the keys to
    -- catch a regression that double-counted on overwrite.
    forAll (arbitrary :: Gen (Array (Tuple Int Int))) \pairs -> do
      let
        program :: RIO () () Int
        program = do
          m <- atomically (newTMap :: _ (_ Int Int))
          for_ pairs \(Tuple k v) -> atomically (insertTMap k v m)
          atomically (sizeTMap m)
      r <- runRIO' program
      r `shouldEqual` Array.length (Array.nub (map fst pairs))
