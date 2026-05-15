module Test.RIO.STM.TMap.QuerySpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.STM (atomically)
import RIO.STM.TMap
  ( TMap
  , clearTMap
  , entriesTMap
  , insertTMap
  , keysTMap
  , lookupTMap
  , newTMap
  , sizeTMap
  , updateTMap
  , valuesTMap
  )

mkPopulated :: forall e. RIO () e (TMap String Int)
mkPopulated = atomically do
  m <- (newTMap :: _ (_ String Int))
  insertTMap "alpha" 1 m
  insertTMap "bravo" 2 m
  insertTMap "charlie" 3 m
  pure m

spec :: Spec Unit
spec = describe "RIO.STM.TMap (keys / values / entries / update / clear)" do

  describe "keysTMap / valuesTMap / entriesTMap" do
    it "returns keys in ascending Ord order" do
      let
        program :: RIO () () (Array String)
        program = do
          m <- mkPopulated
          atomically (keysTMap m)
      result <- runRIO' program
      result `shouldEqual` [ "alpha", "bravo", "charlie" ]

    it "returns values in key order" do
      let
        program :: RIO () () (Array Int)
        program = do
          m <- mkPopulated
          atomically (valuesTMap m)
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "returns entries paired in key order" do
      let
        program :: RIO () () (Array (Tuple String Int))
        program = do
          m <- mkPopulated
          atomically (entriesTMap m)
      result <- runRIO' program
      result `shouldEqual`
        [ Tuple "alpha" 1, Tuple "bravo" 2, Tuple "charlie" 3 ]

    it "returns empty arrays for an empty map" do
      let
        program
          :: RIO () ()
               { keys :: Array String
               , values :: Array Int
               , entries :: Array (Tuple String Int)
               }
        program = atomically do
          m <- (newTMap :: _ (_ String Int))
          ks <- keysTMap m
          vs <- valuesTMap m
          es <- entriesTMap m
          pure { keys: ks, values: vs, entries: es }
      result <- runRIO' program
      result.keys `shouldEqual` []
      result.values `shouldEqual` []
      result.entries `shouldEqual` []

  describe "clearTMap" do
    it "removes every entry" do
      let
        program :: RIO () () { sizeBefore :: Int, sizeAfter :: Int }
        program = do
          m <- mkPopulated
          before <- atomically (sizeTMap m)
          atomically (clearTMap m)
          after <- atomically (sizeTMap m)
          pure { sizeBefore: before, sizeAfter: after }
      result <- runRIO' program
      result.sizeBefore `shouldEqual` 3
      result.sizeAfter `shouldEqual` 0

    it "leaves an already-empty map empty" do
      let
        program :: RIO () () Int
        program = do
          m <- atomically (newTMap :: _ (_ String Int))
          atomically (clearTMap m)
          atomically (sizeTMap m)
      result <- runRIO' program
      result `shouldEqual` 0

  describe "updateTMap" do
    it "applies the function to a present key" do
      let
        program :: RIO () () (Maybe Int)
        program = do
          m <- mkPopulated
          atomically (updateTMap "bravo" (_ * 10) m)
          atomically (lookupTMap "bravo" m)
      result <- runRIO' program
      result `shouldEqual` Just 20

    it "is a no-op on an absent key" do
      let
        program
          :: RIO () ()
               { lookedUp :: Maybe Int, size :: Int }
        program = do
          m <- mkPopulated
          atomically (updateTMap "ghost" (_ + 1) m)
          lookedUp <- atomically (lookupTMap "ghost" m)
          size <- atomically (sizeTMap m)
          pure { lookedUp, size }
      result <- runRIO' program
      result.lookedUp `shouldEqual` Nothing
      result.size `shouldEqual` 3
