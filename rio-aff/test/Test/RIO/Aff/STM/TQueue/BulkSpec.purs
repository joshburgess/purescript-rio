module Test.RIO.Aff.STM.TQueue.BulkSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.STM (atomically)
import RIO.Aff.STM.TQueue
  ( TQueue
  , flushTQueue
  , isEmptyTQueue
  , lengthTQueue
  , newTQueue
  , tryPeekTQueue
  , writeAllTQueue
  , writeTQueue
  )

spec :: Spec Unit
spec = describe "RIO.Aff.STM.TQueue (flush / writeAll / tryPeek)" do

  describe "tryPeekTQueue" do
    it "returns Just the head without removing it" do
      let
        program :: RIO () () { peeked :: Maybe Int, size :: Int }
        program = atomically do
          q <- (newTQueue :: _ (TQueue Int))
          writeTQueue q 11
          writeTQueue q 22
          peeked <- tryPeekTQueue q
          size <- lengthTQueue q
          pure { peeked, size }
      result <- runRIO' program
      result.peeked `shouldEqual` Just 11
      result.size `shouldEqual` 2

    it "returns Nothing on an empty queue (non-blocking)" do
      let
        program :: RIO () () (Maybe Int)
        program = atomically do
          q <- (newTQueue :: _ (TQueue Int))
          tryPeekTQueue q
      result <- runRIO' program
      result `shouldEqual` Nothing

  describe "flushTQueue" do
    it "drains every element in FIFO order and leaves the queue empty" do
      let
        program
          :: RIO () ()
               { drained :: Array Int, empty_ :: Boolean }
        program = atomically do
          q <- (newTQueue :: _ (TQueue Int))
          writeTQueue q 1
          writeTQueue q 2
          writeTQueue q 3
          drained <- flushTQueue q
          empty_ <- isEmptyTQueue q
          pure { drained, empty_ }
      result <- runRIO' program
      result.drained `shouldEqual` [ 1, 2, 3 ]
      result.empty_ `shouldEqual` true

    it "returns an empty array on an empty queue (non-blocking)" do
      let
        program :: RIO () () (Array Int)
        program = atomically do
          q <- (newTQueue :: _ (TQueue Int))
          flushTQueue q
      result <- runRIO' program
      result `shouldEqual` []

  describe "writeAllTQueue" do
    it "appends every element in order" do
      let
        program :: RIO () () (Array Int)
        program = atomically do
          q <- (newTQueue :: _ (TQueue Int))
          writeAllTQueue q [ 1, 2, 3 ]
          flushTQueue q
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "is a no-op for an empty input" do
      let
        program :: RIO () () { size :: Int }
        program = atomically do
          q <- (newTQueue :: _ (TQueue Int))
          writeAllTQueue q []
          size <- lengthTQueue q
          pure { size }
      result <- runRIO' program
      result.size `shouldEqual` 0

    it "atomically commits behind prior writes (single transaction view)" do
      let
        program :: RIO () () (Array Int)
        program = atomically do
          q <- (newTQueue :: _ (TQueue Int))
          writeTQueue q 100
          writeAllTQueue q [ 200, 300 ]
          writeTQueue q 400
          flushTQueue q
      result <- runRIO' program
      result `shouldEqual` [ 100, 200, 300, 400 ]
