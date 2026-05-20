module Test.RIO.Aff.Queue.BulkSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Queue (Queue)
import RIO.Aff.Queue as Queue

spec :: Spec Unit
spec = describe "RIO.Aff.Queue (offerAll / takeAll / takeUpTo)" do

  describe "offerAll" do
    it "delivers every item, in order, into an unbounded queue" do
      let
        program :: RIO () () (Tuple (Array Int) (Array Int))
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          dropped <- Queue.offerAll q [ 1, 2, 3, 4 ]
          drained <- Queue.takeAll q
          pure (Tuple dropped drained)
      Tuple dropped drained <- runRIO' program
      dropped `shouldEqual` []
      drained `shouldEqual` [ 1, 2, 3, 4 ]

    it "returns an empty 'dropped' array on a non-shutdown queue" do
      let
        program :: RIO () () (Array Int)
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          Queue.offerAll q [ 9, 10 ]
      result <- runRIO' program
      result `shouldEqual` []

    it "reports the remaining items as 'dropped' once the queue is shut down" do
      let
        program :: RIO () () (Array Int)
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          Queue.shutdown q
          Queue.offerAll q [ 1, 2, 3 ]
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

  describe "takeAll" do
    it "drains everything currently buffered in FIFO order" do
      let
        program :: RIO () () (Array Int)
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          _ <- Queue.offer q 1
          _ <- Queue.offer q 2
          _ <- Queue.offer q 3
          Queue.takeAll q
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "returns an empty array when the queue is empty (non-blocking)" do
      let
        program :: RIO () () (Array Int)
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          Queue.takeAll q
      result <- runRIO' program
      result `shouldEqual` []

    it "a subsequent take after shutdown returns Nothing because the buffer was emptied" do
      let
        program :: RIO () () (Maybe Int)
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          _ <- Queue.offer q 100
          _ <- Queue.takeAll q
          Queue.shutdown q
          Queue.take q
      result <- runRIO' program
      result `shouldEqual` Nothing

  describe "takeUpTo" do
    it "returns up to N items in FIFO order" do
      let
        program :: RIO () () (Array Int)
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          _ <- Queue.offerAll q [ 1, 2, 3, 4, 5 ]
          Queue.takeUpTo q 3
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "returns fewer than N when the queue runs dry" do
      let
        program :: RIO () () (Array Int)
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          _ <- Queue.offerAll q [ 7, 8 ]
          Queue.takeUpTo q 10
      result <- runRIO' program
      result `shouldEqual` [ 7, 8 ]

    it "returns an empty array for n <= 0" do
      let
        program :: RIO () () (Array Int)
        program = do
          q <- liftEffect (Queue.unbounded :: _ (Queue Int))
          _ <- Queue.offer q 1
          Queue.takeUpTo q 0
      result <- runRIO' program
      result `shouldEqual` []
