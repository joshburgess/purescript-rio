module Test.RIO.Stream.HeadLastFindSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Stream (Stream)
import RIO.Stream as Stream

spec :: Spec Unit
spec = describe "RIO.Stream (head / last / find / forever)" do

  describe "head" do
    it "returns Just of the first element" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Maybe Int)
        program = Stream.head s
      result <- runRIO' program
      result `shouldEqual` Just 1

    it "returns Nothing for an empty stream" do
      let
        program :: RIO () () (Maybe Int)
        program = Stream.head (Stream.empty :: Stream () () Int)
      result <- runRIO' program
      result `shouldEqual` Nothing

    it "does not pull more than the first element" do
      counter <- liftEffect (Ref.new 0)
      let
        producer :: Stream () () Int
        producer = Stream.repeatM
          (liftEffect (Ref.modify (_ + 1) counter))

        program :: RIO () () (Maybe Int)
        program = Stream.head producer
      result <- runRIO' program
      result `shouldEqual` Just 1
      pulls <- liftEffect (Ref.read counter)
      pulls `shouldEqual` 1

  describe "last" do
    it "returns Just of the final element" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3, 4 ]

        program :: RIO () () (Maybe Int)
        program = Stream.last s
      result <- runRIO' program
      result `shouldEqual` Just 4

    it "returns Nothing for an empty stream" do
      let
        program :: RIO () () (Maybe Int)
        program = Stream.last (Stream.empty :: Stream () () Int)
      result <- runRIO' program
      result `shouldEqual` Nothing

  describe "find" do
    it "returns the first matching element" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3, 4, 5 ]

        program :: RIO () () (Maybe Int)
        program = Stream.find (\n -> n > 2) s
      result <- runRIO' program
      result `shouldEqual` Just 3

    it "returns Nothing when no element matches" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Maybe Int)
        program = Stream.find (\n -> n > 10) s
      result <- runRIO' program
      result `shouldEqual` Nothing

    it "stops pulling at the first match" do
      counter <- liftEffect (Ref.new 0)
      let
        producer :: Stream () () Int
        producer = Stream.repeatM
          (liftEffect (Ref.modify (_ + 1) counter))

        program :: RIO () () (Maybe Int)
        program = Stream.find (\n -> n == 3) producer
      result <- runRIO' program
      result `shouldEqual` Just 3
      pulls <- liftEffect (Ref.read counter)
      pulls `shouldEqual` 3

  describe "forever" do
    it "repeats a finite stream until take cuts it off" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect (Stream.take 8 (Stream.forever s))
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3, 1, 2, 3, 1, 2 ]

    it "treats an empty inner stream as empty (no busy loop)" do
      let
        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.forever (Stream.empty :: Stream () () Int))
      result <- runRIO' program
      result `shouldEqual` []
