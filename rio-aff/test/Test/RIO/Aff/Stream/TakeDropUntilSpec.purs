module Test.RIO.Aff.Stream.TakeDropUntilSpec (spec) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Stream (Stream)
import RIO.Aff.Stream as Stream

spec :: Spec Unit
spec = describe "RIO.Aff.Stream (takeUntil / dropUntil)" do

  describe "takeUntil" do
    it "emits the triggering element and then stops" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3, 4, 5 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect (Stream.takeUntil (\n -> n >= 3) s)
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "forwards the entire stream when the predicate never holds" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect (Stream.takeUntil (\_ -> false) s)
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "emits only the first element when it already satisfies the predicate" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 10, 20, 30 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect (Stream.takeUntil (\_ -> true) s)
      result <- runRIO' program
      result `shouldEqual` [ 10 ]

    it "stops pulling after the triggering element" do
      counter <- liftEffect (Ref.new 0)
      let
        producer :: Stream () () Int
        producer = Stream.repeatM
          (liftEffect (Ref.modify (_ + 1) counter))

        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.takeUntil (\n -> n == 4) producer)
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3, 4 ]
      pulls <- liftEffect (Ref.read counter)
      pulls `shouldEqual` 4

  describe "dropUntil" do
    it "drops the triggering element and yields everything after" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3, 4, 5 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect (Stream.dropUntil (\n -> n == 3) s)
      result <- runRIO' program
      result `shouldEqual` [ 4, 5 ]

    it "yields nothing when the predicate never holds" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect (Stream.dropUntil (\_ -> false) s)
      result <- runRIO' program
      result `shouldEqual` []

    it "yields everything after when the first element triggers" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect (Stream.dropUntil (\_ -> true) s)
      result <- runRIO' program
      result `shouldEqual` [ 2, 3 ]

    it "yields an empty stream on empty input" do
      let
        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.dropUntil (\_ -> true) (Stream.empty :: Stream () () Int))
      result <- runRIO' program
      result `shouldEqual` []
