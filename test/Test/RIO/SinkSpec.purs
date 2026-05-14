module Test.RIO.SinkSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (runRIO')
import RIO.Sink (Sink)
import RIO.Sink as Sink
import RIO.Stream (Stream)
import RIO.Stream as Stream

source :: Stream () () Int
source = Stream.fromArray [ 1, 2, 3, 4, 5 ]

spec :: Spec Unit
spec = describe "RIO.Sink" do

  describe "primitives" do
    it "drain returns unit and consumes the whole stream" do
      r <- runRIO' (Sink.runSink Sink.drain source)
      r `shouldEqual` unit

    it "head returns Just of the first element" do
      r <- runRIO' (Sink.runSink Sink.head source)
      r `shouldEqual` Just 1

    it "head returns Nothing on an empty stream" do
      r <- runRIO'
        (Sink.runSink (Sink.head :: Sink () () Int (Maybe Int)) Stream.empty)
      r `shouldEqual` Nothing

    it "last returns Just of the final element" do
      r <- runRIO' (Sink.runSink Sink.last source)
      r `shouldEqual` Just 5

    it "last returns Nothing on an empty stream" do
      r <- runRIO'
        (Sink.runSink (Sink.last :: Sink () () Int (Maybe Int)) Stream.empty)
      r `shouldEqual` Nothing

    it "count returns the number of elements" do
      r <- runRIO' (Sink.runSink Sink.count source)
      r `shouldEqual` 5

    it "count returns 0 on an empty stream" do
      r <- runRIO'
        (Sink.runSink (Sink.count :: Sink () () Int Int) Stream.empty)
      r `shouldEqual` 0

    it "collect returns every element in input order" do
      r <- runRIO' (Sink.runSink Sink.collect source)
      r `shouldEqual` [ 1, 2, 3, 4, 5 ]

  describe "folds" do
    it "foldL accumulates with a pure step" do
      r <- runRIO' (Sink.runSink (Sink.foldL 0 (+)) source)
      r `shouldEqual` 15

    it "foldM accumulates with an effectful step" do
      ref <- liftEffect (Ref.new 0)
      let
        step acc i = do
          liftEffect (Ref.modify_ (_ + 1) ref)
          pure (acc + i)
      r <- runRIO' (Sink.runSink (Sink.foldM 0 step) source)
      r `shouldEqual` 15
      calls <- liftEffect (Ref.read ref)
      calls `shouldEqual` 5

  describe "short-circuiting" do
    it "take n returns the first n elements" do
      r <- runRIO' (Sink.runSink (Sink.take 3) source)
      r `shouldEqual` [ 1, 2, 3 ]

    it "take 0 returns the empty array without pulling" do
      r <- runRIO' (Sink.runSink (Sink.take 0) source)
      r `shouldEqual` ([] :: Array Int)

    it "take returns what it could when the stream is short" do
      r <- runRIO' (Sink.runSink (Sink.take 10) source)
      r `shouldEqual` [ 1, 2, 3, 4, 5 ]

    it "find returns the first match and halts" do
      r <- runRIO' (Sink.runSink (Sink.find (_ > 3)) source)
      r `shouldEqual` Just 4

    it "find returns Nothing when nothing matches" do
      r <- runRIO' (Sink.runSink (Sink.find (_ > 100)) source)
      r `shouldEqual` Nothing

    it "any short-circuits on a match" do
      r <- runRIO' (Sink.runSink (Sink.any (_ == 3)) source)
      r `shouldEqual` true

    it "any returns false on an empty stream" do
      r <- runRIO'
        ( Sink.runSink
            (Sink.any (_ == 3) :: Sink () () Int Boolean)
            Stream.empty
        )
      r `shouldEqual` false

    it "all short-circuits on a non-match" do
      r <- runRIO' (Sink.runSink (Sink.all (_ < 4)) source)
      r `shouldEqual` false

    it "all returns true on an empty stream" do
      r <- runRIO'
        ( Sink.runSink
            (Sink.all (_ < 4) :: Sink () () Int Boolean)
            Stream.empty
        )
      r `shouldEqual` true

  describe "combinators" do
    it "mapResult post-processes the result" do
      r <- runRIO' (Sink.runSink (Sink.mapResult show Sink.count) source)
      r `shouldEqual` "5"

    it "mapInput pre-processes each element" do
      r <- runRIO' (Sink.runSink (Sink.mapInput show Sink.collect) source)
      r `shouldEqual` [ "1", "2", "3", "4", "5" ]

    it "filterIn drops elements before the inner sink sees them" do
      r <- runRIO'
        ( Sink.runSink
            (Sink.filterIn (\n -> n `mod` 2 == 0) Sink.collect)
            source
        )
      r `shouldEqual` [ 2, 4 ]

    it "andThen sequences two sinks at the same stream position" do
      let
        sink = Sink.head `Sink.andThen` \mFirst ->
          Sink.mapResult (\rest -> { first: mFirst, rest }) Sink.collect
      r <- runRIO' (Sink.runSink sink source)
      r `shouldEqual` { first: Just 1, rest: [ 2, 3, 4, 5 ] }

    it "andThen on empty stream runs the second sink's finish" do
      let
        sink = Sink.head `Sink.andThen` \_ -> Sink.count
      r <- runRIO' (Sink.runSink (sink :: Sink () () Int Int) Stream.empty)
      r `shouldEqual` 0

  describe "Stream interop" do
    it "Sink.collect matches runCollect" do
      r1 <- runRIO' (Sink.runSink Sink.collect source)
      r2 <- runRIO' (Stream.runCollect source)
      r1 `shouldEqual` r2

    it "Sink.foldL matches runFold" do
      r1 <- runRIO' (Sink.runSink (Sink.foldL 100 (+)) source)
      r2 <- runRIO' (Stream.runFold 100 (+) source)
      r1 `shouldEqual` r2
