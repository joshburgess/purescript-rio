module Test.RIO.Stream.TapMapAccumSpec (spec) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Stream (Stream)
import RIO.Stream as Stream

spec :: Spec Unit
spec = describe "RIO.Stream (tap / mapAccum)" do

  describe "tap" do
    it "runs the effect for every element and passes values through" do
      seen <- liftEffect (Ref.new ([] :: Array Int))
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.tap (\n -> liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) seen)) s)
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]
      observed <- liftEffect (Ref.read seen)
      observed `shouldEqual` [ 1, 2, 3 ]

    it "runs the effect lazily: only for elements actually pulled" do
      seen <- liftEffect (Ref.new ([] :: Array Int))
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3, 4, 5 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect
          ( Stream.take 2
              (Stream.tap (\n -> liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) seen)) s)
          )
      result <- runRIO' program
      result `shouldEqual` [ 1, 2 ]
      observed <- liftEffect (Ref.read seen)
      observed `shouldEqual` [ 1, 2 ]

    it "is a no-op on an empty stream" do
      seen <- liftEffect (Ref.new ([] :: Array Int))
      let
        program :: RIO () () (Array Int)
        program = Stream.runCollect
          ( Stream.tap
              (\n -> liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) seen))
              (Stream.empty :: Stream () () Int)
          )
      result <- runRIO' program
      result `shouldEqual` []
      observed <- liftEffect (Ref.read seen)
      observed `shouldEqual` []

  describe "mapAccum" do
    it "threads an accumulator and emits per-step values" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 10, 20, 30 ]

        -- running sum: each emitted element is the accumulated total so far
        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.mapAccum 0 (\acc n -> Tuple (acc + n) (acc + n)) s)
      result <- runRIO' program
      result `shouldEqual` [ 10, 30, 60 ]

    it "can emit a different type than it accumulates" do
      let
        s :: Stream () () String
        s = Stream.fromArray [ "a", "b", "c" ]

        -- (index, element) pairs encoded as "n:elem"
        program :: RIO () () (Array String)
        program = Stream.runCollect
          ( Stream.mapAccum 0
              (\i x -> Tuple (i + 1) (show i <> ":" <> x))
              s
          )
      result <- runRIO' program
      result `shouldEqual` [ "0:a", "1:b", "2:c" ]

    it "yields no elements on an empty stream" do
      let
        program :: RIO () () (Array Int)
        program = Stream.runCollect
          ( Stream.mapAccum 0
              (\acc n -> Tuple (acc + n) (acc + n))
              (Stream.empty :: Stream () () Int)
          )
      result <- runRIO' program
      result `shouldEqual` []
