module Test.RIO.Aff.PipeSpec (spec) where

import Prelude

import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Pipe as Pipe
import RIO.Aff.Stream as S

spec :: Spec Unit
spec = describe "RIO.Aff.Pipe" do
  it "identity: passes every element through unchanged" do
    let
      program :: RIO () () (Array Int)
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3 ]) Pipe.identity)
    xs <- runRIO' program
    xs `shouldEqual` [ 1, 2, 3 ]

  it "map: applies the function to every element" do
    let
      program :: RIO () () (Array Int)
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3 ]) (Pipe.map (_ * 10)))
    xs <- runRIO' program
    xs `shouldEqual` [ 10, 20, 30 ]

  it "filter: keeps elements that satisfy the predicate" do
    let
      program :: RIO () () (Array Int)
      program = S.runCollect
        ( S.via (S.fromArray [ 1, 2, 3, 4, 5, 6 ])
            (Pipe.filter (\n -> n `mod` 2 == 0))
        )
    xs <- runRIO' program
    xs `shouldEqual` [ 2, 4, 6 ]

  it "mapAccum: stateful map computes running sum" do
    let
      program :: RIO () () (Array Int)
      program = S.runCollect
        ( S.via (S.fromArray [ 1, 2, 3, 4 ])
            (Pipe.mapAccum 0 (\s a -> let s' = s + a in Tuple s' s'))
        )
    xs <- runRIO' program
    xs `shouldEqual` [ 1, 3, 6, 10 ]

  it "take: forwards the first n and stops" do
    let
      program :: RIO () () (Array Int)
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4, 5 ]) (Pipe.take 3))
    xs <- runRIO' program
    xs `shouldEqual` [ 1, 2, 3 ]

  it "take 0: emits nothing" do
    let
      program :: RIO () () (Array Int)
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3 ]) (Pipe.take 0))
    xs <- runRIO' program
    xs `shouldEqual` []

  it "chunked: groups into fixed-size arrays with a trailing partial chunk" do
    let
      program :: RIO () () (Array (Array Int))
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4, 5 ]) (Pipe.chunked 2))
    xs <- runRIO' program
    xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ], [ 5 ] ]

  it "chunked: emits nothing extra when the input divides evenly" do
    let
      program :: RIO () () (Array (Array Int))
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4 ]) (Pipe.chunked 2))
    xs <- runRIO' program
    xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ] ]

  it "andThen: composes map then filter" do
    let
      pipeline :: Pipe.Pipe () () Int Int
      pipeline = Pipe.andThen (Pipe.map (_ * 10)) (Pipe.filter (_ > 15))

      program :: RIO () () (Array Int)
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4 ]) pipeline)
    xs <- runRIO' program
    xs `shouldEqual` [ 20, 30, 40 ]

  it "andThen with take: respects the cap from the downstream pipe" do
    let
      pipeline :: Pipe.Pipe () () Int Int
      pipeline = Pipe.andThen (Pipe.map (_ + 100)) (Pipe.take 2)

      program :: RIO () () (Array Int)
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4, 5 ]) pipeline)
    xs <- runRIO' program
    xs `shouldEqual` [ 101, 102 ]

  it "andThen with chunked then take: produces two chunks and stops" do
    let
      pipeline :: Pipe.Pipe () () Int (Array Int)
      pipeline = Pipe.andThen (Pipe.chunked 2) (Pipe.take 2)

      program :: RIO () () (Array (Array Int))
      program = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4, 5, 6, 7 ]) pipeline)
    xs <- runRIO' program
    xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ] ]

  it "via on an empty stream still runs onDone (chunked emits nothing)" do
    let
      program :: RIO () () (Array (Array Int))
      program = S.runCollect (S.via S.empty (Pipe.chunked 3))
    xs <- runRIO' program
    xs `shouldEqual` []
