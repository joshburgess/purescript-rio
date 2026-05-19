module Test.RIO.Fiber.PipeSpec (spec) where

import Prelude

import Data.Tuple (Tuple(..))
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Pipe as Pipe
import RIO.Fiber.Stream as S
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Pipe" do
  it "identity: passes every element through unchanged" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.via (S.fromArray [ 1, 2, 3 ]) Pipe.identity)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "map: applies the function to every element" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3 ]) (Pipe.map (_ * 10)))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "filter: keeps elements that satisfy the predicate" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        ( S.via (S.fromArray [ 1, 2, 3, 4, 5, 6 ])
            (Pipe.filter (\n -> n `mod` 2 == 0))
        )
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 2, 4, 6 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mapAccum: stateful map computes running sum" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        ( S.via (S.fromArray [ 1, 2, 3, 4 ])
            (Pipe.mapAccum 0 (\s a -> let s' = s + a in Tuple s' s'))
        )
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 3, 6, 10 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "take: forwards the first n and stops" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4, 5 ]) (Pipe.take 3))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "take 0: emits nothing" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3 ]) (Pipe.take 0))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` []
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "chunked: groups into fixed-size arrays with a trailing partial chunk" do
    let
      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4, 5 ]) (Pipe.chunked 2))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ], [ 5 ] ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "chunked: emits nothing extra when the input divides evenly" do
    let
      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4 ]) (Pipe.chunked 2))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ] ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "andThen: composes map then filter" do
    let
      pipeline :: Pipe.Pipe () () Int Int
      pipeline = Pipe.andThen (Pipe.map (_ * 10))
        (Pipe.filter (_ > 15))

      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.via (S.fromArray [ 1, 2, 3, 4 ]) pipeline)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 20, 30, 40 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "andThen with take: respects the cap from the downstream pipe" do
    let
      pipeline :: Pipe.Pipe () () Int Int
      pipeline = Pipe.andThen (Pipe.map (_ + 100)) (Pipe.take 2)

      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4, 5 ]) pipeline)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 101, 102 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "andThen with chunked then take: produces two chunks and stops" do
    let
      pipeline :: Pipe.Pipe () () Int (Array Int)
      pipeline = Pipe.andThen (Pipe.chunked 2) (Pipe.take 2)

      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect
        (S.via (S.fromArray [ 1, 2, 3, 4, 5, 6, 7 ]) pipeline)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ [ 1, 2 ], [ 3, 4 ] ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "via on an empty stream still runs onDone (chunked emits nothing)" do
    let
      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect (S.via S.empty (Pipe.chunked 3))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` []
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
