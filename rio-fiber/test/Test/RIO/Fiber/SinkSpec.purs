module Test.RIO.Fiber.SinkSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Sink as Sink
import RIO.Fiber.Stream as S
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Sink" do
  it "count: counts every element" do
    let
      prog :: F.RIO () () Int
      prog = S.runSink (S.fromArray [ 1, 2, 3, 4, 5 ]) Sink.count
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 5
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "count: zero on an empty stream" do
    let
      prog :: F.RIO () () Int
      prog = S.runSink S.empty Sink.count
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "sum: adds elements" do
    let
      prog :: F.RIO () () Int
      prog = S.runSink (S.fromArray [ 1, 2, 3, 4 ]) Sink.sum
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 10
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "collectAll: collects elements in order" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runSink (S.fromArray [ 10, 20, 30 ]) Sink.collectAll
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "head: returns the first element on a non-empty stream" do
    let
      prog :: F.RIO () () (Maybe Int)
      prog = S.runSink (S.fromArray [ 7, 8, 9 ]) Sink.head
    out <- runAff prog {}
    case out of
      Success x -> x `shouldEqual` Just 7
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "head: short-circuits the stream after one element" do
    pullsRef <- liftEffect (Ref.new 0)
    let
      countingStream :: S.Stream () () Int
      countingStream = S.Stream do
        F.liftEffect (Ref.modify_ (_ + 1) pullsRef)
        pure (S.Yield 1 countingStream)

      prog :: F.RIO () () (Maybe Int)
      prog = S.runSink countingStream Sink.head
    out <- runAff prog {}
    case out of
      Success x -> x `shouldEqual` Just 1
      other -> fail ("expected Success, got " <> describeOutcome other)
    pulls <- liftEffect (Ref.read pullsRef)
    pulls `shouldEqual` 1

  it "head: returns Nothing on an empty stream" do
    let
      prog :: F.RIO () () (Maybe Int)
      prog = S.runSink S.empty Sink.head
    out <- runAff prog {}
    case out of
      Success x -> x `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "last: returns the last element" do
    let
      prog :: F.RIO () () (Maybe Int)
      prog = S.runSink (S.fromArray [ 1, 2, 3 ]) Sink.last
    out <- runAff prog {}
    case out of
      Success x -> x `shouldEqual` Just 3
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "drain: discards every element" do
    let
      prog :: F.RIO () () Unit
      prog = S.runSink (S.fromArray [ 1, 2, 3 ]) Sink.drain
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "foreach: runs an effect per element" do
    log <- liftEffect (Ref.new ([] :: Array Int))
    let
      prog :: F.RIO () () Unit
      prog = S.runSink (S.fromArray [ 1, 2, 3 ])
        (Sink.foreach \i -> F.liftEffect (Ref.modify_ (\xs -> xs <> [ i ]) log))
    _ <- runAff prog {}
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ 1, 2, 3 ]

  it "fold: strict left fold lifts to a Sink" do
    let
      prog :: F.RIO () () Int
      prog = S.runSink (S.fromArray [ 1, 2, 3, 4 ]) (Sink.fold (+) 0)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 10
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "foldRIO: effectful step composes" do
    let
      prog :: F.RIO () () Int
      prog = S.runSink (S.fromArray [ 1, 2, 3 ])
        (Sink.foldRIO (\acc a -> pure (acc + a * 10)) 0)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 60
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "foldUntil: terminates early when the predicate fires" do
    pullsRef <- liftEffect (Ref.new 0)
    let
      countingStream :: Int -> S.Stream () () Int
      countingStream i = S.Stream do
        F.liftEffect (Ref.modify_ (_ + 1) pullsRef)
        pure (S.Yield i (countingStream (i + 1)))

      prog :: F.RIO () () Int
      prog = S.runSink (countingStream 1)
        (Sink.foldUntil (\acc -> acc >= 6) (+) 0)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 6
      other -> fail ("expected Success, got " <> describeOutcome other)
    pulls <- liftEffect (Ref.read pullsRef)
    pulls `shouldEqual` 3

  it "takeN: takes the first n elements and stops" do
    pullsRef <- liftEffect (Ref.new 0)
    let
      countingStream :: Int -> S.Stream () () Int
      countingStream i = S.Stream do
        F.liftEffect (Ref.modify_ (_ + 1) pullsRef)
        pure (S.Yield i (countingStream (i + 1)))

      prog :: F.RIO () () (Array Int)
      prog = S.runSink (countingStream 0) (Sink.takeN 3)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 0, 1, 2 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    pulls <- liftEffect (Ref.read pullsRef)
    pulls `shouldEqual` 3

  it "takeN: returns what was seen when the stream ends early" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runSink (S.fromArray [ 1, 2 ]) (Sink.takeN 5)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "takeN zero: emits an empty array without pulling" do
    pullsRef <- liftEffect (Ref.new 0)
    let
      countingStream :: S.Stream () () Int
      countingStream = S.Stream do
        F.liftEffect (Ref.modify_ (_ + 1) pullsRef)
        pure (S.Yield 0 countingStream)

      prog :: F.RIO () () (Array Int)
      prog = S.runSink countingStream (Sink.takeN 0)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` []
      other -> fail ("expected Success, got " <> describeOutcome other)
    pulls <- liftEffect (Ref.read pullsRef)
    pulls `shouldEqual` 1

  it "map: post-processes the output" do
    let
      prog :: F.RIO () () String
      prog = S.runSink (S.fromArray [ 1, 2, 3 ])
        (Sink.map show Sink.sum)
    out <- runAff prog {}
    case out of
      Success s -> s `shouldEqual` "6"
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "contramap: pre-processes each input" do
    let
      prog :: F.RIO () () Int
      prog = S.runSink (S.fromArray [ "1", "22", "333" ])
        (Sink.contramap String.length Sink.sum)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 6
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
