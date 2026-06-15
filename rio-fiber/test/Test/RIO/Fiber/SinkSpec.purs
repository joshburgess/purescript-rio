module Test.RIO.Fiber.SinkSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
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

  it "takeWhile: collects the prefix that satisfies p, dropping the violator" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runSink (S.fromArray [ 1, 2, 3, 10, 4, 5 ])
        (Sink.takeWhile (_ < 10))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "takeWhile: short-circuits the upstream on first violation" do
    pullsRef <- liftEffect (Ref.new 0)
    let
      countingStream :: Int -> S.Stream () () Int
      countingStream i = S.Stream do
        F.liftEffect (Ref.modify_ (_ + 1) pullsRef)
        pure (S.Yield i (countingStream (i + 1)))

      prog :: F.RIO () () (Array Int)
      prog = S.runSink (countingStream 0) (Sink.takeWhile (_ < 3))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 0, 1, 2 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    pulls <- liftEffect (Ref.read pullsRef)
    pulls `shouldEqual` 4

  it "takeWhile: returns the full array when no element violates p" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runSink (S.fromArray [ 1, 2, 3 ]) (Sink.takeWhile (_ < 100))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "dropWhile: skips the leading prefix and keeps the violator onward" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runSink (S.fromArray [ 1, 2, 3, 10, 4, 5 ])
        (Sink.dropWhile (_ < 10))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 4, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "dropWhile: empty array when every element matches the predicate" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runSink (S.fromArray [ 1, 2, 3 ]) (Sink.dropWhile (_ < 100))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` []
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "dropWhile: keeps every element when the first one violates p" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runSink (S.fromArray [ 10, 1, 2, 3 ]) (Sink.dropWhile (_ < 5))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mkString: empty stream yields empty string" do
    let
      prog :: F.RIO () () String
      prog = S.runSink S.empty (Sink.mkString ", ")
    out <- runAff prog {}
    case out of
      Success s -> s `shouldEqual` ""
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mkString: single element yields itself with no separator" do
    let
      prog :: F.RIO () () String
      prog = S.runSink (S.fromArray [ "alone" ]) (Sink.mkString ", ")
    out <- runAff prog {}
    case out of
      Success s -> s `shouldEqual` "alone"
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mkString: joins with the separator between elements" do
    let
      prog :: F.RIO () () String
      prog = S.runSink (S.fromArray [ "a", "b", "c" ]) (Sink.mkString "-")
    out <- runAff prog {}
    case out of
      Success s -> s `shouldEqual` "a-b-c"
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "findRIO: returns Just the first element matching the predicate" do
    let
      prog :: F.RIO () () (Maybe Int)
      prog = S.runSink (S.fromArray [ 1, 2, 3, 4, 5 ])
        (Sink.findRIO (\i -> pure (i > 2)))
    out <- runAff prog {}
    case out of
      Success m -> m `shouldEqual` Just 3
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "findRIO: short-circuits the upstream on the first hit" do
    pullsRef <- liftEffect (Ref.new 0)
    let
      countingStream :: Int -> S.Stream () () Int
      countingStream i = S.Stream do
        F.liftEffect (Ref.modify_ (_ + 1) pullsRef)
        pure (S.Yield i (countingStream (i + 1)))

      prog :: F.RIO () () (Maybe Int)
      prog = S.runSink (countingStream 0)
        (Sink.findRIO (\i -> pure (i == 2)))
    out <- runAff prog {}
    case out of
      Success m -> m `shouldEqual` Just 2
      other -> fail ("expected Success, got " <> describeOutcome other)
    pulls <- liftEffect (Ref.read pullsRef)
    pulls `shouldEqual` 3

  it "findRIO: Nothing when no element matches" do
    let
      prog :: F.RIO () () (Maybe Int)
      prog = S.runSink (S.fromArray [ 1, 2, 3 ])
        (Sink.findRIO (\i -> pure (i > 100)))
    out <- runAff prog {}
    case out of
      Success m -> m `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  describe "foldRIOUntil" do
    it "terminates early when the effectful step pushes acc past stop" do
      let
        prog :: F.RIO () () Int
        prog = S.runSink (S.fromArray [ 1, 2, 3, 4, 5 ])
          (Sink.foldRIOUntil (_ >= 6) (\b a -> pure (b + a)) 0)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 6 -- 1+2+3 stops at >=6
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "returns the final acc when stop never fires" do
      let
        prog :: F.RIO () () Int
        prog = S.runSink (S.fromArray [ 1, 2, 3 ])
          (Sink.foldRIOUntil (_ >= 100) (\b a -> pure (b + a)) 0)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 6
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "dropN" do
    it "skips the first n elements and collects the rest" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runSink (S.fromArray [ 1, 2, 3, 4, 5 ]) (Sink.dropN 2)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 3, 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "non-positive n collects the full stream" do
      let
        prog :: F.RIO () () (Array Int)
        prog = S.runSink (S.fromArray [ 7, 8, 9 ]) (Sink.dropN 0)
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 7, 8, 9 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "race" do
    it "returns the left sink's result when it terminates first" do
      let
        prog :: F.RIO () () (Maybe Int)
        prog = S.runSink (S.fromArray [ 1, 2, 3, 4, 5 ])
          (Sink.race Sink.head Sink.last)
      out <- runAff prog {}
      case out of
        Success m -> m `shouldEqual` Just 1
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "falls back to the left done when neither side terminates early" do
      let
        prog :: F.RIO () () Int
        prog = S.runSink (S.fromArray [ 1, 2, 3 ])
          (Sink.race Sink.sum Sink.count)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 6 -- left = sum [1,2,3]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "zipPar" do
    it "pairs results from two sinks that both fold the whole stream" do
      let
        prog :: F.RIO () () (Tuple Int Int)
        prog = S.runSink (S.fromArray [ 1, 2, 3, 4 ])
          (Sink.zipPar Sink.sum Sink.count)
      out <- runAff prog {}
      case out of
        Success r -> r `shouldEqual` Tuple 10 4
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "one side terminating early keeps the other side consuming" do
      -- head terminates on the first element; sum still sees every element (both sinks step on each input)
      let
        prog :: F.RIO () () (Tuple (Maybe Int) Int)
        prog = S.runSink (S.fromArray [ 5, 10, 20, 40 ])
          (Sink.zipPar Sink.head Sink.sum)
      out <- runAff prog {}
      case out of
        Success r -> r `shouldEqual` Tuple (Just 5) 75
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "zipWithPar combines the two results via the supplied function" do
      let
        prog :: F.RIO () () Int
        prog = S.runSink (S.fromArray [ 1, 2, 3, 4 ])
          (Sink.zipWithPar (+) Sink.sum Sink.count)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 14 -- sum 10 + count 4
        other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
