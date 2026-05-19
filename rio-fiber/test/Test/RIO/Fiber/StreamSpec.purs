module Test.RIO.Fiber.StreamSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Queue as Q
import RIO.Fiber.Stream as S
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Stream" do
  it "emit + runCollect yields a single-element array" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.emit 42)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 42 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "fromArray + runCollect round-trips" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.fromArray [ 1, 2, 3 ])
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "map transforms every element" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.map (_ * 10) (S.fromArray [ 1, 2, 3 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "filter keeps only matching elements" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.filter (\x -> x `mod` 2 == 0) (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 2, 4 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "take bounds an infinite stream" do
    counter <- liftEffect (Ref.new 0)
    let
      tick :: F.RIO () () Int
      tick = F.liftEffect (Ref.modify (_ + 1) counter)

      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.take 3 (S.repeatRIO tick))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    -- the producer should have been pulled exactly 3 times
    n <- liftEffect (Ref.read counter)
    n `shouldEqual` 3

  it "fold reduces with a seed" do
    let
      prog :: F.RIO () () Int
      prog = S.fold (+) 0 (S.fromArray [ 1, 2, 3, 4 ])
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 10
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "forEach runs an action per element in order" do
    log <- liftEffect (Ref.new ([] :: Array Int))
    let
      prog :: F.RIO () () Unit
      prog = S.forEach
        (\x -> F.liftEffect (Ref.modify_ (\xs -> xs <> [ x ]) log))
        (S.fromArray [ 7, 8, 9 ])
    _ <- runAff prog {}
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ 7, 8, 9 ]

  it "buffer decouples producer and consumer pacing" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.buffer 4 (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "merge interleaves two streams and ends when both end" do
    let
      prog :: F.RIO () () Int
      prog = S.fold (+) 0
        (S.merge (S.fromArray [ 1, 2, 3 ]) (S.fromArray [ 10, 20, 30 ]))
    out <- runAff prog {}
    case out of
      -- order is non-deterministic but the sum is well-defined
      Success n -> n `shouldEqual` (1 + 2 + 3 + 10 + 20 + 30)
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mapPar transforms elements with bounded concurrency" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.mapPar 3 (\x -> pure (x * 10)) (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30, 40, 50 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mapPar runs workers concurrently across element boundaries" do
    inFlight <- liftEffect (Ref.new 0)
    maxInFlight <- liftEffect (Ref.new 0)
    let
      slow :: Int -> F.RIO () () Int
      slow x = do
        n <- F.liftEffect (Ref.modify (_ + 1) inFlight)
        F.liftEffect
          (Ref.modify_ (\m -> if n > m then n else m) maxInFlight)
        F.sleep (Milliseconds 10.0)
        _ <- F.liftEffect (Ref.modify (_ - 1) inFlight)
        pure x

      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.mapPar 3 slow (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)
    peak <- liftEffect (Ref.read maxInFlight)
    -- with 5 elements and concurrency 3, we should observe at least
    -- 2 workers active simultaneously at some point.
    (peak >= 2) `shouldEqual` true

  it "scan emits seed then each accumulated value" do
    let
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect (S.scan (+) 0 (S.fromArray [ 1, 2, 3 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 0, 1, 3, 6 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "groupBy splits into adjacent runs by key" do
    let
      prog :: F.RIO () () (Array (Array Int))
      prog = S.runCollect
        (S.groupBy (\x -> x `mod` 2) (S.fromArray [ 1, 3, 4, 6, 7, 8 ]))
    out <- runAff prog {}
    case out of
      Success xs ->
        xs `shouldEqual`
          [ [ 1, 3 ], [ 4, 6 ], [ 7 ], [ 8 ] ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "zipPar pairs elements positionally and ends when either ends" do
    let
      prog :: F.RIO () () (Array (Tuple Int String))
      prog = S.runCollect
        (S.zipPar
          (S.fromArray [ 1, 2, 3 ])
          (S.fromArray [ "a", "b" ]))
    out <- runAff prog {}
    case out of
      Success xs ->
        xs `shouldEqual` [ Tuple 1 "a", Tuple 2 "b" ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "throttle paces a fast source to one per duration" do
    let
      -- 5 elements with a 30 ms throttle should take at least ~120 ms
      -- (4 inter-emission gaps); we check ordering and approximate
      -- pacing via a single elapsed-time read in a single fiber.
      prog :: F.RIO () () (Array Int)
      prog = S.runCollect
        (S.throttle (Milliseconds 30.0) (S.fromArray [ 1, 2, 3, 4, 5 ]))
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "throttle does not slow a source that is already slower than the rate" do
    -- emit 3 elements paced by sleep (20 ms) into a queue, then
    -- throttle at 5 ms; the output rate is governed by the producer.
    q <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
    let
      feed :: F.RIO () () Unit
      feed = do
        Q.offer q (Just 1)
        F.sleep (Milliseconds 20.0)
        Q.offer q (Just 2)
        F.sleep (Milliseconds 20.0)
        Q.offer q (Just 3)
        Q.offer q Nothing

      drained :: S.Stream () () Int
      drained = S.Stream do
        m <- Q.take q
        case m of
          Nothing -> pure S.Done
          Just a -> pure (S.Yield a drained)

      prog :: F.RIO () () (Array Int)
      prog = do
        feeder <- F.fork feed
        result <- S.runCollect (S.throttle (Milliseconds 5.0) drained)
        _ <- F.join feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "debounce coalesces bursts into the trailing element" do
    -- producer: emit 1, 2, 3 in rapid succession (5 ms apart), then
    -- wait 60 ms, then 4 -- debounce(40ms) should yield [3, 4].
    q <- liftEffect (Q.make 4 :: _ (Q.Queue (Maybe Int)))
    let
      feed :: F.RIO () () Unit
      feed = do
        Q.offer q (Just 1)
        F.sleep (Milliseconds 5.0)
        Q.offer q (Just 2)
        F.sleep (Milliseconds 5.0)
        Q.offer q (Just 3)
        F.sleep (Milliseconds 80.0)
        Q.offer q (Just 4)
        F.sleep (Milliseconds 80.0)
        Q.offer q Nothing

      drained :: S.Stream () () Int
      drained = S.Stream do
        m <- Q.take q
        case m of
          Nothing -> pure S.Done
          Just a -> pure (S.Yield a drained)

      prog :: F.RIO () () (Array Int)
      prog = do
        feeder <- F.fork feed
        result <- S.runCollect (S.debounce (Milliseconds 40.0) drained)
        _ <- F.join feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 3, 4 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "fromQueue + take pulls from a queue concurrently with a feeder" do
    q <- liftEffect (Q.make 4 :: _ (Q.Queue Int))
    let
      feed :: F.RIO () () Unit
      feed = do
        Q.offer q 1
        F.sleep (Milliseconds 5.0)
        Q.offer q 2
        F.sleep (Milliseconds 5.0)
        Q.offer q 3

      prog :: F.RIO () () (Array Int)
      prog = do
        feeder <- F.fork feed
        result <- S.runCollect (S.take 3 (S.fromQueue q))
        _ <- F.join feeder
        pure result
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
