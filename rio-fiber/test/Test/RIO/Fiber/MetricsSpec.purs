module Test.RIO.Fiber.MetricsSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Metrics as M
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Metrics" do
  describe "Counter" do
    it "incr / incrBy accumulate" do
      c <- liftEffect M.newCounter
      let
        prog :: F.RIO () () Int
        prog = do
          M.incr c
          M.incr c
          M.incrBy c 5
          M.counterValue c
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 7
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "incrBy with a negative argument is a no-op" do
      c <- liftEffect M.newCounter
      let
        prog :: F.RIO () () Int
        prog = do
          M.incrBy c 3
          M.incrBy c (-10)
          M.counterValue c
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 3
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "Gauge" do
    it "set / incr / decr move the value" do
      g <- liftEffect (M.newGauge 0.0)
      let
        prog :: F.RIO () () Number
        prog = do
          M.set g 10.0
          M.gaugeIncr g 2.5
          M.gaugeDecr g 0.5
          M.gaugeValue g
      out <- runAff prog {}
      case out of
        Success x -> x `shouldEqual` 12.0
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "Histogram" do
    it "tracks count, sum, min, max" do
      h <- liftEffect (M.newHistogram 8)
      let
        prog :: F.RIO () () M.HistogramSummary
        prog = do
          M.record h 1.0
          M.record h 5.0
          M.record h 3.0
          M.summary h
      out <- runAff prog {}
      case out of
        Success s -> do
          s.count `shouldEqual` 3
          s.sum `shouldEqual` 9.0
          s.min `shouldEqual` Just 1.0
          s.max `shouldEqual` Just 5.0
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "summary returns Nothing percentiles when empty" do
      h <- liftEffect (M.newHistogram 4)
      let
        prog :: F.RIO () () M.HistogramSummary
        prog = M.summary h
      out <- runAff prog {}
      case out of
        Success s -> do
          s.count `shouldEqual` 0
          s.p50 `shouldEqual` Nothing
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "BucketHistogram" do
    it "records into the lowest bucket whose le >= sample" do
      h <- liftEffect (M.newBucketHistogram (M.Explicit [ 1.0, 5.0, 10.0 ]))
      let
        prog :: F.RIO () () M.BucketSnapshot
        prog = do
          M.recordBucket h 0.5
          M.recordBucket h 3.0
          M.recordBucket h 4.999
          M.recordBucket h 7.0
          M.recordBucket h 50.0
          M.bucketSnapshot h
      out <- runAff prog {}
      case out of
        Success snap -> do
          snap.count `shouldEqual` 5
          snap.sum `shouldEqual` (0.5 + 3.0 + 4.999 + 7.0 + 50.0)
          let
            counts = map _.count snap.buckets
          -- cumulative: <=1 (1), <=5 (1+2=3), <=10 (3+1=4), overflow (4+1=5)
          counts `shouldEqual` [ 1, 3, 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "linear layout builds evenly-spaced boundaries" do
      h <- liftEffect
        (M.newBucketHistogram (M.Linear { start: 1.0, width: 2.0, count: 3 }))
      let
        prog :: F.RIO () () M.BucketSnapshot
        prog = do
          M.recordBucket h 0.5
          M.recordBucket h 2.5
          M.recordBucket h 4.5
          M.recordBucket h 99.0
          M.bucketSnapshot h
      out <- runAff prog {}
      case out of
        Success snap -> do
          let bounds = map _.le snap.buckets
          -- boundaries 1.0, 3.0, 5.0 + overflow (infinity)
          Array.take 3 bounds `shouldEqual` [ 1.0, 3.0, 5.0 ]
          snap.count `shouldEqual` 4
          let counts = map _.count snap.buckets
          counts `shouldEqual` [ 1, 2, 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "exponential layout builds factor-spaced boundaries" do
      h <- liftEffect
        ( M.newBucketHistogram
            (M.Exponential { start: 1.0, factor: 2.0, count: 4 })
        )
      let
        prog :: F.RIO () () M.BucketSnapshot
        prog = M.bucketSnapshot h
      out <- runAff prog {}
      case out of
        Success snap -> do
          let bounds = map _.le snap.buckets
          Array.take 4 bounds `shouldEqual` [ 1.0, 2.0, 4.0, 8.0 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "snapshot of an empty histogram has zero counts" do
      h <- liftEffect (M.newBucketHistogram (M.Explicit [ 1.0, 5.0 ]))
      let
        prog :: F.RIO () () M.BucketSnapshot
        prog = M.bucketSnapshot h
      out <- runAff prog {}
      case out of
        Success snap -> do
          snap.count `shouldEqual` 0
          snap.sum `shouldEqual` 0.0
          let counts = map _.count snap.buckets
          counts `shouldEqual` [ 0, 0, 0 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "everything beyond the top boundary lands in the overflow bucket" do
      h <- liftEffect (M.newBucketHistogram (M.Explicit [ 1.0, 5.0 ]))
      let
        prog :: F.RIO () () M.BucketSnapshot
        prog = do
          M.recordBucket h 100.0
          M.recordBucket h 1000.0
          M.bucketSnapshot h
      out <- runAff prog {}
      case out of
        Success snap -> do
          let counts = map _.count snap.buckets
          counts `shouldEqual` [ 0, 0, 2 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "withCounter" do
    it "bumps on every completion regardless of outcome" do
      c <- liftEffect M.newCounter
      let
        ok :: F.RIO () () Int
        ok = M.withCounter c (pure 1)

        bad :: F.RIO () () Int
        bad = M.withCounter c (F.die (Exception.error "boom"))
      _ <- runAff ok {}
      _ <- runAff ok {}
      _ <- runAff bad {}
      total <- runAff (M.counterValue c) {}
      case total of
        Success n -> n `shouldEqual` 3
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "passes the action's success value through unchanged" do
      c <- liftEffect M.newCounter
      let
        prog :: F.RIO () () Int
        prog = M.withCounter c (pure 42)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 42
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "withTimer" do
    it "records one sample per invocation" do
      h <- liftEffect (M.newHistogram 8)
      let
        prog :: F.RIO () () Unit
        prog = do
          _ <- M.withTimer h (pure 1)
          _ <- M.withTimer h (pure 2)
          _ <- M.withTimer h (pure 3)
          pure unit
      _ <- runAff prog {}
      s <- runAff (M.summary h) {}
      case s of
        Success summary -> summary.count `shouldEqual` 3
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "records a non-negative duration" do
      h <- liftEffect (M.newHistogram 8)
      let
        prog :: F.RIO () () Unit
        prog = M.withTimer h
          (F.sleep (Milliseconds 10.0))
      _ <- runAff prog {}
      s <- runAff (M.summary h) {}
      case s of
        Success summary -> do
          summary.count `shouldEqual` 1
          case summary.min of
            Just m -> (m >= 0.0) `shouldEqual` true
            Nothing -> fail "expected at least one recorded sample"
        other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
