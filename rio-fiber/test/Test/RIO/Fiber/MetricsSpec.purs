module Test.RIO.Fiber.MetricsSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
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

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
