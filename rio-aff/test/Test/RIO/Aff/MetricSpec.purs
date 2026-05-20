module Test.RIO.Aff.MetricSpec (spec) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (runRIO')
import RIO.Aff.Metric.Counter as Counter
import RIO.Aff.Metric.Gauge as Gauge
import RIO.Aff.Metric.Histogram as Histogram
import RIO.Aff.Metric.Summary as Summary

spec :: Spec Unit
spec = describe "RIO.Aff.Metric (first-class instruments)" do

  describe "Counter" do
    it "starts at zero" do
      result <- runRIO' do
        c <- Counter.make
        Counter.value c
      result `shouldEqual` 0.0

    it "increment bumps the value by one" do
      result <- runRIO' do
        c <- Counter.make
        Counter.increment c
        Counter.increment c
        Counter.increment c
        Counter.value c
      result `shouldEqual` 3.0

    it "incrementBy adds the supplied delta" do
      result <- runRIO' do
        c <- Counter.make
        Counter.incrementBy 7.5 c
        Counter.incrementBy 2.5 c
        Counter.value c
      result `shouldEqual` 10.0

  describe "Gauge" do
    it "starts at zero" do
      result <- runRIO' do
        g <- Gauge.make
        Gauge.value g
      result `shouldEqual` 0.0

    it "set overwrites the value" do
      result <- runRIO' do
        g <- Gauge.make
        Gauge.set 42.0 g
        Gauge.set 100.0 g
        Gauge.value g
      result `shouldEqual` 100.0

    it "increment and decrement move the value up and down" do
      result <- runRIO' do
        g <- Gauge.make
        Gauge.set 10.0 g
        Gauge.increment g
        Gauge.increment g
        Gauge.decrement g
        Gauge.value g
      result `shouldEqual` 11.0

    it "adjust adds an arbitrary delta (can be negative)" do
      result <- runRIO' do
        g <- Gauge.make
        Gauge.adjust 5.0 g
        Gauge.adjust (-3.0) g
        Gauge.value g
      result `shouldEqual` 2.0

  describe "Histogram" do
    it "starts with all buckets at zero" do
      snap <- runRIO' do
        h <- Histogram.make [ 1.0, 5.0, 10.0 ]
        Histogram.snapshot h
      snap.buckets `shouldEqual` [ 0, 0, 0, 0 ]
      snap.count `shouldEqual` 0
      snap.sum `shouldEqual` 0.0

    it "places each observation in the first bucket whose boundary >= value" do
      snap <- runRIO' do
        h <- Histogram.make [ 1.0, 5.0, 10.0 ]
        for_ [ 0.5, 1.0, 3.0, 5.0, 7.0, 10.0, 99.0 ]
          (\v -> Histogram.observe v h)
        Histogram.snapshot h
      -- 0.5 -> idx 0 (boundary 1.0)
      -- 1.0 -> idx 0 (boundary 1.0)
      -- 3.0 -> idx 1 (boundary 5.0)
      -- 5.0 -> idx 1 (boundary 5.0)
      -- 7.0 -> idx 2 (boundary 10.0)
      -- 10.0 -> idx 2 (boundary 10.0)
      -- 99.0 -> idx 3 (overflow)
      snap.buckets `shouldEqual` [ 2, 2, 2, 1 ]
      snap.count `shouldEqual` 7

    it "tracks the running sum of every observation" do
      snap <- runRIO' do
        h <- Histogram.make [ 10.0 ]
        Histogram.observe 1.0 h
        Histogram.observe 2.5 h
        Histogram.observe 7.5 h
        Histogram.snapshot h
      snap.sum `shouldEqual` 11.0

    it "with empty boundaries every observation lands in overflow" do
      snap <- runRIO' do
        h <- Histogram.make []
        Histogram.observe 1.0 h
        Histogram.observe 2.0 h
        Histogram.observe 3.0 h
        Histogram.snapshot h
      snap.buckets `shouldEqual` [ 3 ]
      snap.count `shouldEqual` 3

  describe "Summary" do
    it "starts empty and returns Nothing for any quantile" do
      result <- runRIO' do
        s <- Summary.make 100
        Summary.quantile 0.5 s
      result `shouldEqual` Nothing

    it "records every observation up to the capacity" do
      snap <- runRIO' do
        s <- Summary.make 5
        for_ [ 1.0, 2.0, 3.0 ] (\v -> Summary.observe v s)
        Summary.snapshot s
      snap.count `shouldEqual` 3
      snap.sum `shouldEqual` 6.0
      snap.samples `shouldEqual` [ 1.0, 2.0, 3.0 ]

    it "drops the oldest sample once the capacity is exceeded" do
      snap <- runRIO' do
        s <- Summary.make 3
        for_ [ 1.0, 2.0, 3.0, 4.0, 5.0 ] (\v -> Summary.observe v s)
        Summary.snapshot s
      snap.samples `shouldEqual` [ 3.0, 4.0, 5.0 ]

    it "computes nearest-rank quantiles over the sorted samples" do
      result <- runRIO' do
        s <- Summary.make 100
        for_ [ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0 ]
          (\v -> Summary.observe v s)
        p50 <- Summary.quantile 0.5 s
        p90 <- Summary.quantile 0.9 s
        p100 <- Summary.quantile 1.0 s
        p0 <- Summary.quantile 0.0 s
        pure { p50, p90, p100, p0 }
      -- floor(0.5 * 10) = 5; sorted[5] = 6.0
      result.p50 `shouldEqual` Just 6.0
      -- floor(0.9 * 10) = 9; sorted[9] = 10.0
      result.p90 `shouldEqual` Just 10.0
      -- q = 1.0 clamps idx to last
      result.p100 `shouldEqual` Just 10.0
      -- floor(0.0 * 10) = 0; sorted[0] = 1.0
      result.p0 `shouldEqual` Just 1.0

    it "clamps out-of-range quantiles into [0, 1]" do
      result <- runRIO' do
        s <- Summary.make 100
        for_ [ 1.0, 2.0, 3.0 ] (\v -> Summary.observe v s)
        below <- Summary.quantile (-0.5) s
        above <- Summary.quantile 5.0 s
        pure { below, above }
      result.below `shouldEqual` Just 1.0
      result.above `shouldEqual` Just 3.0
