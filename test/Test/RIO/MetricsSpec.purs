module Test.RIO.MetricsSpec (spec) where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, provideAll, runRIO')
import RIO.Metrics
  ( Metrics
  , incrementCounter
  , observeHistogram
  , recordCounter
  , setGauge
  )
import RIO.Test.Metrics (MetricKind(..), newRecordingMetrics)

spec :: Spec Unit
spec = describe "RIO.Metrics" do
  it "records counters, gauges, and histograms in call order" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = do
        recordCounter "requests.total" 1.0
        setGauge "queue.depth" 3.0
        observeHistogram "latency.ms" 42.0
    runRIO' (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Counter, name: "requests.total", value: 1.0 }
      , { kind: Gauge, name: "queue.depth", value: 3.0 }
      , { kind: Histogram, name: "latency.ms", value: 42.0 }
      ]

  it "incrementCounter is recordCounter with delta = 1.0" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = do
        incrementCounter "hits"
        incrementCounter "hits"
        incrementCounter "hits"
    runRIO' (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Counter, name: "hits", value: 1.0 }
      , { kind: Counter, name: "hits", value: 1.0 }
      , { kind: Counter, name: "hits", value: 1.0 }
      ]
