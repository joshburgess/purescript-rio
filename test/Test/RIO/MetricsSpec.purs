module Test.RIO.MetricsSpec (spec) where

import Prelude

import Effect.Aff (attempt, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, die, fail, provideAll, runRIO, runRIO')
import RIO.Metrics
  ( Metrics
  , incrementCounter
  , noopMetrics
  , observeHistogram
  , recordCounter
  , recordGauge
  , recordHistogram
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

  it "recordGauge records a Gauge directly (no aliasing through setGauge)" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = do
        recordGauge "queue.depth" 5.0
        recordGauge "queue.depth" 2.0
    runRIO' (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Gauge, name: "queue.depth", value: 5.0 }
      , { kind: Gauge, name: "queue.depth", value: 2.0 }
      ]

  it "recordHistogram records a Histogram directly (no aliasing through observeHistogram)" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = do
        recordHistogram "latency.ms" 1.0
        recordHistogram "latency.ms" 2.0
        recordHistogram "latency.ms" 3.0
    runRIO' (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Histogram, name: "latency.ms", value: 1.0 }
      , { kind: Histogram, name: "latency.ms", value: 2.0 }
      , { kind: Histogram, name: "latency.ms", value: 3.0 }
      ]

  it "distinct counter names are recorded independently in call order" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = do
        recordCounter "a" 1.0
        recordCounter "b" 1.0
        recordCounter "a" 1.0
    runRIO' (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Counter, name: "a", value: 1.0 }
      , { kind: Counter, name: "b", value: 1.0 }
      , { kind: Counter, name: "a", value: 1.0 }
      ]

  it "same name across kinds does not collide in the snapshot" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = do
        recordCounter "events" 1.0
        setGauge "events" 7.0
        observeHistogram "events" 12.5
    runRIO' (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Counter, name: "events", value: 1.0 }
      , { kind: Gauge, name: "events", value: 7.0 }
      , { kind: Histogram, name: "events", value: 12.5 }
      ]

  it "gauge accepts negative values" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = setGauge "balance" (-1.5)
    runRIO' (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Gauge, name: "balance", value: -1.5 } ]

  it "emissions before a typed failure survive in the snapshot" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) (boom :: Unit) Unit
      program = do
        incrementCounter "before"
        fail (Proxy :: Proxy "boom") unit
    _ <- runRIO (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Counter, name: "before", value: 1.0 } ]

  it "emissions before a defect survive in the snapshot" do
    -- Docstring promise: "newRecordingMetrics returns the service
    -- plus a snapshot action that returns every recorded emission
    -- in order." The "every emission" promise is pinned for the
    -- typed-failure path above; the symmetric defect path (`die`)
    -- has its own pin in LoggerSpec but no equivalent in
    -- MetricsSpec. Pin it here so a future refactor that adds
    -- buffering / batching / flush-on-exit to the recording
    -- backend cannot regress one termination path silently.
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = do
        incrementCounter "before"
        die (error "kaboom")
    _ <- attempt (runRIO' (provideAll { metrics: rec.metrics } program))
    records <- liftEffect rec.snapshot
    records `shouldEqual`
      [ { kind: Counter, name: "before", value: 1.0 } ]

  it "noopMetrics satisfies the row and records nothing observable" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = do
        recordCounter "hits" 1.0
        setGauge "depth" 2.0
        observeHistogram "latency.ms" 3.0
    -- Provide noopMetrics; the program type-checks and runs, but the
    -- recording backend never sees a single call because it was never
    -- given the recording service.
    runRIO' (provideAll { metrics: noopMetrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual` []

  it "empty program records nothing" do
    rec <- liftAff newRecordingMetrics
    let
      program :: RIO (metrics :: Metrics) () Unit
      program = pure unit
    runRIO' (provideAll { metrics: rec.metrics } program)
    records <- liftEffect rec.snapshot
    records `shouldEqual` []
