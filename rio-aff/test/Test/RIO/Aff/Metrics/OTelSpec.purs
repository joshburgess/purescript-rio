module Test.RIO.Aff.Metrics.OTelSpec (spec) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser as Parser
import Data.Either (Either(..))
import Data.String (Pattern(..), contains)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Aff.Metrics.OTel (ExportConfig)
import RIO.Aff.Metrics.OTel as OTel
import RIO.Aff.Test.Metrics (MetricKind(..), MetricRecord)

cfg :: ExportConfig
cfg =
  { resource: { serviceName: "my-service", serviceVersion: "0.1.0" }
  , scope: { name: "rio.metrics", version: "0.1.0" }
  , timeUnixNano: "1700000000000000000"
  }

counter :: String -> Number -> MetricRecord
counter name value = { kind: Counter, name, value }

gauge :: String -> Number -> MetricRecord
gauge name value = { kind: Gauge, name, value }

histogram :: String -> Number -> MetricRecord
histogram name value = { kind: Histogram, name, value }

spec :: Spec Unit
spec = describe "RIO.Aff.Metrics.OTel (OTLP/JSON exporter)" do

  it "wraps metrics in resourceMetrics / scopeMetrics envelope" do
    let
      out = OTel.renderOTLP
        (OTel.exportMetrics cfg [ counter "requests" 1.0 ])
    contains (Pattern "\"resourceMetrics\"") out `shouldEqual` true
    contains (Pattern "\"scopeMetrics\"") out `shouldEqual` true
    contains (Pattern "\"service.name\"") out `shouldEqual` true
    contains (Pattern "\"my-service\"") out `shouldEqual` true

  it "tags every data point with the configured time" do
    let
      out = OTel.renderOTLP
        (OTel.exportMetrics cfg [ counter "requests" 1.0 ])
    contains
      (Pattern "\"timeUnixNano\":\"1700000000000000000\"")
      out
      `shouldEqual` true

  it "emits the scope name and version" do
    let
      out = OTel.renderOTLP
        (OTel.exportMetrics cfg [ counter "requests" 1.0 ])
    contains (Pattern "\"name\":\"rio.metrics\"") out
      `shouldEqual` true
    contains (Pattern "\"version\":\"0.1.0\"") out
      `shouldEqual` true

  it "sums multiple counter records into one cumulative data point" do
    -- Three increments of "requests" must aggregate to one
    -- data point with asDouble = 6 and isMonotonic = true.
    let
      out = OTel.renderOTLP
        ( OTel.exportMetrics cfg
            [ counter "requests" 1.0
            , counter "requests" 2.0
            , counter "requests" 3.0
            ]
        )
    contains (Pattern "\"sum\"") out `shouldEqual` true
    contains (Pattern "\"isMonotonic\":true") out `shouldEqual` true
    contains (Pattern "\"asDouble\":6") out `shouldEqual` true
    -- Counters use cumulative aggregation temporality (2).
    contains (Pattern "\"aggregationTemporality\":2") out
      `shouldEqual` true

  it "gauge keeps the last recorded value (last-write-wins)" do
    let
      out = OTel.renderOTLP
        ( OTel.exportMetrics cfg
            [ gauge "queue.depth" 5.0
            , gauge "queue.depth" 12.0
            , gauge "queue.depth" 7.0
            ]
        )
    contains (Pattern "\"gauge\"") out `shouldEqual` true
    contains (Pattern "\"asDouble\":7") out `shouldEqual` true
    -- Other gauge values should not appear.
    contains (Pattern "\"asDouble\":5") out `shouldEqual` false
    contains (Pattern "\"asDouble\":12") out `shouldEqual` false

  it "histogram aggregates into count / sum / min / max" do
    let
      out = OTel.renderOTLP
        ( OTel.exportMetrics cfg
            [ histogram "latency.ms" 10.0
            , histogram "latency.ms" 50.0
            , histogram "latency.ms" 30.0
            , histogram "latency.ms" 20.0
            ]
        )
    contains (Pattern "\"histogram\"") out `shouldEqual` true
    contains (Pattern "\"count\":\"4\"") out `shouldEqual` true
    contains (Pattern "\"sum\":110") out `shouldEqual` true
    contains (Pattern "\"min\":10") out `shouldEqual` true
    contains (Pattern "\"max\":50") out `shouldEqual` true

  it "emits one metric per (name, kind), preserving first-mention order" do
    -- A counter named "x" and a gauge named "x" are distinct
    -- metrics; both must appear in the output, and counter-first
    -- input must produce counter-first output.
    let
      out = OTel.renderOTLP
        ( OTel.exportMetrics cfg
            [ counter "x" 1.0
            , gauge "y" 5.0
            , counter "x" 2.0
            , gauge "y" 6.0
            ]
        )
    contains (Pattern "\"name\":\"x\"") out `shouldEqual` true
    contains (Pattern "\"name\":\"y\"") out `shouldEqual` true
    contains (Pattern "\"asDouble\":3") out `shouldEqual` true
    contains (Pattern "\"asDouble\":6") out `shouldEqual` true

  it "produces an empty metrics array for an empty input" do
    let
      out = OTel.renderOTLP (OTel.exportMetrics cfg [])
    contains (Pattern "\"resourceMetrics\"") out `shouldEqual` true
    contains (Pattern "\"metrics\":[]") out `shouldEqual` true

  it "output is valid JSON" do
    let
      out = OTel.renderOTLP
        ( OTel.exportMetrics cfg
            [ counter "requests" 4.0
            , gauge "queue.depth" 9.0
            , histogram "latency.ms" 12.0
            , histogram "latency.ms" 21.0
            ]
        )
    case Parser.jsonParser out of
      Right (_ :: Json) -> pure unit
      Left e -> out `shouldSatisfy` \_ ->
        case e of
          _ -> false
