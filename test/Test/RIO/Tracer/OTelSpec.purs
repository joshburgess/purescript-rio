module Test.RIO.Tracer.OTelSpec (spec) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser as Parser
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Tracer (Span, SpanId(..), SpanStatus(..))
import RIO.Tracer.OTel (ExportConfig)
import RIO.Tracer.OTel as OTel

cfg :: ExportConfig
cfg =
  { resource: { serviceName: "my-service", serviceVersion: "0.1.0" }
  , scope: { name: "rio.tracer", version: "0.1.0" }
  , traceId: "4bf92f3577b34da6a3ce929d0e0e4736"
  }

spans :: Array Span
spans =
  [ { id: SpanId 1
    , parent: Nothing
    , name: "root"
    , startMs: 1000.0
    , endMs: Just 1010.0
    , status: SpanOk
    , attributes: [ Tuple "request.id" "r-42" ]
    }
  , { id: SpanId 2
    , parent: Just (SpanId 1)
    , name: "child"
    , startMs: 1002.0
    , endMs: Just 1009.0
    , status: SpanFailed
    , attributes: []
    }
  ]

ids :: Map.Map SpanId String
ids = Map.fromFoldable
  [ Tuple (SpanId 1) "00f067aa0ba902b7"
  , Tuple (SpanId 2) "00f067aa0ba902b8"
  ]

spec :: Spec Unit
spec = describe "RIO.Tracer.OTel (OTLP/JSON exporter)" do
  it "wraps spans in resourceSpans / scopeSpans envelope" do
    let
      out = OTel.renderOTLP (OTel.exportSpans cfg ids spans)
    contains (Pattern "\"resourceSpans\"") out `shouldEqual` true
    contains (Pattern "\"scopeSpans\"") out `shouldEqual` true
    contains (Pattern "\"service.name\"") out `shouldEqual` true
    contains (Pattern "\"my-service\"") out `shouldEqual` true

  it "tags every span with the configured traceId" do
    let
      out = OTel.renderOTLP (OTel.exportSpans cfg ids spans)
    contains
      (Pattern "\"traceId\":\"4bf92f3577b34da6a3ce929d0e0e4736\"")
      out
      `shouldEqual` true

  it "emits per-span hex spanId from the id map" do
    let
      out = OTel.renderOTLP (OTel.exportSpans cfg ids spans)
    contains (Pattern "\"spanId\":\"00f067aa0ba902b7\"") out
      `shouldEqual` true
    contains (Pattern "\"spanId\":\"00f067aa0ba902b8\"") out
      `shouldEqual` true

  it "attaches parentSpanId when the parent is in the map" do
    let
      out = OTel.renderOTLP (OTel.exportSpans cfg ids spans)
    contains (Pattern "\"parentSpanId\":\"00f067aa0ba902b7\"") out
      `shouldEqual` true

  it "emits start / end times as stringified Unix nanoseconds" do
    let
      out = OTel.renderOTLP (OTel.exportSpans cfg ids spans)
    contains (Pattern "\"startTimeUnixNano\":\"1000000000\"") out
      `shouldEqual` true
    contains (Pattern "\"endTimeUnixNano\":\"1010000000\"") out
      `shouldEqual` true

  it "renders attributes in the keyValue / stringValue shape" do
    let
      out = OTel.renderOTLP (OTel.exportSpans cfg ids spans)
    contains (Pattern "\"key\":\"request.id\"") out `shouldEqual` true
    contains (Pattern "\"stringValue\":\"r-42\"") out
      `shouldEqual` true

  it "maps SpanOk to status code 1 and SpanFailed to code 2" do
    let
      out = OTel.renderOTLP (OTel.exportSpans cfg ids spans)
    contains (Pattern "\"code\":1") out `shouldEqual` true
    contains (Pattern "\"code\":2") out `shouldEqual` true

  it "drops spans whose id is not present in the id map" do
    let
      missingIds = Map.fromFoldable
        [ Tuple (SpanId 1) "00f067aa0ba902b7" ]
      out = OTel.renderOTLP (OTel.exportSpans cfg missingIds spans)
    contains (Pattern "\"spanId\":\"00f067aa0ba902b7\"") out
      `shouldEqual` true
    contains (Pattern "\"00f067aa0ba902b8\"") out `shouldEqual` false

  it "output is valid JSON" do
    let
      out = OTel.renderOTLP (OTel.exportSpans cfg ids spans)
    case Parser.jsonParser out of
      Right (_ :: Json) -> pure unit
      Left e -> out `shouldSatisfy` \_ ->
        -- force the assertion to fail with a useful message
        case e of
          _ -> false
