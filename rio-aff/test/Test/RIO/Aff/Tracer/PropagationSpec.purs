module Test.RIO.Aff.Tracer.PropagationSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Aff.Tracer.Propagation as Prop

spec :: Spec Unit
spec = describe "RIO.Aff.Tracer.Propagation (W3C Trace Context)" do
  describe "parseTraceparent" do
    it "parses a sampled header into traceId / spanId / sampled" do
      let
        header =
          "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
      case Prop.parseTraceparent header of
        Nothing -> fail "expected a valid parse"
        Just ctx -> do
          ctx.traceId `shouldEqual` "4bf92f3577b34da6a3ce929d0e0e4736"
          ctx.spanId `shouldEqual` "00f067aa0ba902b7"
          ctx.sampled `shouldEqual` true

    it "parses an unsampled header" do
      let
        header =
          "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00"
      case Prop.parseTraceparent header of
        Just ctx -> ctx.sampled `shouldEqual` false
        Nothing -> fail "expected a valid parse"

    it "rejects an unknown version" do
      Prop.parseTraceparent
        "01-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
        `shouldEqual` Nothing

    it "rejects all-zero trace id" do
      Prop.parseTraceparent
        "00-00000000000000000000000000000000-00f067aa0ba902b7-01"
        `shouldEqual` Nothing

    it "rejects all-zero span id" do
      Prop.parseTraceparent
        "00-4bf92f3577b34da6a3ce929d0e0e4736-0000000000000000-01"
        `shouldEqual` Nothing

    it "rejects malformed lengths" do
      Prop.parseTraceparent "00-too-short-01" `shouldEqual` Nothing

  describe "formatTraceparent" do
    it "round-trips with parseTraceparent" do
      let
        original =
          "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
      case Prop.parseTraceparent original of
        Just ctx -> Prop.formatTraceparent ctx `shouldEqual` original
        Nothing -> fail "expected a valid parse"

    it "emits trace-flags as two hex chars" do
      let
        ctx :: Prop.TraceContext
        ctx =
          { traceId: "4bf92f3577b34da6a3ce929d0e0e4736"
          , spanId: "00f067aa0ba902b7"
          , sampled: false
          , tracestate: []
          }
      Prop.formatTraceparent ctx `shouldEqual`
        "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-00"

  describe "tracestate" do
    it "parses a simple list of key=value pairs" do
      Prop.parseTracestate "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
        `shouldEqual`
          [ Tuple "rojo" "00f067aa0ba902b7"
          , Tuple "congo" "t61rcWkgMzE"
          ]

    it "trims whitespace around entries" do
      Prop.parseTracestate "  rojo = abc , foo = bar  "
        `shouldEqual`
          [ Tuple "rojo" "abc"
          , Tuple "foo" "bar"
          ]

    it "drops entries without =" do
      Prop.parseTracestate "rojo=abc,broken,foo=bar"
        `shouldEqual`
          [ Tuple "rojo" "abc"
          , Tuple "foo" "bar"
          ]

    it "round-trips through formatTracestate" do
      let
        original =
          [ Tuple "rojo" "00f067aa0ba902b7"
          , Tuple "congo" "t61rcWkgMzE"
          ]
      Prop.parseTracestate (Prop.formatTracestate original)
        `shouldEqual` original

  describe "newTraceId / newSpanId" do
    it "produces 32-hex-char trace ids" do
      tid <- liftEffect Prop.newTraceId
      CU.length tid `shouldEqual` 32

    it "produces 16-hex-char span ids" do
      sid <- liftEffect Prop.newSpanId
      CU.length sid `shouldEqual` 16

    it "parseTraceparent accepts freshly generated ids" do
      tid <- liftEffect Prop.newTraceId
      sid <- liftEffect Prop.newSpanId
      let header = "00-" <> tid <> "-" <> sid <> "-01"
      case Prop.parseTraceparent header of
        Just _ -> pure unit
        Nothing -> fail
          ("freshly generated header was rejected: " <> header)
