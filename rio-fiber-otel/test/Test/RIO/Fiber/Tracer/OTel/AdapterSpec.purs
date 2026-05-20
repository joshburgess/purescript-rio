-- | Unit tests for the rio-fiber-otel adapter.
-- |
-- | These tests run against the no-op tracer that
-- | `@opentelemetry/api` returns when no SDK is registered. Span
-- | export is not observable in this configuration; what is
-- | observable is that the adapter constructs `Span` values via
-- | the fiber `Tracer` contract and that finishing a span does
-- | not crash. End-to-end OTel round-trips (with a real SDK +
-- | in-memory exporter) live in `examples/otel-demo/`.
module Test.RIO.Fiber.Tracer.OTel.AdapterSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)

import RIO.Fiber.Tracer (Span(..), Tracer(..))
import RIO.Fiber.Tracer.OTel.Adapter (makeOTelTracer)

spec :: Spec Unit
spec = describe "RIO.Fiber.Tracer.OTel.Adapter.makeOTelTracer" do
  it "starts a root span and finishes it without crashing" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      Span s <- t.startSpan
        { name: "root", attributes: [], parent: Nothing }
      s.addAttribute "k" "v"
      s.finish

  it "opens a child span when parent is a Just produced by this tracer" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      outer <- t.startSpan
        { name: "outer", attributes: [], parent: Nothing }
      Span inner <- t.startSpan
        { name: "inner", attributes: [], parent: Just outer }
      inner.finish
      let (Span o) = outer
      o.finish

  it "falls back to a root span when the parent's handle is no longer tracked" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      outer <- t.startSpan
        { name: "outer", attributes: [], parent: Nothing }
      let (Span o) = outer
      o.finish
      -- outer is finished, so its handle has been removed.
      -- startSpan with this stale parent falls back to a root.
      Span inner <- t.startSpan
        { name: "inner", attributes: [], parent: Just outer }
      inner.finish

  it "is safe to finish the same span twice" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      Span s <- t.startSpan
        { name: "twice", attributes: [], parent: Nothing }
      s.finish
      s.finish

  it "passes initial attributes to the OTel span on startSpan" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      Span s <- t.startSpan
        { name: "with-attrs"
        , attributes:
            [ { key: "service.name", value: "test" }
            , { key: "kind", value: "internal" }
            ]
        , parent: Nothing
        }
      s.finish
