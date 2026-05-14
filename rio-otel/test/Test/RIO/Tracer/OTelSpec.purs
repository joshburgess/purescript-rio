-- | Unit tests for the rio-otel adapter.
-- |
-- | These tests run against the no-op tracer that
-- | `@opentelemetry/api` returns when no SDK is registered. Span
-- | export is not observable in this configuration; what is
-- | observable is the adapter's own bookkeeping (counter, parent
-- | stack, span map). That is exactly what these tests pin down:
-- | id allocation, parent/child reporting via `currentSpan`,
-- | stack pop on `endSpan`, and `addAttribute` safety against
-- | already-closed or unknown span ids. End-to-end OTel
-- | round-trips (with a real SDK + in-memory exporter) live in
-- | `examples/otel-demo/`.
module Test.RIO.Tracer.OTelSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Tracer (SpanId(..), SpanStatus(..))
import RIO.Tracer.OTel (makeOTelTracer)

spec :: Spec Unit
spec = describe "RIO.Tracer.OTel.makeOTelTracer" do
  it "allocates sequential SpanIds starting at 1" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan "a")
    b <- liftEffect (tracer.startSpan "b")
    c <- liftEffect (tracer.startSpan "c")
    a `shouldEqual` SpanId 1
    b `shouldEqual` SpanId 2
    c `shouldEqual` SpanId 3

  it "reports Nothing as the current span before any startSpan" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Nothing

  it "reports the most recently opened span as current" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    _outer <- liftEffect (tracer.startSpan "outer")
    inner <- liftEffect (tracer.startSpan "inner")
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Just inner

  it "restores the parent as current after endSpan" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    outer <- liftEffect (tracer.startSpan "outer")
    inner <- liftEffect (tracer.startSpan "inner")
    liftEffect (tracer.endSpan inner SpanOk)
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Just outer

  it "reports Nothing as current once every open span has ended" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan "a")
    b <- liftEffect (tracer.startSpan "b")
    liftEffect (tracer.endSpan b SpanOk)
    liftEffect (tracer.endSpan a SpanOk)
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Nothing

  it "treats endSpan of a non-current span as removing it from the stack" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan "a")
    _b <- liftEffect (tracer.startSpan "b")
    liftEffect (tracer.endSpan a SpanFailed)
    -- The most recently opened still-open span (b) becomes the
    -- current pointer; a has been filtered out of the stack.
    cur <- liftEffect tracer.currentSpan
    case cur of
      Just (SpanId 2) -> pure unit
      _ -> shouldEqual cur (Just (SpanId 2))

  it "treats endSpan of an unknown SpanId as a no-op" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan "a")
    liftEffect (tracer.endSpan (SpanId 999) SpanOk)
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Just a

  it "treats endSpan of an already-closed SpanId as a no-op" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan "a")
    liftEffect (tracer.endSpan a SpanOk)
    liftEffect (tracer.endSpan a SpanOk)
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Nothing

  it "accepts addAttribute on the current span without crashing" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan "with-attrs")
    liftEffect (tracer.addAttribute a "k" "v")
    -- The OTel api package returns a no-op tracer when no SDK
    -- is registered, so we only verify the call did not throw
    -- and that the span is still the current one.
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Just a

  it "treats addAttribute on an unknown SpanId as a no-op" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    liftEffect (tracer.addAttribute (SpanId 999) "k" "v")
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Nothing

  it "treats addAttribute on an already-closed SpanId as a no-op" do
    -- This module's docstring promises "addAttribute safety
    -- against already-closed or unknown span ids". The unknown
    -- case is pinned above; pin the already-closed case here so
    -- a future change that, say, retains closed spans in the
    -- internal map and forwards attribute writes to a finalized
    -- OTel span (which would throw at runtime) is caught.
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan "soon-closed")
    liftEffect (tracer.endSpan a SpanOk)
    liftEffect (tracer.addAttribute a "k" "v")
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Nothing

  it "treats endSpan with SpanInterrupted the same as other statuses for bookkeeping" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    outer <- liftEffect (tracer.startSpan "outer")
    inner <- liftEffect (tracer.startSpan "inner")
    liftEffect (tracer.endSpan inner SpanInterrupted)
    cur <- liftEffect tracer.currentSpan
    cur `shouldEqual` Just outer

  it "gives each fresh tracer its own counter" do
    tracerA <- liftEffect (makeOTelTracer "rio-otel-test-a")
    tracerB <- liftEffect (makeOTelTracer "rio-otel-test-b")
    a1 <- liftEffect (tracerA.startSpan "x")
    b1 <- liftEffect (tracerB.startSpan "x")
    a1 `shouldEqual` SpanId 1
    b1 `shouldEqual` SpanId 1
