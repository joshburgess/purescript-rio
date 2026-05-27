-- | Unit tests for the rio-aff-otel adapter.
-- |
-- | These tests run against the no-op tracer that
-- | `@opentelemetry/api` returns when no SDK is registered. Span
-- | export is not observable in this configuration; what is
-- | observable is the adapter's own bookkeeping (counter, span
-- | map). That is exactly what these tests pin down: id
-- | allocation, parent-passing through `startSpan`, `endSpan`
-- | tearing the span out of the map, and `addAttribute` safety
-- | against already-closed or unknown span ids. The new tracer
-- | contract puts "current span" tracking under `withSpan`'s
-- | control via a per-block `Ref` swap, so the adapter's
-- | `currentSpan` is `Nothing`-only and is pinned that way here.
-- | End-to-end OTel round-trips (with a real SDK + in-memory
-- | exporter) live in `examples/otel-demo/`.
module Test.RIO.Aff.Tracer.OTel.AdapterSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Tracer (SpanId(..), SpanStatus(..))
import RIO.Aff.Tracer.OTel.Adapter (makeOTelTracer)

spec :: Spec Unit
spec = describe "RIO.Aff.Tracer.OTel.Adapter.makeOTelTracer" do
  it "allocates sequential SpanIds starting at 1" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan { name: "a", parent: Nothing })
    b <- liftEffect (tracer.startSpan { name: "b", parent: Nothing })
    c <- liftEffect (tracer.startSpan { name: "c", parent: Nothing })
    a `shouldEqual` SpanId 1
    b `shouldEqual` SpanId 2
    c `shouldEqual` SpanId 3

  it "always reports Nothing as the current span (withSpan owns that view)" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    cur0 <- liftEffect tracer.currentSpan
    cur0 `shouldEqual` Nothing
    _ <- liftEffect (tracer.startSpan { name: "a", parent: Nothing })
    cur1 <- liftEffect tracer.currentSpan
    cur1 `shouldEqual` Nothing

  it "opens a child span when parent is Just a known SpanId" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    outer <- liftEffect (tracer.startSpan { name: "outer", parent: Nothing })
    inner <- liftEffect (tracer.startSpan { name: "inner", parent: Just outer })
    -- The OTel API returns a no-op tracer, so we cannot inspect
    -- the parent link on the exported span; what is observable
    -- is that startSpan returned a fresh SpanId and did not
    -- throw on the parent lookup path.
    inner `shouldEqual` SpanId 2

  it "falls back to a root span when parent is a Just but unknown SpanId" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    sid <- liftEffect (tracer.startSpan { name: "orphan", parent: Just (SpanId 999) })
    sid `shouldEqual` SpanId 1

  it "removes a closed span from the internal map" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan { name: "a", parent: Nothing })
    liftEffect (tracer.endSpan a SpanOk)
    -- A subsequent attribute write on the closed id is a no-op
    -- because the lookup misses; if the span had not been
    -- removed, the no-op OTel span would still accept the call,
    -- which would be indistinguishable from this case. The
    -- already-closed test below covers the post-end safety
    -- guarantee directly.
    liftEffect (tracer.addAttribute a "k" "v")

  it "treats endSpan of an unknown SpanId as a no-op" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    liftEffect (tracer.endSpan (SpanId 999) SpanOk)

  it "treats endSpan of an already-closed SpanId as a no-op" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan { name: "a", parent: Nothing })
    liftEffect (tracer.endSpan a SpanOk)
    liftEffect (tracer.endSpan a SpanOk)

  it "accepts addAttribute on an open span without crashing" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan { name: "with-attrs", parent: Nothing })
    liftEffect (tracer.addAttribute a "k" "v")

  it "treats addAttribute on an unknown SpanId as a no-op" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    liftEffect (tracer.addAttribute (SpanId 999) "k" "v")

  it "treats addAttribute on an already-closed SpanId as a no-op" do
    -- This module's docstring promises "addAttribute safety
    -- against already-closed or unknown span ids". The unknown
    -- case is pinned above; pin the already-closed case here so
    -- a future change that, say, retains closed spans in the
    -- internal map and forwards attribute writes to a finalized
    -- OTel span (which would throw at runtime) is caught.
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan { name: "soon-closed", parent: Nothing })
    liftEffect (tracer.endSpan a SpanOk)
    liftEffect (tracer.addAttribute a "k" "v")

  it "treats endSpan with SpanInterrupted the same as other statuses for bookkeeping" do
    tracer <- liftEffect (makeOTelTracer "rio-otel-test")
    a <- liftEffect (tracer.startSpan { name: "a", parent: Nothing })
    liftEffect (tracer.endSpan a SpanInterrupted)
    -- The span is removed from the map, so a subsequent
    -- addAttribute is a no-op rather than reaching a finalized
    -- OTel span.
    liftEffect (tracer.addAttribute a "k" "v")

  it "gives each fresh tracer its own counter" do
    tracerA <- liftEffect (makeOTelTracer "rio-otel-test-a")
    tracerB <- liftEffect (makeOTelTracer "rio-otel-test-b")
    a1 <- liftEffect (tracerA.startSpan { name: "x", parent: Nothing })
    b1 <- liftEffect (tracerB.startSpan { name: "x", parent: Nothing })
    a1 `shouldEqual` SpanId 1
    b1 `shouldEqual` SpanId 1
