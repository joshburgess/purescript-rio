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

import Data.Maybe (Maybe(..), isNothing)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Fiber.Aff (runAffThrow)
import RIO.Fiber.Tracer
  ( Span(..)
  , SpanKind(..)
  , Tracer(..)
  , currentSpan
  , withTracer
  )
import RIO.Fiber.Tracer.OTel.Adapter (makeOTelTracer)

spec :: Spec Unit
spec = describe "RIO.Fiber.Tracer.OTel.Adapter.makeOTelTracer" do
  it "starts a root span and finishes it without crashing" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      Span s <- t.startSpan
        { name: "root", attributes: [], parent: Nothing, kind: Internal }
      s.addAttribute "k" "v"
      s.finish

  it "opens a child span when parent is a Just produced by this tracer" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      outer <- t.startSpan
        { name: "outer", attributes: [], parent: Nothing, kind: Internal }
      Span inner <- t.startSpan
        { name: "inner", attributes: [], parent: Just outer, kind: Internal }
      inner.finish
      let (Span o) = outer
      o.finish

  it "falls back to a root span when the parent's handle is no longer tracked" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      outer <- t.startSpan
        { name: "outer", attributes: [], parent: Nothing, kind: Internal }
      let (Span o) = outer
      o.finish
      -- outer is finished, so its handle has been removed.
      -- startSpan with this stale parent falls back to a root.
      Span inner <- t.startSpan
        { name: "inner", attributes: [], parent: Just outer, kind: Internal }
      inner.finish

  it "is safe to finish the same span twice" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      Span s <- t.startSpan
        { name: "twice", attributes: [], parent: Nothing, kind: Internal }
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
        , kind: Internal
        }
      s.finish

  it "addAttribute after finish is safe (no crash)" do
    Tracer t <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    liftEffect do
      Span s <- t.startSpan
        { name: "finished-first", attributes: [], parent: Nothing, kind: Internal }
      s.finish
      s.addAttribute "after.finish" "ignored"

  it "two tracer instances do not share parent-lookup state" do
    Tracer a <- liftEffect (makeOTelTracer "rio-fiber-otel-test-a")
    Tracer b <- liftEffect (makeOTelTracer "rio-fiber-otel-test-b")
    liftEffect do
      aParent <- a.startSpan
        { name: "a-parent", attributes: [], parent: Nothing, kind: Internal }
      -- Passing tracer A's parent to tracer B should fall back to a
      -- root span rather than crash; tracer B has never seen this
      -- handle.
      Span bChild <- b.startSpan
        { name: "b-child", attributes: [], parent: Just aParent, kind: Internal }
      bChild.finish
      let (Span aRec) = aParent
      aRec.finish

  it "currentSpan in RIO is Nothing outside withSpan even with the OTel tracer installed" do
    tracer <- liftEffect (makeOTelTracer "rio-fiber-otel-test")
    seen <- runAffThrow (withTracer tracer currentSpan)
    isNothing seen `shouldEqual` true
