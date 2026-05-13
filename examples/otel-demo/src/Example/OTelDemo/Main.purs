-- | A small program that wires `RIO.Tracer.OTel.makeOTelTracer`
-- | into a real OpenTelemetry SDK with an in-memory exporter
-- | (`InMemorySpanExporter` from `@opentelemetry/sdk-trace-base`).
-- |
-- | The program runs a nested-span workload through the same
-- | `RIO.Tracer` API the rest of `rio` uses, then dumps the
-- | captured spans so you can see that parent / child
-- | relationships, attributes, and status codes all survived
-- | the OTel round-trip.
-- |
-- | Run with:
-- |
-- |   npx spago run -p rio-example-otel-demo
module Example.OTelDemo.Main
  ( main
  ) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Example.OTelDemo.InMemoryExporter
  ( ExportedSpan
  , installInMemoryExporter
  , readExportedSpans
  )
import RIO.Core
  ( RIO
  , catchAll
  , fail
  , provideAll
  , runRIO'
  )
import RIO.Tracer (Tracer, addAttribute, withSpan)
import RIO.Tracer.OTel (makeOTelTracer)
import Type.Proxy (Proxy(..))

work :: forall r e. RIO (tracer :: Tracer | r) e Unit
work = withSpan "outer" do
  addAttribute "outer.kind" "demo"
  withSpan "inner-a" do
    addAttribute "inner-a.work" "small"
    pure unit
  void
    $ catchAll (\_ -> pure unit)
    $ withSpan "inner-b-fails" do
        addAttribute "inner-b.attempted" "true"
        fail (Proxy :: Proxy "badThing") "boom"

main :: Effect Unit
main = launchAff_ do
  liftEffect (installInMemoryExporter "rio-otel-demo")
  tracer <- liftEffect (makeOTelTracer "rio-otel-demo")
  _ <- runRIO' (provideAll { tracer } work)
  spans <- liftEffect readExportedSpans
  liftEffect do
    log ""
    log "captured spans (parent first, attributes preserved):"
    log "---------------------------------------------------"
    renderSpans spans

renderSpans :: Array ExportedSpan -> Effect Unit
renderSpans spans = case spans of
  [] -> log "  (none -- exporter did not flush)"
  _ -> for_ spans \s -> do
    log
      ( "  "
          <> s.name
          <> "  status="
          <> s.status
          <> (if s.parentId == "" then "" else "  parent=" <> s.parentId)
      )
    for_ s.attributes \a ->
      log ("      " <> a.key <> " = " <> a.value)
