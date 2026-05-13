-- | OpenTelemetry adapter for `RIO.Tracer`.
-- |
-- | `makeOTelTracer name` wraps an `@opentelemetry/api` tracer in
-- | the `Tracer` record that the rest of `rio` consumes. The
-- | call sites (`withSpan`, `addAttribute`, `currentSpan`) do
-- | not change.
-- |
-- | The adapter delegates span lifecycle and attribute writes to
-- | the OTel tracer directly. Parent / child relationships are
-- | preserved by walking the active span stack the same way
-- | `RIO.Test.Tracer` does: the latest open span is the parent
-- | of any new span started while it is active. Closing a span
-- | restores the previous active span. Span status maps to
-- | `SpanStatusCode.OK` for `SpanOk` and `SpanStatusCode.ERROR`
-- | for both `SpanFailed` and `SpanInterrupted`; the interrupted
-- | case additionally sets a `"interrupted"` status message so
-- | the cause survives into the exporter.
-- |
-- | Production wiring: install `@opentelemetry/sdk-node` (or
-- | another OTel SDK) and register it at application startup
-- | before any RIO program runs. Once a real SDK is registered,
-- | `api.trace.getTracer` returns a tracer that exports spans;
-- | without an SDK the OTel API returns a no-op tracer and this
-- | adapter is silent (the `Tracer` record still satisfies the
-- | row, so program structure is unaffected).
module RIO.Tracer.OTel
  ( OTelSpan
  , OTelTracer
  , makeOTelTracer
  ) where

import Prelude

import Data.Array (cons, filter, head) as Array
import Data.Map (Map)
import Data.Map (delete, empty, insert, lookup) as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref

import RIO.Tracer (SpanId(..), SpanStatus(..), Tracer)

foreign import data OTelTracer :: Type
foreign import data OTelSpan :: Type

foreign import otelGetTracer :: String -> Effect OTelTracer

foreign import otelStartRootSpan
  :: OTelTracer -> String -> Effect OTelSpan

foreign import otelStartChildSpan
  :: OTelTracer -> String -> OTelSpan -> Effect OTelSpan

foreign import otelSetAttribute
  :: OTelSpan -> String -> String -> Effect Unit

foreign import otelEndSpanOk :: OTelSpan -> Effect Unit

foreign import otelEndSpanError :: OTelSpan -> Effect Unit

foreign import otelEndSpanInterrupted :: OTelSpan -> Effect Unit

-- | Build a `Tracer` that forwards every operation to an
-- | OpenTelemetry tracer named `name`. The OTel global tracer
-- | provider (configured by your SDK) decides what `name`
-- | means; the convention is your application or library name,
-- | e.g. `"my-service"` or `"rio"`.
-- |
-- | Returns an `Effect` because building the tracer touches the
-- | global OTel registry. Call it once at startup, then place
-- | the result in your environment record like any other
-- | service.
-- |
-- | ```purescript
-- | main = launchAff_ do
-- |   tracer <- liftEffect (makeOTelTracer "my-service")
-- |   runRIO' (provideAll { tracer } program)
-- | ```
makeOTelTracer :: String -> Effect Tracer
makeOTelTracer name = do
  otel <- otelGetTracer name
  counterRef <- Ref.new 0
  spansRef <- Ref.new (Map.empty :: Map Int OTelSpan)
  stackRef <- Ref.new ([] :: Array Int)

  let
    nextId :: Effect Int
    nextId = do
      n <- Ref.read counterRef
      let n' = n + 1
      Ref.write n' counterRef
      pure n'

    parentSpan :: Effect (Maybe OTelSpan)
    parentSpan = do
      stack <- Ref.read stackRef
      case Array.head stack of
        Nothing -> pure Nothing
        Just parentId -> do
          spans <- Ref.read spansRef
          pure (Map.lookup parentId spans)

    startSpan :: String -> Effect SpanId
    startSpan spanName = do
      n <- nextId
      mp <- parentSpan
      otelSpan <- case mp of
        Nothing -> otelStartRootSpan otel spanName
        Just p -> otelStartChildSpan otel spanName p
      Ref.modify_ (Map.insert n otelSpan) spansRef
      Ref.modify_ (Array.cons n) stackRef
      pure (SpanId n)

    endSpan :: SpanId -> SpanStatus -> Effect Unit
    endSpan (SpanId n) status = do
      spans <- Ref.read spansRef
      case Map.lookup n spans of
        Nothing -> pure unit
        Just otelSpan -> do
          case status of
            SpanOk -> otelEndSpanOk otelSpan
            SpanFailed -> otelEndSpanError otelSpan
            SpanInterrupted -> otelEndSpanInterrupted otelSpan
          Ref.modify_ (Map.delete n) spansRef
          Ref.modify_ (Array.filter (_ /= n)) stackRef

    addAttribute :: SpanId -> String -> String -> Effect Unit
    addAttribute (SpanId n) key value = do
      spans <- Ref.read spansRef
      case Map.lookup n spans of
        Nothing -> pure unit
        Just otelSpan -> otelSetAttribute otelSpan key value

    currentSpan :: Effect (Maybe SpanId)
    currentSpan = do
      stack <- Ref.read stackRef
      pure (map SpanId (Array.head stack))

  pure
    { startSpan
    , endSpan
    , addAttribute
    , currentSpan
    }
