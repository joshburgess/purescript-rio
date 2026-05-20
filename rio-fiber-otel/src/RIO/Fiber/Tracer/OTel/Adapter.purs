-- | OpenTelemetry adapter for `RIO.Fiber.Tracer`.
-- |
-- | `makeOTelTracer name` wraps an `@opentelemetry/api` tracer in
-- | the fiber `Tracer` record that the rest of `rio-fiber` consumes.
-- | The call sites (`withSpan`, `addAttribute`, `currentSpan`,
-- | `startSpan`, `finishSpan`) do not change.
-- |
-- | The adapter delegates span lifecycle and attribute writes to
-- | the OTel tracer directly. The fiber `Tracer` model hands the
-- | parent `Span` opaquely on `startSpan`; when the parent was
-- | created by this adapter, the OTel handle is recovered from a
-- | private map keyed on the underlying span record's reference
-- | identity. A `Just parent` whose handle is no longer in the
-- | map (already finished, or created by a different tracer)
-- | falls back to opening a root span rather than throwing.
-- |
-- | The fiber `Tracer` API does not surface a status code, so
-- | every closed span is reported to OTel with
-- | `SpanStatusCode.OK`. Callers that need richer status can
-- | call `addAttribute` to record a status string and the
-- | exporter can pick it up.
-- |
-- | Production wiring: install `@opentelemetry/sdk-node` (or
-- | another OTel SDK) and register it at application startup
-- | before any RIO program runs. Once a real SDK is registered,
-- | `api.trace.getTracer` returns a tracer that exports spans;
-- | without an SDK the OTel API returns a no-op tracer and this
-- | adapter is silent (the `Tracer` record still satisfies the
-- | row, so program structure is unaffected).
module RIO.Fiber.Tracer.OTel.Adapter
  ( OTelSpan
  , OTelTracer
  , makeOTelTracer
  ) where

import Prelude

import Data.Array (filter, find) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Ref as Ref

import RIO.Fiber.Tracer (Span(..), StartSpanRequest, Tracer(..))

foreign import refEq :: forall a b. a -> b -> Boolean

unsafeRefEq :: forall a b. a -> b -> Boolean
unsafeRefEq = refEq

foreign import data OTelTracer :: Type
foreign import data OTelSpan :: Type

foreign import otelGetTracer :: String -> Effect OTelTracer

foreign import otelStartRootSpan
  :: OTelTracer
  -> String
  -> Array { key :: String, value :: String }
  -> Effect OTelSpan

foreign import otelStartChildSpan
  :: OTelTracer
  -> String
  -> Array { key :: String, value :: String }
  -> OTelSpan
  -> Effect OTelSpan

foreign import otelSetAttribute
  :: OTelSpan -> String -> String -> Effect Unit

foreign import otelEndSpan :: OTelSpan -> Effect Unit

-- | The opaque record inside a fiber `Span` newtype. We compare
-- | by reference identity to recover the OTel handle for a parent
-- | that the adapter previously produced.
type SpanRec =
  { addAttribute :: String -> String -> Effect Unit
  , finish :: Effect Unit
  }

-- | Build a `Tracer` that forwards every operation to an
-- | OpenTelemetry tracer named `name`. The OTel global tracer
-- | provider (configured by your SDK) decides what `name`
-- | means; the convention is your application or library name,
-- | e.g. `"my-service"` or `"rio-fiber"`.
-- |
-- | Returns an `Effect` because building the tracer touches the
-- | global OTel registry. Call it once at startup, then place
-- | the result in your runtime configuration or scope.
-- |
-- | ```purescript
-- | main = do
-- |   tracer <- makeOTelTracer "my-service"
-- |   runAffThrow (RIO.Fiber.Tracer.withTracer tracer program)
-- | ```
makeOTelTracer :: String -> Effect Tracer
makeOTelTracer name = do
  otel <- otelGetTracer name
  spansRef <- Ref.new ([] :: Array (Tuple SpanRec OTelSpan))

  let
    lookupOTel :: SpanRec -> Effect (Maybe OTelSpan)
    lookupOTel rec = do
      xs <- Ref.read spansRef
      pure (map snd (Array.find (\t -> unsafeRefEq (fst t) rec) xs))

    removeEntry :: SpanRec -> Effect Unit
    removeEntry rec =
      Ref.modify_ (Array.filter (\t -> not (unsafeRefEq (fst t) rec))) spansRef

    startSpan :: StartSpanRequest -> Effect Span
    startSpan req = do
      parentOtel <- case req.parent of
        Nothing -> pure Nothing
        Just (Span p) -> lookupOTel p
      otelSpan <- case parentOtel of
        Nothing -> otelStartRootSpan otel req.name req.attributes
        Just p -> otelStartChildSpan otel req.name req.attributes p
      let
        rec :: SpanRec
        rec =
          { addAttribute: \k v -> otelSetAttribute otelSpan k v
          , finish: do
              otelEndSpan otelSpan
              removeEntry rec
          }
      Ref.modify_ (\xs -> xs <> [ Tuple rec otelSpan ]) spansRef
      pure (Span rec)

  pure (Tracer { startSpan })
