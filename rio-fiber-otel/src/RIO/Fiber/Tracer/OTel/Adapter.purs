-- | OpenTelemetry adapter for `RIO.Fiber.Tracer`.
-- |
-- | `makeOTelTracer name` wraps an `@opentelemetry/api` tracer in
-- | the fiber `Tracer` record that the rest of `rio-fiber` consumes.
-- | Call sites that work with the fiber `Tracer` API (`withSpan`,
-- | `addAttribute`, `addEvent`, `addLink`, `setStatus`,
-- | `currentSpan`, `startSpan`, `finishSpan`) work against the OTel
-- | tracer transparently.
-- |
-- | The adapter delegates span lifecycle, attributes, events, and
-- | status to the OTel tracer directly. The fiber `Tracer` model
-- | hands the parent `Span` opaquely on `startSpan`; when the
-- | parent was created by this adapter, the OTel handle is
-- | recovered from a private map keyed on the underlying span
-- | record's reference identity. A `Just parent` whose handle is
-- | no longer in the map (already finished, or created by a
-- | different tracer) falls back to opening a root span rather than
-- | throwing.
-- |
-- | `SpanKind` is forwarded to OTel as the OTel `SpanKind` (Server
-- | / Client / Producer / Consumer / Internal) at span start.
-- | `addLink` after start time attaches the linked span's
-- | trace/span IDs as attributes, since the OTel JS API only
-- | accepts links at construction time. `setStatus` maps to
-- | `SpanStatusCode.OK` / `ERROR` / `UNSET`; if `setStatus` is
-- | never called the span finishes with the default `UNSET`,
-- | which most exporters treat as implicit OK.
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

import RIO.Fiber.Tracer
  ( Span(..)
  , SpanKind(..)
  , SpanStatus(..)
  , StartSpanRequest
  , Tracer(..)
  )

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
  -> String
  -> Effect OTelSpan

foreign import otelStartChildSpan
  :: OTelTracer
  -> String
  -> Array { key :: String, value :: String }
  -> String
  -> OTelSpan
  -> Effect OTelSpan

foreign import otelSetAttribute
  :: OTelSpan -> String -> String -> Effect Unit

foreign import otelAddEvent
  :: OTelSpan
  -> String
  -> Array { key :: String, value :: String }
  -> Effect Unit

foreign import otelAddLink :: OTelSpan -> OTelSpan -> Effect Unit

foreign import otelSetStatusOk :: OTelSpan -> Effect Unit
foreign import otelSetStatusError :: OTelSpan -> String -> Effect Unit
foreign import otelSetStatusUnset :: OTelSpan -> Effect Unit

foreign import otelEndSpan :: OTelSpan -> Effect Unit

-- | The opaque record inside a fiber `Span` newtype. We compare
-- | by reference identity to recover the OTel handle for a parent
-- | that the adapter previously produced.
type SpanRec =
  { addAttribute :: String -> String -> Effect Unit
  , addEvent :: String -> Array { key :: String, value :: String } -> Effect Unit
  , addLink :: Span -> Effect Unit
  , setStatus :: SpanStatus -> Effect Unit
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

    kindTag :: SpanKind -> String
    kindTag = case _ of
      Internal -> "Internal"
      Server -> "Server"
      Client -> "Client"
      Producer -> "Producer"
      Consumer -> "Consumer"

    startSpan :: StartSpanRequest -> Effect Span
    startSpan req = do
      let tag = kindTag req.kind
      parentOtel <- case req.parent of
        Nothing -> pure Nothing
        Just (Span p) -> lookupOTel p
      otelSpan <- case parentOtel of
        Nothing -> otelStartRootSpan otel req.name req.attributes tag
        Just p -> otelStartChildSpan otel req.name req.attributes tag p

      let
        rec :: SpanRec
        rec =
          { addAttribute: \k v -> otelSetAttribute otelSpan k v
          , addEvent: \evName evAttrs -> otelAddEvent otelSpan evName evAttrs
          , addLink: \(Span linkSpan) -> do
              mTarget <- lookupOTel linkSpan
              case mTarget of
                Just target -> otelAddLink otelSpan target
                Nothing -> pure unit
          , setStatus: \status -> case status of
              StatusOk -> otelSetStatusOk otelSpan
              StatusError msg -> otelSetStatusError otelSpan msg
              StatusUnset -> otelSetStatusUnset otelSpan
          , finish: do
              otelEndSpan otelSpan
              removeEntry rec
          }
      Ref.modify_ (\xs -> xs <> [ Tuple rec otelSpan ]) spansRef
      pure (Span rec)

  pure (Tracer { startSpan })
