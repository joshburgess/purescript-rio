-- | A `Tracer` service plus a noop implementation.
-- |
-- | `withSpan` wraps a region of `RIO` work in a named span: the span
-- | opens before the action runs, closes after it ends, and records
-- | the outcome (`SpanOk` on success, `SpanFailed` on typed failure,
-- | `SpanInterrupted` on a fiber kill before the action completed).
-- | `addAttribute` attaches a string key/value to the currently
-- | active span.
-- |
-- | The Tracer service is responsible for tracking which span is
-- | "current": `startSpan` makes the new span a child of the current
-- | one, then sets the current pointer to the new span; `endSpan`
-- | restores the current pointer to the new span's parent. This works
-- | correctly for sequential code. A `fork`ed fiber inherits whichever
-- | span was current at the point of fork (because the same Tracer is
-- | shared by the underlying `Effect.Ref`s); after the fork, the
-- | parent fiber's subsequent spans land under the same parent until
-- | the parent's `endSpan` runs. This is the standard "implicit
-- | context" model used by OTel-style tracers in single-threaded
-- | runtimes; if you need fully explicit parent context, capture
-- | `currentSpan` at the fork point and reattach manually.
-- |
-- | Test backend: `RIO.Test.Tracer.newRecordingTracer` returns a
-- | tracer that captures every span into an in-memory list, plus a
-- | `snapshot` action that returns them.
module RIO.Tracer
  ( Span
  , SpanId(..)
  , SpanStatus(..)
  , Tracer
  , addAttribute
  , currentSpan
  , noopTracer
  , withSpan
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (finally)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Record (get) as Record
import Type.Proxy (Proxy(..))

import RIO.Core (ask)
import RIO.Internal (RIO(..), unRIO)

-- | A span identifier. Unique within a `Tracer`'s lifetime.
newtype SpanId = SpanId Int

derive newtype instance eqSpanId :: Eq SpanId
derive newtype instance ordSpanId :: Ord SpanId
derive newtype instance showSpanId :: Show SpanId

-- | The terminal outcome of a span. `SpanOk` is the happy path;
-- | `SpanFailed` means the wrapped action raised a typed failure;
-- | `SpanInterrupted` means the fiber was killed before the action
-- | completed (`withSpan` could not observe a success or failure
-- | because the parent never returned).
data SpanStatus
  = SpanOk
  | SpanFailed
  | SpanInterrupted

derive instance eqSpanStatus :: Eq SpanStatus

instance showSpanStatus :: Show SpanStatus where
  show = case _ of
    SpanOk -> "SpanOk"
    SpanFailed -> "SpanFailed"
    SpanInterrupted -> "SpanInterrupted"

-- | A recorded span. `endMs` is `Nothing` while the span is still
-- | open; a recording backend may surface mid-flight spans this way
-- | for inspection.
type Span =
  { id :: SpanId
  , parent :: Maybe SpanId
  , name :: String
  , startMs :: Number
  , endMs :: Maybe Number
  , status :: SpanStatus
  , attributes :: Array (Tuple String String)
  }

-- | The service record. `startSpan` opens a span as a child of the
-- | tracer's currently-active span (if any) and makes the new span
-- | active. `endSpan` closes it and restores the previous active
-- | span. `addAttribute` attaches a string key/value to an open span.
-- | `currentSpan` reports the active span, used by `withSpan`'s
-- | bracket logic and by `addAttribute` to find a target.
type Tracer =
  { startSpan :: String -> Effect SpanId
  , endSpan :: SpanId -> SpanStatus -> Effect Unit
  , addAttribute :: SpanId -> String -> String -> Effect Unit
  , currentSpan :: Effect (Maybe SpanId)
  }

-- | A tracer that records nothing. Useful at the top of an
-- | application that does not want tracing in production, or in
-- | tests that need the row to be satisfied but don't care about
-- | the spans.
noopTracer :: Tracer
noopTracer =
  { startSpan: \_ -> pure (SpanId 0)
  , endSpan: \_ _ -> pure unit
  , addAttribute: \_ _ _ -> pure unit
  , currentSpan: pure Nothing
  }

-- | Wrap an action in a span.
-- |
-- | The span opens immediately, the action runs, and the span closes
-- | with `SpanOk` on success, `SpanFailed` on typed failure, or
-- | `SpanInterrupted` if the fiber is killed before the action
-- | completes. The close is guaranteed by `Aff.finally`, so the span
-- | is closed even on a kill that lands inside the action.
-- |
-- | ```purescript
-- | handleRequest req = withSpan "handle-request" do
-- |   addAttribute "request.id" (show req.id)
-- |   ...
-- | ```
withSpan
  :: forall r e a
   . String
  -> RIO (tracer :: Tracer | r) e a
  -> RIO (tracer :: Tracer | r) e a
withSpan name action = RIO \r -> do
  let tracer = Record.get (Proxy :: Proxy "tracer") r
  spanId <- liftEffect (tracer.startSpan name)
  endedRef <- liftEffect (Ref.new false)
  let
    finalize :: SpanStatus -> Effect Unit
    finalize status = do
      ended <- Ref.read endedRef
      when (not ended) do
        tracer.endSpan spanId status
        Ref.write true endedRef
  finally
    (liftEffect (finalize SpanInterrupted))
    do
      result <- unRIO action r
      liftEffect case result of
        Right _ -> finalize SpanOk
        Left _ -> finalize SpanFailed
      pure result

-- | Attach a string attribute to the currently-active span. A no-op
-- | when no span is active (e.g. when called outside a `withSpan`
-- | region or under `noopTracer`).
addAttribute
  :: forall r e
   . String
  -> String
  -> RIO (tracer :: Tracer | r) e Unit
addAttribute key value = do
  tracer <- ask (Proxy :: Proxy "tracer")
  liftAff do
    cur <- liftEffect tracer.currentSpan
    case cur of
      Nothing -> pure unit
      Just sid -> liftEffect (tracer.addAttribute sid key value)

-- | The currently-active span, if any. Useful when you need to
-- | snapshot the current context (for instance, before forking a
-- | fiber that should run under a known parent).
currentSpan
  :: forall r e
   . RIO (tracer :: Tracer | r) e (Maybe SpanId)
currentSpan = do
  tracer <- ask (Proxy :: Proxy "tracer")
  liftAff (liftEffect tracer.currentSpan)
