-- | A `Tracer` service plus a noop implementation.
-- |
-- | `withSpan` wraps a region of `RIO` work in a named span: the span
-- | opens before the action runs, closes after it ends, and records
-- | the outcome (`SpanOk` on success, `SpanFailed` on typed failure,
-- | `SpanInterrupted` on a fiber kill before the action completed).
-- | `addAttribute` attaches a string key/value to the currently
-- | active span.
-- |
-- | "Current span" is per-fiber: each `withSpan` block allocates
-- | a private cell holding the span it opened, swaps the tracer's
-- | `currentSpan` callback to read from that cell, and runs its
-- | inner action against the swapped environment record.
-- | `startSpan` takes the parent span explicitly, so the backend
-- | never has to track who is the current span for whom.
-- |
-- | The consequence: a `fork`ed fiber captures the swapped
-- | environment at the fork point and keeps seeing its parent's
-- | span as the active parent for as long as it lives, regardless
-- | of what subsequent `withSpan`s the parent enters. Concurrent
-- | parent and child spans cannot clobber each other's view.
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
import RIO.Internal (RIO(..), rioFail, unRIO)

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

-- | The service record. `startSpan` opens a span as a child of
-- | the supplied `parent` (or as a root span when `parent` is
-- | `Nothing`). `endSpan` closes it. `addAttribute` attaches a
-- | string key/value to an open span. `currentSpan` reports the
-- | span the *calling fiber* sees as active; `withSpan` swaps in
-- | a per-block view of this callback so that forked fibers
-- | inherit the snapshot at fork time rather than racing the
-- | parent's later span activity.
-- |
-- | Backends do not need to track current-span state themselves:
-- | `withSpan` owns that, threads explicit parents into
-- | `startSpan`, and overrides `currentSpan` per block via an
-- | environment-record swap. A bare backend's `currentSpan` may
-- | safely return `Nothing`.
type Tracer =
  { startSpan :: { name :: String, parent :: Maybe SpanId } -> Effect SpanId
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
-- | The span opens immediately as a child of whatever
-- | `currentSpan` reports for the *calling fiber*, the action
-- | runs, and the span closes with `SpanOk` on success,
-- | `SpanFailed` on typed failure, or `SpanInterrupted` if the
-- | fiber is killed before the action completes. The close is
-- | guaranteed by `Aff.finally`, so the span is closed even on a
-- | kill that lands inside the action.
-- |
-- | Per-fiber "current span" is owned by `withSpan` itself: each
-- | block allocates a private `Ref`, swaps the tracer's
-- | `currentSpan` callback to read from that `Ref`, and runs
-- | `action` against the swapped environment. A fiber forked
-- | inside the block captures this private cell and keeps seeing
-- | the snapshot regardless of what the parent's surrounding
-- | spans do; concurrent `withSpan`s in parent and child cannot
-- | clobber each other's view.
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
  parent <- liftEffect tracer.currentSpan
  spanId <- liftEffect (tracer.startSpan { name, parent })
  privateRef <- liftEffect (Ref.new (Just spanId))
  let
    scopedTracer = tracer
      { currentSpan = Ref.read privateRef
      }
    r' = r { tracer = scopedTracer }
  endedRef <- liftEffect (Ref.new false)
  let
    finalize :: SpanStatus -> Effect Unit
    finalize status = do
      ended <- Ref.read endedRef
      when (not ended) do
        tracer.endSpan spanId status
        Ref.write Nothing privateRef
        Ref.write true endedRef
  finally
    (liftEffect (finalize SpanInterrupted))
    do
      result <- unRIO action r'
      liftEffect case result of
        Right _ -> finalize SpanOk
        Left _ -> finalize SpanFailed
      case result of
        Right a -> pure a
        Left v -> rioFail v

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
