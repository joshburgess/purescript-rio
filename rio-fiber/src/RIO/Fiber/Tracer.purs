-- | A swappable tracing service.
-- |
-- | Production code wraps work in `withSpan` instead of calling a
-- | tracing library directly. Tests can swap the active tracer to
-- | capture spans in a `Ref` and assert on what was recorded.
-- |
-- | A `Tracer` exposes a single primitive: `startSpan` returns a
-- | `Span` handle whose `finish` is called when the span ends. The
-- | bracketed forms (`withSpan`, `withSpanWith`) always finish the
-- | span, even on failure or interrupt.
-- |
-- | A `Span` carries five operations:
-- |
-- |   * `addAttribute` attaches a key/value tag at any time.
-- |   * `addEvent` records a timestamped event with optional
-- |     attributes (e.g. `"cache.miss"` with `{ key: "...key..." }`).
-- |   * `addLink` adds a non-parent reference to another span,
-- |     used for cross-trace correlation (e.g. a batch span that
-- |     links to the spans of the records it consumed).
-- |   * `setStatus` reports the span's outcome as `Ok`, `Error msg`,
-- |     or leaves it `Unset` (the default).
-- |   * `finish` ends the span. Idempotent at the adapter's
-- |     discretion.
-- |
-- | Each span also has a `SpanKind` decided at start time:
-- | `Internal` (the default), `Server`, `Client`, `Producer`, or
-- | `Consumer`. The kind controls how OpenTelemetry visualizes the
-- | span (server spans show up as inbound work, client spans as
-- | outbound calls, etc.) and is read by the OTel adapter.
-- |
-- | Spans nest implicitly. `withSpan` pushes the new span onto a
-- | per-fiber "current span" slot; the tracer is told the parent
-- | when starting a child. Forked fibers inherit the current span
-- | at the moment of fork (via `FiberRef`), so concurrent work
-- | started inside a span is attributed to that span until the
-- | child calls `withSpan` itself.
-- |
-- | The default tracer is a no-op so production code can be
-- | instrumented without forcing every deployment to ship a backend.
module RIO.Fiber.Tracer
  ( Tracer(..)
  , Span(..)
  , SpanId(..)
  , Attr
  , SpanKind(..)
  , SpanStatus(..)
  , StartSpanRequest
  , defaultTracer
  , startSpan
  , startSpanWith
  , finishSpan
  , spanId
  , addAttribute
  , addEvent
  , addLink
  , setStatus
  , withSpan
  , withSpanWith
  , currentSpan
  , getTracer
  , setTracer
  , withTracer
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Core (RIO, ensuring, liftEffect)
import RIO.Fiber.Ref (locally) as Ref
import RIO.Fiber.Internal (FiberRef)
import RIO.Fiber.Ref (getFiberRef, newFiberRef, setFiberRef)

-- | A key/value attribute on a span or event.
type Attr = { key :: String, value :: String }

-- | The identity a `Span` carries for cross-span reference. The
-- | string is whatever shape the producing tracer chooses: the
-- | default no-op tracer emits the empty string; the in-memory
-- | recording tracer (`RIO.Fiber.Test.Tracer`) emits its sequence
-- | number formatted as decimal; an OTLP-backed tracer emits the
-- | W3C 16-hex span identifier. Callers that need a particular
-- | shape should map at the export boundary, not here.
newtype SpanId = SpanId String

derive newtype instance eqSpanId :: Eq SpanId
derive newtype instance ordSpanId :: Ord SpanId
derive newtype instance showSpanId :: Show SpanId

-- | The role a span plays in the trace, used by OpenTelemetry-style
-- | visualizers to distinguish inbound work (`Server`), outbound
-- | calls (`Client`), and message-queue producers / consumers from
-- | regular internal work.
data SpanKind
  = Internal
  | Server
  | Client
  | Producer
  | Consumer

derive instance eqSpanKind :: Eq SpanKind

instance showSpanKind :: Show SpanKind where
  show Internal = "internal"
  show Server = "server"
  show Client = "client"
  show Producer = "producer"
  show Consumer = "consumer"

-- | The outcome of a span. `StatusUnset` is the default and means
-- | "no explicit decision"; `StatusOk` is "completed successfully";
-- | `StatusError msg` carries a human-readable failure description.
-- | OTel exporters surface the status on the span; callers that
-- | want their own conventions can also `addAttribute "status" ...`
-- | in addition to (or instead of) `setStatus`.
data SpanStatus
  = StatusUnset
  | StatusOk
  | StatusError String

derive instance eqSpanStatus :: Eq SpanStatus

instance showSpanStatus :: Show SpanStatus where
  show StatusUnset = "unset"
  show StatusOk = "ok"
  show (StatusError msg) = "error: " <> msg

-- | An opaque span handle. Implementations stash whatever state they
-- | need to attribute / finish via the closures in the record;
-- | `spanId` is the only field readers should rely on, and is the
-- | stable identity callers can use to correlate spans across
-- | `addLink` / `parent` / OTel-style exports.
newtype Span = Span
  { spanId :: SpanId
  , addAttribute :: String -> String -> Effect Unit
  , addEvent :: String -> Array Attr -> Effect Unit
  , addLink :: Span -> Effect Unit
  , setStatus :: SpanStatus -> Effect Unit
  , finish :: Effect Unit
  }

-- | Project a span's identifier. The accessor is total and pure:
-- | every `Span` is built with a `spanId`, set once at start time
-- | by the producing tracer.
spanId :: Span -> SpanId
spanId (Span s) = s.spanId

-- | What `startSpan` is told about a new span: its name, initial
-- | attributes, parent (if a span is active on the current fiber),
-- | and `SpanKind`. The tracer is the one who created the parent
-- | (if any), so it can correlate them via its own captured state.
type StartSpanRequest =
  { name :: String
  , attributes :: Array Attr
  , parent :: Maybe Span
  , kind :: SpanKind
  }

-- | A tracer implementation. `startSpan` returns a fresh span; the
-- | span's `finish` is called exactly once.
newtype Tracer = Tracer
  { startSpan :: StartSpanRequest -> Effect Span
  }

-- | The default tracer is a no-op: spans record nothing and finish
-- | quietly. Every span shares the empty `SpanId`, which is enough
-- | for code that reads the id reflectively without participating
-- | in a real trace.
defaultTracer :: Tracer
defaultTracer = Tracer
  { startSpan: \_ -> pure
      ( Span
          { spanId: SpanId ""
          , addAttribute: \_ _ -> pure unit
          , addEvent: \_ _ -> pure unit
          , addLink: \_ -> pure unit
          , setStatus: \_ -> pure unit
          , finish: pure unit
          }
      )
  }

tracerRef :: FiberRef Tracer
tracerRef = unsafePerformEffect (newFiberRef defaultTracer)

-- | Per-fiber pointer to the currently-active span. `withSpan`
-- | updates it for the duration of the body; `fork` copies it to
-- | children so concurrent work is attributed to the parent span
-- | until the child opens its own.
currentSpanRef :: FiberRef (Maybe Span)
currentSpanRef = unsafePerformEffect (newFiberRef Nothing)

-- | Start a span via the active tracer with the default
-- | `Internal` kind. The current span (if any) is passed as the
-- | parent so the tracer can stitch the trace tree. For non-internal
-- | kinds use `startSpanWith`.
startSpan
  :: forall r e
   . String
  -> Array Attr
  -> RIO r e Span
startSpan name attrs = startSpanWith
  { name, attributes: attrs, kind: Internal }

-- | Start a span via the active tracer with an explicit kind. The
-- | record form lets callers pick `Server` / `Client` / `Producer`
-- | / `Consumer` for inbound / outbound / queue work.
startSpanWith
  :: forall r e
   . { name :: String, attributes :: Array Attr, kind :: SpanKind }
  -> RIO r e Span
startSpanWith req = do
  Tracer t <- getFiberRef tracerRef
  parent <- getFiberRef currentSpanRef
  liftEffect
    ( t.startSpan
        { name: req.name
        , attributes: req.attributes
        , parent
        , kind: req.kind
        }
    )

-- | Finish the given span. Idempotent at the implementation's
-- | discretion; the default no-op tracer handles repeat calls.
finishSpan :: forall r e. Span -> RIO r e Unit
finishSpan (Span s) = liftEffect s.finish

-- | Attach a single attribute to the given span.
addAttribute :: forall r e. Span -> String -> String -> RIO r e Unit
addAttribute (Span s) k v = liftEffect (s.addAttribute k v)

-- | Record a timestamped event on the span. Events sit on a span
-- | like notes on a timeline: "cache miss happened here", "retry
-- | 3 of 5", etc. The attributes parameter is for event-specific
-- | metadata distinct from the span's own attributes.
addEvent :: forall r e. Span -> String -> Array Attr -> RIO r e Unit
addEvent (Span s) name attrs = liftEffect (s.addEvent name attrs)

-- | Add a non-parent reference from this span to another. Links
-- | are used for cross-trace correlation: a batch processor's span
-- | might link to each of the spans that produced the records it
-- | consumed, even though those spans live in different traces.
addLink :: forall r e. Span -> Span -> RIO r e Unit
addLink (Span s) target = liftEffect (s.addLink target)

-- | Set the span's status. Most callers don't need this: `withSpan`
-- | finishes the span normally on success. Use this to explicitly
-- | mark a span as `Ok` (some backends require it) or as `Error msg`
-- | when the work failed in a way the caller wants the trace to
-- | reflect even though the surrounding RIO program continues.
setStatus :: forall r e. Span -> SpanStatus -> RIO r e Unit
setStatus (Span s) status = liftEffect (s.setStatus status)

-- | Run `body` inside a span with the default `Internal` kind:
-- | starts the span before `body`, finishes it on exit (success,
-- | failure, interrupt, or defect). While `body` runs, the new
-- | span is the "current span" for this fiber; nested `withSpan`
-- | calls see it as their parent, and child fibers forked from
-- | inside the block inherit it at the point of fork.
withSpan
  :: forall r e a
   . String
  -> Array Attr
  -> (Span -> RIO r e a)
  -> RIO r e a
withSpan name attrs = withSpanWith
  { name, attributes: attrs, kind: Internal }

-- | Like `withSpan` but with an explicit `SpanKind`. Use this for
-- | inbound request handlers (`Server`), outbound HTTP / RPC calls
-- | (`Client`), or message-queue work (`Producer` / `Consumer`).
withSpanWith
  :: forall r e a
   . { name :: String, attributes :: Array Attr, kind :: SpanKind }
  -> (Span -> RIO r e a)
  -> RIO r e a
withSpanWith req body = do
  prevSpan <- getFiberRef currentSpanRef
  span <- startSpanWith req
  setFiberRef currentSpanRef (Just span)
  ensuring
    (setFiberRef currentSpanRef prevSpan *> finishSpan span)
    (body span)

-- | Read the currently-active span, if any. `Nothing` when no span
-- | is open on this fiber.
currentSpan :: forall r e. RIO r e (Maybe Span)
currentSpan = getFiberRef currentSpanRef

-- | Read the active tracer implementation.
getTracer :: forall r e. RIO r e Tracer
getTracer = getFiberRef tracerRef

-- | Replace the active tracer for the current fiber and its
-- | descendants.
setTracer :: forall r e. Tracer -> RIO r e Unit
setTracer = setFiberRef tracerRef

-- | Run `body` with `tracer` as the active tracer, restoring the
-- | previous implementation on exit.
withTracer :: forall r e a. Tracer -> RIO r e a -> RIO r e a
withTracer tracer body = Ref.locally tracerRef tracer body
