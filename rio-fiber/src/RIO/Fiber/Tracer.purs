-- | A swappable tracing service.
-- |
-- | Production code wraps work in `withSpan` instead of calling a
-- | tracing library directly. Tests can swap the active tracer to
-- | capture spans in a `Ref` and assert on what was recorded.
-- |
-- | A `Tracer` exposes a single primitive: `startSpan` returns a
-- | `Span` handle whose `finish` is called when the span ends.
-- | `withSpan` is the bracketed form that always finishes the span,
-- | even on failure / interrupt. Attributes are simple string-to-
-- | string pairs; richer payloads belong in user-side adapters.
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
  , StartSpanRequest
  , defaultTracer
  , startSpan
  , finishSpan
  , addAttribute
  , withSpan
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
import RIO.Fiber.Internal (FiberRef)
import RIO.Fiber.Ref (getFiberRef, newFiberRef, setFiberRef)

-- | An opaque span handle. Implementations stash whatever state they
-- | need to attribute / finish via the closures in the record.
newtype Span = Span
  { addAttribute :: String -> String -> Effect Unit
  , finish :: Effect Unit
  }

-- | What `startSpan` is told about a new span: its name, initial
-- | attributes, and the parent if one is active. The tracer is the
-- | one who created the parent (if any), so it can correlate them
-- | via its own captured state.
type StartSpanRequest =
  { name :: String
  , attributes :: Array { key :: String, value :: String }
  , parent :: Maybe Span
  }

-- | A tracer implementation. `startSpan` returns a fresh span; the
-- | span's `finish` is called exactly once.
newtype Tracer = Tracer
  { startSpan :: StartSpanRequest -> Effect Span
  }

-- | The default tracer is a no-op: spans record nothing and finish
-- | quietly.
defaultTracer :: Tracer
defaultTracer = Tracer
  { startSpan: \_ -> pure
      ( Span
          { addAttribute: \_ _ -> pure unit
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

-- | Start a span via the active tracer. The current span (if any)
-- | is passed as the parent so the tracer can stitch the trace tree.
startSpan
  :: forall r e
   . String
  -> Array { key :: String, value :: String }
  -> RIO r e Span
startSpan name attrs = do
  Tracer t <- getFiberRef tracerRef
  parent <- getFiberRef currentSpanRef
  liftEffect (t.startSpan { name, attributes: attrs, parent })

-- | Finish the given span. Idempotent at the implementation's
-- | discretion; the default no-op tracer handles repeat calls.
finishSpan :: forall r e. Span -> RIO r e Unit
finishSpan (Span s) = liftEffect s.finish

-- | Attach a single attribute to the given span.
addAttribute :: forall r e. Span -> String -> String -> RIO r e Unit
addAttribute (Span s) k v = liftEffect (s.addAttribute k v)

-- | Run `body` inside a span: starts the span before `body`, finishes
-- | it on exit (success, failure, interrupt, or defect). While
-- | `body` runs, the new span is the "current span" for this fiber;
-- | nested `withSpan` calls see it as their parent, and child fibers
-- | forked from inside the block inherit it at the point of fork.
withSpan
  :: forall r e a
   . String
  -> Array { key :: String, value :: String }
  -> (Span -> RIO r e a)
  -> RIO r e a
withSpan name attrs body = do
  prevSpan <- getFiberRef currentSpanRef
  span <- startSpan name attrs
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
withTracer tracer body = do
  prev <- getTracer
  setTracer tracer
  ensuring (setTracer prev) body
