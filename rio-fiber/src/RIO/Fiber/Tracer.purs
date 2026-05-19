-- | A swappable tracing service.
-- |
-- | Production code wraps work in `withSpan` instead of calling a
-- | tracing library directly. Tests can swap the active tracer to
-- | capture spans in a `Ref` and assert on what was recorded.
-- |
-- | A `Tracer` exposes a single primitive: `startSpan name attrs`
-- | returns a `Span` handle whose `finish` is called when the span
-- | ends. `withSpan` is the bracketed form that always finishes the
-- | span, even on failure / interrupt. Attributes are simple
-- | string-to-string pairs; richer payloads belong in user-side
-- | adapters.
-- |
-- | The default tracer is a no-op so production code can be
-- | instrumented without forcing every deployment to ship a backend.
module RIO.Fiber.Tracer
  ( Tracer(..)
  , Span(..)
  , defaultTracer
  , startSpan
  , finishSpan
  , addAttribute
  , withSpan
  , getTracer
  , setTracer
  , withTracer
  ) where

import Prelude

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

-- | A tracer implementation. `startSpan name initialAttrs` returns a
-- | fresh span; the span's `finish` is called exactly once.
newtype Tracer = Tracer
  { startSpan :: String -> Array { key :: String, value :: String } -> Effect Span
  }

-- | The default tracer is a no-op: spans record nothing and finish
-- | quietly.
defaultTracer :: Tracer
defaultTracer = Tracer
  { startSpan: \_ _ -> pure
      ( Span
          { addAttribute: \_ _ -> pure unit
          , finish: pure unit
          }
      )
  }

tracerRef :: FiberRef Tracer
tracerRef = unsafePerformEffect (newFiberRef defaultTracer)

-- | Start a span via the active tracer.
startSpan
  :: forall r e
   . String
  -> Array { key :: String, value :: String }
  -> RIO r e Span
startSpan name attrs = do
  Tracer t <- getFiberRef tracerRef
  liftEffect (t.startSpan name attrs)

-- | Finish the given span. Idempotent at the implementation's
-- | discretion; the default no-op tracer handles repeat calls.
finishSpan :: forall r e. Span -> RIO r e Unit
finishSpan (Span s) = liftEffect s.finish

-- | Attach a single attribute to the given span.
addAttribute :: forall r e. Span -> String -> String -> RIO r e Unit
addAttribute (Span s) k v = liftEffect (s.addAttribute k v)

-- | Run `body` inside a span: starts the span before `body`, finishes
-- | it on exit (success, failure, interrupt, or defect). The span
-- | handle is exposed to the body so it can attach attributes.
withSpan
  :: forall r e a
   . String
  -> Array { key :: String, value :: String }
  -> (Span -> RIO r e a)
  -> RIO r e a
withSpan name attrs body = do
  span <- startSpan name attrs
  ensuring (finishSpan span) (body span)

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
