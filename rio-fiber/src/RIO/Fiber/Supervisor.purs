-- | Fiber-lifecycle observation hooks.
-- |
-- | A `Supervisor` is a pair of callbacks the runtime invokes when a
-- | fiber starts and when it completes. The runtime maintains a
-- | single global registry; `register` adds a supervisor and returns
-- | an `Effect Unit` that removes it. The callbacks must be cheap and
-- | non-throwing: the interpreter swallows exceptions from supervisor
-- | hooks so a misbehaving observer can't crash a running fiber, but
-- | a slow hook will still slow down every fiber spawn.
-- |
-- | Typical uses: an `activeFiberCount` Ref for metrics, an
-- | append-only log for debugging, or a span open / close pair for
-- | distributed tracing.
module RIO.Fiber.Supervisor
  ( Supervisor(..)
  , FiberId
  , register
  ) where

import Prelude

import Effect (Effect)
import RIO.Fiber.Internal (_registerSupervisor) as Internal

-- | An opaque integer assigned to each fiber at construction time.
-- | Stable for the lifetime of the fiber.
type FiberId = Int

-- | A pair of lifecycle callbacks. `onStart` fires inside the fiber
-- | constructor; `onEnd` fires when the fiber transitions to the
-- | completed state, before any waiting joiners are notified.
newtype Supervisor = Supervisor
  { onStart :: FiberId -> Effect Unit
  , onEnd :: FiberId -> Effect Unit
  }

-- | Add a supervisor to the global registry. Returns an action that
-- | removes it. Multiple supervisors may be registered; they are
-- | invoked in registration order.
register :: Supervisor -> Effect (Effect Unit)
register (Supervisor s) = Internal._registerSupervisor s
