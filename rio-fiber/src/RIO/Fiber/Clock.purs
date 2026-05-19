-- | A swappable clock service.
-- |
-- | Production code reads time via `currentTime` and `currentEpoch`
-- | instead of calling `Effect.Now` directly. Tests can swap the
-- | implementation with `withClock` to advance time deterministically
-- | (e.g. via a `Ref`-backed fake), so a flaky `sleep`-driven assertion
-- | becomes a synchronous one.
-- |
-- | The default implementation reads system time via
-- | `Effect.Now.now`. The current implementation lives in a
-- | module-level `FiberRef`, so a `withClock` block scopes the
-- | override to the wrapped action and any child fibers forked from
-- | inside it; sibling fibers are untouched.
module RIO.Fiber.Clock
  ( Clock(..)
  , defaultClock
  , currentTime
  , currentEpoch
  , withClock
  , getClock
  , setClock
  ) where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Now (now) as Now
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Core (RIO, ensuring, liftEffect)
import RIO.Fiber.Internal (FiberRef)
import RIO.Fiber.Ref (getFiberRef, newFiberRef, setFiberRef)

-- | A clock implementation. Two operations: the raw `Instant` and a
-- | convenience epoch reading as `Milliseconds`. Tests can build
-- | their own via a `Ref Instant`.
newtype Clock = Clock
  { instant :: Effect Instant
  , epoch :: Effect Milliseconds
  }

-- | The default clock: delegates to `Effect.Now.now`.
defaultClock :: Clock
defaultClock = Clock
  { instant: Now.now
  , epoch: do
      i <- Now.now
      pure (unInstant i)
  }

clockRef :: FiberRef Clock
clockRef = unsafePerformEffect (newFiberRef defaultClock)

-- | Read the current instant from the active clock.
currentTime :: forall r e. RIO r e Instant
currentTime = do
  Clock c <- getFiberRef clockRef
  liftEffect c.instant

-- | Read the current epoch as `Milliseconds` from the active clock.
currentEpoch :: forall r e. RIO r e Milliseconds
currentEpoch = do
  Clock c <- getFiberRef clockRef
  liftEffect c.epoch

-- | Read the active clock implementation.
getClock :: forall r e. RIO r e Clock
getClock = getFiberRef clockRef

-- | Replace the active clock for the current fiber. Children forked
-- | from this point inherit the override; siblings forked earlier
-- | do not.
setClock :: forall r e. Clock -> RIO r e Unit
setClock = setFiberRef clockRef

-- | Run `body` with `clock` as the active clock, restoring the
-- | previous implementation on exit (success or failure).
withClock :: forall r e a. Clock -> RIO r e a -> RIO r e a
withClock clock body = do
  prev <- getClock
  setClock clock
  ensuring (setClock prev) body
