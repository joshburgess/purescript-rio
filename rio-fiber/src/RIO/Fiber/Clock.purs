-- | A swappable clock service.
-- |
-- | Production code reads time via `currentTime` and `currentEpoch`
-- | and suspends via `sleep`. Tests can swap the implementation with
-- | `withClock` to advance time deterministically (e.g. via a
-- | `Ref`-backed fake), so flaky `sleep`-driven assertions become
-- | synchronous.
-- |
-- | The default implementation reads system time via `Effect.Now`
-- | and sleeps via `setTimeout`. The current implementation lives
-- | in a module-level `FiberRef`, so a `withClock` block scopes the
-- | override to the wrapped action and any child fibers forked from
-- | inside it; sibling fibers are untouched.
module RIO.Fiber.Clock
  ( Clock(..)
  , defaultClock
  , currentTime
  , currentEpoch
  , sleep
  , withClock
  , getClock
  , setClock
  ) where

import Prelude

import Data.DateTime.Instant (Instant, unInstant)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now) as Now
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Internal (FiberRef, RIO(..))
import RIO.Fiber.Internal as Internal
import RIO.Fiber.Ref (getFiberRef, locally, newFiberRef, setFiberRef)

-- | A clock implementation. Three operations: read the raw
-- | `Instant`, read the epoch as `Milliseconds`, and suspend for a
-- | given duration. Tests can build their own via a `Ref` and a
-- | pending-wakeups list.
newtype Clock = Clock
  { instant :: Effect Instant
  , epoch :: Effect Milliseconds
  -- | Duration + a wake callback. Returns a best-effort canceller.
  , sleep :: Milliseconds -> Effect Unit -> Effect (Effect Unit)
  }

-- | The default clock: real wall time via `Effect.Now` and real
-- | `setTimeout`.
defaultClock :: Clock
defaultClock = Clock
  { instant: Now.now
  , epoch: do
      i <- Now.now
      pure (unInstant i)
  , sleep: \(Milliseconds ms) wake -> do
      id <- _setTimeout ms wake
      pure (_clearTimeout id)
  }

clockRef :: FiberRef Clock
clockRef = unsafePerformEffect (newFiberRef defaultClock)

-- | Read the current instant from the active clock.
currentTime :: forall r e. RIO r e Instant
currentTime = do
  Clock c <- getFiberRef clockRef
  RIO (Internal.opLiftEffect c.instant)

-- | Read the current epoch as `Milliseconds` from the active clock.
currentEpoch :: forall r e. RIO r e Milliseconds
currentEpoch = do
  Clock c <- getFiberRef clockRef
  RIO (Internal.opLiftEffect c.epoch)

-- | Suspend the fiber for the given duration via the active clock.
-- | The default clock uses real `setTimeout`; a virtual clock can
-- | resume the fiber synchronously when `advance` moves past the
-- | scheduled wake time.
sleep :: forall r e. Milliseconds -> RIO r e Unit
sleep ms = do
  Clock c <- getFiberRef clockRef
  RIO
    ( Internal.opAsync \onOk _onFail -> do
        cancel <- c.sleep ms (onOk unit)
        pure cancel
    )

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
withClock clock body = locally clockRef clock body

foreign import data TimeoutId :: Type
foreign import _setTimeout :: Number -> Effect Unit -> Effect TimeoutId
foreign import _clearTimeout :: TimeoutId -> Effect Unit
