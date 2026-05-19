-- | A controllable clock backed by a `Ref`. Use it together with
-- | `Clock.withClock` to make time-based code deterministic.
-- |
-- | The clock starts at a caller-chosen instant and only moves
-- | forward when `advance` is called. `instant` and `epoch` read the
-- | current virtual time; `sleep` does not block on wall-clock time,
-- | it parks the fiber on a pending-wakeups list keyed by deadline.
-- | Calling `advance` (or `setEpoch` forward) past a sleep's
-- | deadline resumes that fiber synchronously, in insertion order
-- | for ties.
module RIO.Fiber.TestClock
  ( TestClock
  , make
  , clock
  , advance
  , setEpoch
  , readEpoch
  ) where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (instant)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Clock (Clock(..))
import RIO.Fiber.Core (RIO, liftEffect)

type Pending =
  { id :: Int
  , deadline :: Number
  , resume :: Effect Unit
  }

-- | A test clock: virtual epoch plus a list of pending sleeps.
newtype TestClock = TestClock
  { epoch :: Ref Milliseconds
  , nextId :: Ref Int
  , pending :: Ref (Array Pending)
  }

-- | Build a `TestClock` anchored at the given epoch.
make :: Milliseconds -> Effect TestClock
make start = do
  epoch <- Ref.new start
  nextId <- Ref.new 0
  pending <- Ref.new []
  pure (TestClock { epoch, nextId, pending })

-- | Project the test clock into the `Clock` service shape so it can
-- | be passed to `withClock`. `sleep` queues a wakeup; only `advance`
-- | (or a forward `setEpoch`) fires it.
clock :: TestClock -> Clock
clock (TestClock t) = Clock
  { instant: do
      ms <- Ref.read t.epoch
      case instant ms of
        Just i -> pure i
        Nothing -> throw "RIO.Fiber.TestClock: virtual epoch out of Instant range"
  , epoch: Ref.read t.epoch
  , sleep: \(Milliseconds dur) wake ->
      if dur <= 0.0 then do
        -- A zero-duration sleep must still yield, otherwise the
        -- calling fiber stays "running" through register and never
        -- gives the scheduler a chance to run forked children. Defer
        -- the wake via the JS microtask queue.
        _queueMicrotask wake
        pure (pure unit)
      else do
        Milliseconds now <- Ref.read t.epoch
        id <- Ref.modify (_ + 1) t.nextId
        let dl = now + dur
        Ref.modify_
          (\xs -> Array.snoc xs { id, deadline: dl, resume: wake })
          t.pending
        pure
          ( Ref.modify_
              (\xs -> Array.filter (\p -> p.id /= id) xs)
              t.pending
          )
  }

foreign import _queueMicrotask :: Effect Unit -> Effect Unit

-- | Move virtual time forward by `delta` and fire any pending sleeps
-- | whose deadline has been reached.
advance :: forall r e. TestClock -> Milliseconds -> RIO r e Unit
advance tc d = liftEffect (advanceEff tc d)

advanceEff :: TestClock -> Milliseconds -> Effect Unit
advanceEff (TestClock t) (Milliseconds delta) = do
  Milliseconds now <- Ref.read t.epoch
  let newNow = now + delta
  Ref.write (Milliseconds newNow) t.epoch
  fireDueEff t newNow

-- | Jump the clock to an absolute epoch and fire any pending sleeps
-- | whose deadline has now passed.
setEpoch :: forall r e. TestClock -> Milliseconds -> RIO r e Unit
setEpoch (TestClock t) (Milliseconds ms) = liftEffect do
  Ref.write (Milliseconds ms) t.epoch
  fireDueEff t ms

fireDueEff
  :: { epoch :: Ref Milliseconds
     , nextId :: Ref Int
     , pending :: Ref (Array Pending)
     }
  -> Number
  -> Effect Unit
fireDueEff t newNow = do
  pending <- Ref.read t.pending
  let parts = Array.partition (\p -> p.deadline <= newNow) pending
  Ref.write parts.no t.pending
  traverse_ (\p -> p.resume) parts.yes

-- | Read the current virtual epoch outside any clock-using context.
readEpoch :: forall r e. TestClock -> RIO r e Milliseconds
readEpoch (TestClock t) = liftEffect (Ref.read t.epoch)
