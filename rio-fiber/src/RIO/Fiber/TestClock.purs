-- | A controllable clock backed by a `Ref`. Use it together with
-- | `Clock.withClock` to make time-based code deterministic.
-- |
-- | The clock starts at a caller-chosen instant and only moves
-- | forward when `advance` is called. `instant` and `epoch` read the
-- | current virtual time; nothing happens on its own.
module RIO.Fiber.TestClock
  ( TestClock
  , make
  , clock
  , advance
  , setEpoch
  , readEpoch
  ) where

import Prelude

import Data.DateTime.Instant (instant)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Clock (Clock(..))
import RIO.Fiber.Core (RIO, liftEffect)

-- | A test clock holds the current virtual epoch in a `Ref`.
newtype TestClock = TestClock (Ref Milliseconds)

-- | Build a `TestClock` anchored at the given epoch.
make :: Milliseconds -> Effect TestClock
make start = TestClock <$> Ref.new start

-- | Project the test clock into the `Clock` service shape so it can
-- | be passed to `withClock`.
clock :: TestClock -> Clock
clock (TestClock ref) = Clock
  { instant: do
      ms <- Ref.read ref
      case instant ms of
        Just i -> pure i
        Nothing -> throw "RIO.Fiber.TestClock: virtual epoch out of Instant range"
  , epoch: Ref.read ref
  }

-- | Move virtual time forward by `delta`.
advance :: forall r e. TestClock -> Milliseconds -> RIO r e Unit
advance (TestClock ref) (Milliseconds delta) =
  liftEffect (Ref.modify_ (\(Milliseconds n) -> Milliseconds (n + delta)) ref)

-- | Jump the clock to an absolute epoch.
setEpoch :: forall r e. TestClock -> Milliseconds -> RIO r e Unit
setEpoch (TestClock ref) ms = liftEffect (Ref.write ms ref)

-- | Read the current virtual epoch outside any clock-using context.
readEpoch :: forall r e. TestClock -> RIO r e Milliseconds
readEpoch (TestClock ref) = liftEffect (Ref.read ref)
