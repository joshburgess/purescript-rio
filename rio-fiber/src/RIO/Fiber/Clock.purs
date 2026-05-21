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
  , ClockParts
  , defaultClock
  , currentTime
  , currentEpoch
  , nowParts
  , partsFromMs
  , sleep
  , timed
  , withClock
  , getClock
  , setClock
  ) where

import Prelude

import Data.Date (day, month, weekday, year) as Date
import Data.DateTime (date, time) as DT
import Data.DateTime.Instant (Instant, instant, toDateTime, unInstant)
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.Time (hour, millisecond, minute, second) as Time
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
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

-- | The decomposed wall-clock components of a moment in time,
-- | always interpreted in UTC.
-- |
-- |   * `year` is the four-digit year.
-- |   * `month` is `1..12`.
-- |   * `day` is the day of the month, `1..31`.
-- |   * `hour` is `0..23`.
-- |   * `minute` and `second` are `0..59`.
-- |   * `millisecond` is `0..999`.
-- |   * `dayOfWeek` is `1..7` with `1 = Monday` and `7 = Sunday`,
-- |     matching ISO 8601.
type ClockParts =
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  , second :: Int
  , millisecond :: Int
  , dayOfWeek :: Int
  }

-- | Decompose a millisecond-since-epoch timestamp into its UTC
-- | wall-clock components. Useful when rendering a timestamp emitted
-- | from the Clock service, or when implementing cron-shaped
-- | schedules.
-- |
-- | Returns `Nothing` if the timestamp is outside the representable
-- | range of `Data.DateTime.Instant`.
partsFromMs :: Milliseconds -> Maybe ClockParts
partsFromMs ms = do
  i <- instant ms
  let
    dt = toDateTime i
    d = DT.date dt
    t = DT.time dt
  pure
    { year: fromEnum (Date.year d)
    , month: fromEnum (Date.month d)
    , day: fromEnum (Date.day d)
    , hour: fromEnum (Time.hour t)
    , minute: fromEnum (Time.minute t)
    , second: fromEnum (Time.second t)
    , millisecond: fromEnum (Time.millisecond t)
    , dayOfWeek: fromEnum (Date.weekday d)
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

-- | Read the current wall-clock time, decomposed into UTC parts.
-- |
-- | Convenience for `currentEpoch` followed by `partsFromMs`.
-- | Falls back to the Unix epoch (1970-01-01T00:00:00Z, Thursday)
-- | if the timestamp cannot be represented as a `DateTime`, which
-- | is only possible under a mock clock whose `epoch` returns
-- | values outside the host's `Date` range.
nowParts :: forall r e. RIO r e ClockParts
nowParts = do
  ms <- currentEpoch
  case partsFromMs ms of
    Just p -> pure p
    Nothing -> pure
      { year: 1970
      , month: 1
      , day: 1
      , hour: 0
      , minute: 0
      , second: 0
      , millisecond: 0
      , dayOfWeek: 4
      }

-- | Run an action and return how long it took alongside the result.
-- | The duration is computed by sampling `currentEpoch` before and
-- | after; under the live clock this is wall-clock time including
-- | any time the fiber was suspended by other work. Under a mock
-- | clock the duration is whatever the mock reports.
-- |
-- | Returns the duration first so the result destructures cleanly
-- | as `Tuple elapsed value`.
timed
  :: forall r e a
   . RIO r e a
  -> RIO r e (Tuple Milliseconds a)
timed action = do
  Milliseconds startMs <- currentEpoch
  result <- action
  Milliseconds endMs <- currentEpoch
  pure (Tuple (Milliseconds (endMs - startMs)) result)

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
