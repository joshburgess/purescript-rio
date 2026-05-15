-- | A `Clock` service plus a live implementation.
-- |
-- | Phase 7.1 introduces the service in production code (rather than
-- | hiding it in a testing module) so application programs can `ask`
-- | for time and `sleep` through the same row machinery they use for
-- | every other service. The mock implementation lives in
-- | `RIO.Test.Clock`.
-- |
-- | Two operations: `now` returns the current wall-clock time in
-- | milliseconds since the Unix epoch, and `sleep ms` suspends the
-- | current fiber for at least `ms` real-time milliseconds.
-- |
-- | The service operations are `Aff`-valued, following the convention
-- | from `docs/02-services.md`; the smart constructors lift them into
-- | `RIO`.
module RIO.Clock
  ( Clock
  , ClockParts
  , now
  , nowParts
  , partsFromMs
  , sleep
  , timed
  , liveClock
  ) where

import Prelude

import Data.DateTime (date, time) as DT
import Data.DateTime.Instant (instant, toDateTime, unInstant)
import Data.Date (day, month, weekday, year) as Date
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.Time (hour, millisecond, minute, second) as Time
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Milliseconds(..))
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now) as Now
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask)

-- | The service record. `now` returns milliseconds since the Unix
-- | epoch; `sleep` blocks for the given duration.
type Clock =
  { now :: Aff Milliseconds
  , sleep :: Milliseconds -> Aff Unit
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
-- | wall-clock components. Useful when you want to render a
-- | timestamp emitted from the Clock service, or when implementing
-- | cron-shaped schedules.
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

-- | Read the current wall-clock time.
-- |
-- | ```purescript
-- | timestampedLog :: forall r e. String -> RIO (clock :: Clock | r) e String
-- | timestampedLog msg = do
-- |   t <- now
-- |   pure (show t <> ": " <> msg)
-- | ```
now :: forall r e. RIO (clock :: Clock | r) e Milliseconds
now = do
  c <- ask (Proxy :: Proxy "clock")
  liftAff c.now

-- | Read the current wall-clock time, decomposed into UTC parts.
-- |
-- | Convenience for `now` followed by `partsFromMs`. Defects with
-- | a defect (rather than returning a `Maybe`) if the timestamp
-- | cannot be represented as a `DateTime`, which is only possible
-- | with a mock clock whose `now` returns values outside the
-- | range of the host's `Date`.
nowParts
  :: forall r e
   . RIO (clock :: Clock | r) e ClockParts
nowParts = do
  ms <- now
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

-- | Suspend the current fiber for at least the given duration.
-- |
-- | Under `liveClock` this delegates to `Effect.Aff.delay`, which is
-- | cancellable: an `interrupt` on the fiber will abort the sleep at
-- | the next event-loop tick (see the Phase 0.5 spike's S1).
-- |
-- | ```purescript
-- | pollEvery :: forall r e. Milliseconds -> RIO r e Unit -> RIO (clock :: Clock | r) e Unit
-- | pollEvery interval action = do
-- |   action
-- |   sleep interval
-- |   pollEvery interval action
-- | ```
sleep :: forall r e. Milliseconds -> RIO (clock :: Clock | r) e Unit
sleep ms = do
  c <- ask (Proxy :: Proxy "clock")
  liftAff (c.sleep ms)

-- | Run an action and return how long it took alongside the
-- | result. The duration is computed by sampling `now` before and
-- | after; under the live clock this is wall-clock time including
-- | any time the fiber was suspended by other work on the event
-- | loop. Under a mock clock the duration is whatever the mock
-- | reports.
-- |
-- | Returns the duration first so the result destructures cleanly
-- | as `Tuple elapsed value`.
-- |
-- | ```purescript
-- | -- log how long the query took
-- | Tuple elapsed rows <- timed runQuery
-- | logInfo ("query took " <> show elapsed)
-- | ```
timed
  :: forall r e a
   . RIO (clock :: Clock | r) e a
  -> RIO (clock :: Clock | r) e (Tuple Milliseconds a)
timed action = do
  Milliseconds startMs <- now
  result <- action
  Milliseconds endMs <- now
  pure (Tuple (Milliseconds (endMs - startMs)) result)

-- | A production-ready implementation backed by `Effect.Now` and
-- | `Effect.Aff.delay`. Provide it via `provide` / `provideAll` or
-- | construct a `Layer` that emits it.
-- |
-- | ```purescript
-- | -- inject the live clock at the top of the program
-- | main = launchAff_ (runRIO (provideAll { clock: liveClock } program))
-- | ```
liveClock :: Clock
liveClock =
  { now: do
      i <- liftEffect Now.now
      pure (unInstant i)
  , sleep: Aff.delay
  }
