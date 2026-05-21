-- | Ergonomics around the `Clock` service and `Data.Time.Duration`.
-- |
-- | `Instant` is the standard `Data.DateTime.Instant.Instant`
-- | (millisecond-precision, anchored at the Unix epoch). `Duration`
-- | is `Data.Time.Duration.Milliseconds`, re-exported so callers can
-- | stick to one import for time-shaped values.
-- |
-- | Reusing the stdlib `Instant` (rather than wrapping `Milliseconds`
-- | in a fresh newtype like `RIO.Aff.Time` does) means the same type
-- | flows through `RIO.Fiber.Clock`, `Data.DateTime`, and any other
-- | code in the PureScript ecosystem already written against
-- | `Data.DateTime.Instant`.
-- |
-- | The combinator surface deliberately stays small: smart
-- | constructors for common `Duration` units (`seconds` / `minutes`
-- | / `hours` / `days`), basic arithmetic (`addDuration` /
-- | `subDuration` / `between`), ISO 8601 round-trip
-- | (`formatISO8601` / `parseISO8601`), and a `humanize` renderer
-- | for `Duration` values.
-- |
-- | The ISO 8601 path goes through the host `Date` constructor:
-- | `formatISO8601` calls `toISOString` (always UTC, always
-- | millisecond precision, e.g. `"2026-05-15T12:34:56.789Z"`) and
-- | `parseISO8601` calls `Date.parse` and returns `Nothing` for the
-- | `NaN` sentinel. This keeps the implementation honest about
-- | platform behaviour: anything `Date` cannot represent is not
-- | representable as an `Instant`.
module RIO.Fiber.Time
  ( module Exports
  , Duration
  , unInstant
  , fromMilliseconds
  , toMilliseconds
  , epoch
  , nowInstant
  -- Duration constructors
  , milliseconds
  , seconds
  , minutes
  , hours
  , days
  -- Arithmetic
  , addDuration
  , subDuration
  , between
  , diffMs
  -- ISO 8601
  , formatISO8601
  , parseISO8601
  -- Humanization
  , humanize
  ) where

import Prelude hiding (between)

import Data.Array (catMaybes, intercalate) as Array
import Data.DateTime.Instant (instant, unInstant) as Instant
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant (Instant) as Exports
import Data.Int (floor) as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Number (isNaN)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

import RIO.Fiber.Clock (currentTime)
import RIO.Fiber.Core (RIO, liftEffect)

-- | A length of time, in milliseconds. Re-exported from
-- | `Data.Time.Duration` so callers can stay on a single import.
type Duration = Milliseconds

-- | Strip the `Instant` newtype to its underlying `Milliseconds`.
unInstant :: Instant -> Milliseconds
unInstant = Instant.unInstant

-- | Build an `Instant` from a `Milliseconds`-since-epoch value.
-- | Returns `Nothing` for values outside the representable range of
-- | `Data.DateTime.Instant`. (For the unchecked sibling reach for
-- | `Data.DateTime.Instant.unsafeInstant`.)
fromMilliseconds :: Milliseconds -> Maybe Instant
fromMilliseconds = Instant.instant

-- | Read the millisecond-since-epoch payload.
toMilliseconds :: Instant -> Milliseconds
toMilliseconds = Instant.unInstant

-- | The Unix epoch, `1970-01-01T00:00:00.000Z`. `Milliseconds 0.0`
-- | is statically representable so the `Maybe` is discharged via
-- | `unsafePartial fromJust`.
epoch :: Instant
epoch = unsafePartial (fromJust (Instant.instant (Milliseconds 0.0)))

-- | Read the current `Instant` from the active `Clock` (via the
-- | `Clock` `FiberRef`). Thin alias for `RIO.Fiber.Clock.currentTime`
-- | so call sites that already import this module need not pull in
-- | the Clock module just for the read.
nowInstant :: forall r e. RIO r e Instant
nowInstant = currentTime

-- | A `Duration` of `n` milliseconds.
milliseconds :: Number -> Duration
milliseconds = Milliseconds

-- | A `Duration` of `n` seconds.
seconds :: Number -> Duration
seconds n = Milliseconds (n * 1000.0)

-- | A `Duration` of `n` minutes.
minutes :: Number -> Duration
minutes n = Milliseconds (n * 60_000.0)

-- | A `Duration` of `n` hours.
hours :: Number -> Duration
hours n = Milliseconds (n * 3_600_000.0)

-- | A `Duration` of `n` 24-hour days. Note that this does not
-- | account for leap seconds or DST; for civil-day arithmetic reach
-- | for `Data.DateTime` directly.
days :: Number -> Duration
days n = Milliseconds (n * 86_400_000.0)

-- | Shift an `Instant` forward by a `Duration`. Returns `Nothing` if
-- | the result would fall outside the representable range of
-- | `Data.DateTime.Instant`.
addDuration :: Duration -> Instant -> Maybe Instant
addDuration (Milliseconds d) i =
  case Instant.unInstant i of
    Milliseconds t -> Instant.instant (Milliseconds (t + d))

-- | Shift an `Instant` backward by a `Duration`. Returns `Nothing`
-- | if the result would fall outside the representable range of
-- | `Data.DateTime.Instant`.
subDuration :: Duration -> Instant -> Maybe Instant
subDuration (Milliseconds d) i =
  case Instant.unInstant i of
    Milliseconds t -> Instant.instant (Milliseconds (t - d))

-- | `between a b` is the `Duration` from `a` to `b`. Negative if `b`
-- | precedes `a`.
between :: Instant -> Instant -> Duration
between a b =
  case Instant.unInstant a, Instant.unInstant b of
    Milliseconds ax, Milliseconds bx -> Milliseconds (bx - ax)

-- | The raw millisecond delta between two `Instant`s, as a `Number`.
-- | Convenience for cases where you want to compare against a
-- | numeric threshold.
diffMs :: Instant -> Instant -> Number
diffMs a b = case between a b of Milliseconds ms -> ms

foreign import toISOStringImpl :: Number -> Effect String
foreign import parseISO8601Impl :: String -> Number

-- | Render an `Instant` as a millisecond-precision UTC ISO 8601
-- | string, e.g. `"2026-05-15T12:34:56.789Z"`.
formatISO8601 :: forall r e. Instant -> RIO r e String
formatISO8601 i =
  case Instant.unInstant i of
    Milliseconds ms -> liftEffect (toISOStringImpl ms)

-- | Parse a millisecond-precision ISO 8601 string into an `Instant`.
-- | Returns `Nothing` if the host `Date` parser rejects the input or
-- | the resulting timestamp is outside the representable `Instant`
-- | range.
parseISO8601 :: String -> Maybe Instant
parseISO8601 s =
  let
    ms = parseISO8601Impl s
  in
    if isNaN ms then Nothing
    else Instant.instant (Milliseconds ms)

-- | Render a `Duration` as a short human-readable string, e.g.
-- | `"1h 30m"`, `"500ms"`, `"-2s"`. Trailing zero units beyond the
-- | largest non-zero component are dropped. Negative durations are
-- | prefixed with `"-"`.
humanize :: Duration -> String
humanize (Milliseconds d)
  | d < 0.0 = "-" <> humanize (Milliseconds (-d))
  | d == 0.0 = "0ms"
  | d < 1000.0 = show (Int.floor d) <> "ms"
  | otherwise =
      let
        total = Int.floor d
        secs = (total / 1000) `mod` 60
        mins = (total / 60_000) `mod` 60
        hrs = (total / 3_600_000) `mod` 24
        ds = total / 86_400_000
        part u n = if n > 0 then Just (show n <> u) else Nothing
      in
        Array.intercalate " "
          ( Array.catMaybes
              [ part "d" ds
              , part "h" hrs
              , part "m" mins
              , part "s" secs
              ]
          )
