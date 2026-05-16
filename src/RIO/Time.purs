-- | Ergonomics around the `Clock` service and `Data.Time.Duration`.
-- |
-- | `Instant` is a millisecond-precision timestamp anchored at the
-- | Unix epoch. `Duration` is `Data.Time.Duration.Milliseconds`,
-- | re-exported here so callers can stick to one import for
-- | time-shaped values.
-- |
-- | The combinator surface deliberately stays small: smart
-- | constructors for common `Duration` units (`seconds` /
-- | `minutes` / `hours` / `days`), basic arithmetic
-- | (`addDuration` / `subDuration` / `between`), comparison via
-- | the derived `Ord` instance, ISO 8601 round-trip
-- | (`formatISO8601` / `parseISO8601`), and a `humanize` renderer
-- | for `Duration` values. The `nowInstant` helper threads
-- | `RIO.Clock` so application code can read an `Instant` from
-- | the same service it already uses for `now` / `sleep`.
-- |
-- | The ISO 8601 path goes through the host `Date` constructor:
-- | `formatISO8601` calls `toISOString` (always UTC, always
-- | millisecond precision, e.g. `"2026-05-15T12:34:56.789Z"`) and
-- | `parseISO8601` calls `Date.parse` and returns `Nothing` for
-- | the `NaN` sentinel. This keeps the implementation honest
-- | about platform behaviour: anything `Date` cannot represent is
-- | not representable as an `Instant`.
module RIO.Time
  ( Instant(..)
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
import Data.Int (floor) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Number (isNaN)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))

import RIO.Clock (Clock)
import RIO.Core (RIO, ask)

-- | A millisecond-precision instant on the wall clock, anchored
-- | at the Unix epoch. Internally a `Milliseconds`; the newtype
-- | distinguishes "a moment in time" from "a length of time".
newtype Instant = Instant Milliseconds

derive instance newtypeInstant :: Newtype Instant _
derive newtype instance eqInstant :: Eq Instant
derive newtype instance ordInstant :: Ord Instant

instance showInstant :: Show Instant where
  show (Instant (Milliseconds ms)) = "(Instant " <> show ms <> ")"

-- | A length of time, in milliseconds. Re-exported from
-- | `Data.Time.Duration` so callers can stay on a single import.
type Duration = Milliseconds

-- | Strip the `Instant` newtype.
unInstant :: Instant -> Milliseconds
unInstant = unwrap

-- | Build an `Instant` from a `Milliseconds`-since-epoch value.
fromMilliseconds :: Milliseconds -> Instant
fromMilliseconds = Instant

-- | Read the millisecond-since-epoch payload.
toMilliseconds :: Instant -> Milliseconds
toMilliseconds = unwrap

-- | The Unix epoch, `1970-01-01T00:00:00.000Z`.
epoch :: Instant
epoch = Instant (Milliseconds 0.0)

-- | Read the current `Instant` from the `Clock` service.
nowInstant :: forall r e. RIO (clock :: Clock | r) e Instant
nowInstant = do
  c <- ask (Proxy :: Proxy "clock")
  Instant <$> liftAff c.now

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
-- | account for leap seconds or DST; for civil-day arithmetic
-- | reach for `Data.DateTime` directly.
days :: Number -> Duration
days n = Milliseconds (n * 86_400_000.0)

-- | Shift an `Instant` forward by a `Duration`.
addDuration :: Duration -> Instant -> Instant
addDuration (Milliseconds d) (Instant (Milliseconds t)) =
  Instant (Milliseconds (t + d))

-- | Shift an `Instant` backward by a `Duration`.
subDuration :: Duration -> Instant -> Instant
subDuration (Milliseconds d) (Instant (Milliseconds t)) =
  Instant (Milliseconds (t - d))

-- | `between a b` is the `Duration` from `a` to `b`. Negative if
-- | `b` precedes `a`.
between :: Instant -> Instant -> Duration
between (Instant (Milliseconds a)) (Instant (Milliseconds b)) =
  Milliseconds (b - a)

-- | The raw millisecond delta between two `Instant`s, as a
-- | `Number`. Convenience for cases where you want to compare
-- | against a numeric threshold.
diffMs :: Instant -> Instant -> Number
diffMs a b = case between a b of Milliseconds ms -> ms

foreign import toISOStringImpl :: Number -> Effect String
foreign import parseISO8601Impl :: String -> Number

-- | Render an `Instant` as a millisecond-precision UTC ISO 8601
-- | string, e.g. `"2026-05-15T12:34:56.789Z"`.
formatISO8601 :: forall r e. Instant -> RIO r e String
formatISO8601 (Instant (Milliseconds ms)) = liftEffect (toISOStringImpl ms)

-- | Parse a millisecond-precision ISO 8601 string into an
-- | `Instant`. Returns `Nothing` if the host `Date` parser
-- | rejects the input.
parseISO8601 :: String -> Maybe Instant
parseISO8601 s =
  let
    ms = parseISO8601Impl s
  in
    if isNaN ms then Nothing
    else Just (Instant (Milliseconds ms))

-- | Render a `Duration` as a short human-readable string, e.g.
-- | `"1h 30m"`, `"500ms"`, `"-2s"`. Trailing zero units beyond
-- | the largest non-zero component are dropped. Negative
-- | durations are prefixed with `"-"`.
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
