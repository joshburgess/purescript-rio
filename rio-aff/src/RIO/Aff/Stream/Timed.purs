-- | Time-driven `Stream` combinators that depend on the `Clock`
-- | service.
-- |
-- | Where the temporal combinators in `RIO.Aff.Stream` are content with
-- | wall-clock waits (none today), the combinators here all read
-- | virtual time from `RIO.Aff.Clock` so they are deterministic against
-- | `RIO.Aff.Test.Clock`.
-- |
-- | * `throttle` rate-limits emission to at most one element per
-- |   interval.
-- | * `debounce` emits a value only after `interval` of upstream
-- |   silence; bursts coalesce to a single emit of the latest item.
-- | * `groupedWithin` batches elements into chunks of up to `n`,
-- |   flushing whichever happens first: the chunk fills or the
-- |   `duration` since the group's first element elapses.
-- |
-- | Cancellation semantics: `debounce` and `groupedWithin` race a
-- | timer against an upstream pull via `RIO.Aff.Concurrency.race`. When
-- | the timer wins, the in-flight pull is interrupted. For pure
-- | streams (`fromArray`, `range`, etc.) this has no observable
-- | effect; for streams whose pull holds external resources, the
-- | resource is released as part of the interrupt (the standard
-- | `Scope` / `acquireRelease` story).
module RIO.Aff.Stream.Timed
  ( debounce
  , groupedWithin
  , throttle
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..))

import RIO.Aff.Clock (Clock)
import RIO.Aff.Clock as Clock
import RIO.Aff.Concurrency (race)
import RIO.Aff.Stream (Step(..), Stream(..), unStream)

-- | Emit elements no faster than one per `interval`. The first
-- | element is emitted as soon as upstream produces it; each
-- | subsequent element is held until `interval` has passed since
-- | the previous emit.
throttle
  :: forall r e a
   . Milliseconds
  -> Stream (clock :: Clock | r) e a
  -> Stream (clock :: Clock | r) e a
throttle interval = go
  where
  go s = Stream do
    step <- unStream s
    case step of
      Done -> pure Done
      Yield a rest -> pure (Yield a (sleepAndGo rest))

  sleepAndGo s = Stream do
    Clock.sleep interval
    unStream (go s)

-- | Emit a value only after `interval` of upstream silence. A
-- | burst of values within `interval` of each other collapses to
-- | the most recent value. Useful for resampling a chattier
-- | upstream into one emission per "settled" window.
debounce
  :: forall r e a
   . Milliseconds
  -> Stream (clock :: Clock | r) e a
  -> Stream (clock :: Clock | r) e a
debounce interval = go Nothing
  where
  go pending s = Stream do
    case pending of
      Nothing -> do
        step <- unStream s
        case step of
          Done -> pure Done
          Yield a rest -> unStream (go (Just a) rest)
      Just latest -> do
        -- Race a pull against an interval-long sleep. If the
        -- sleep wins, the in-flight pull is cancelled and we
        -- emit the buffered value. If the pull wins, replace
        -- the buffered value and start the timer over.
        outcome <- race (Left <$> unStream s) (Right <$> Clock.sleep interval)
        case outcome of
          Right _ -> pure (Yield latest (go Nothing s))
          Left Done -> pure (Yield latest (Stream (pure Done)))
          Left (Yield newLatest rest) ->
            unStream (go (Just newLatest) rest)

-- local Either for race scrutinee; avoids importing Data.Either
-- and aliasing through a separate type.

-- | Group elements into chunks of up to `maxSize`. A chunk flushes
-- | as soon as it fills, *or* when `duration` elapses since the
-- | first element of that chunk arrived (whichever comes first).
-- | An empty input stream produces an empty output stream; a
-- | partial trailing chunk is still flushed on upstream
-- | termination.
groupedWithin
  :: forall r e a
   . Int
  -> Milliseconds
  -> Stream (clock :: Clock | r) e a
  -> Stream (clock :: Clock | r) e (Array a)
groupedWithin maxSize duration
  | maxSize <= 0 = \_ -> Stream (pure Done)
  | otherwise =
      let
        Milliseconds dMs = duration

        go groupStart buf s = Stream do
          if Array.length buf >= maxSize then
            pure (Yield buf (go (Milliseconds 0.0) [] s))
          else if Array.null buf then do
            step <- unStream s
            case step of
              Done -> pure Done
              Yield a rest -> do
                now <- Clock.now
                unStream (go now [ a ] rest)
          else do
            let Milliseconds startMs = groupStart
            Milliseconds nowMs <- Clock.now
            let remainingMs = max 0.0 (startMs + dMs - nowMs)
            if remainingMs <= 0.0 then
              pure (Yield buf (go (Milliseconds 0.0) [] s))
            else do
              outcome <- race
                (Left <$> unStream s)
                (Right <$> Clock.sleep (Milliseconds remainingMs))
              case outcome of
                Right _ -> pure (Yield buf (go (Milliseconds 0.0) [] s))
                Left Done -> pure (Yield buf (Stream (pure Done)))
                Left (Yield a rest) ->
                  unStream (go groupStart (Array.snoc buf a) rest)
      in
        go (Milliseconds 0.0) []

-- Local Either-like helpers for `debounce` and `groupedWithin` so
-- the race scrutinee can pattern-match without importing
-- `Data.Either`.
data Either a b = Left a | Right b
