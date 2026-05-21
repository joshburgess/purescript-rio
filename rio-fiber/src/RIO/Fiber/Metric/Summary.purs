-- | A first-class summary metric that retains every observation
-- | (up to an optional cap) and can compute quantiles on demand.
-- |
-- | Summaries differ from histograms in shape: a histogram
-- | discretises observations into predefined buckets at write
-- | time; a summary keeps the raw samples and computes
-- | quantiles at read time. This makes summaries flexible (no
-- | need to choose boundaries up front) at the cost of unbounded
-- | memory unless `make` is given a sample cap.
-- |
-- | When the cap is reached, the oldest sample is dropped on
-- | the next observation: a simple sliding-window
-- | approximation suitable for ongoing measurement of a steady
-- | stream of values.
-- |
-- | Intended import:
-- |
-- | ```purescript
-- | import RIO.Fiber.Metric.Summary (Summary)
-- | import RIO.Fiber.Metric.Summary as Summary
-- |
-- | s <- Summary.make 1024
-- | for_ latencies (\ms -> Summary.observe ms s)
-- | p50 <- Summary.quantile 0.5 s
-- | p99 <- Summary.quantile 0.99 s
-- | ```
module RIO.Fiber.Metric.Summary
  ( Summary
  , Snapshot
  , make
  , observe
  , quantile
  , snapshot
  ) where

import Prelude

import Data.Array (drop, foldl, index, length, snoc, sort)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Fiber.Core (RIO, liftEffect)

newtype Summary = Summary
  { capacity :: Int
  , samples :: Ref (Array Number)
  }

type Snapshot =
  { count :: Int
  , sum :: Number
  , samples :: Array Number
  }

-- | Allocate a fresh summary that retains up to `capacity`
-- | observations. Non-positive `capacity` is normalised to a
-- | minimum window of 1.
make :: forall r e. Int -> RIO r e Summary
make capacity = liftEffect do
  let cap = if capacity <= 0 then 1 else capacity
  samples <- Ref.new []
  pure (Summary { capacity: cap, samples })

-- | Record an observation. Drops the oldest sample if the
-- | capacity is already reached, keeping the sample buffer
-- | bounded.
observe :: forall r e. Number -> Summary -> RIO r e Unit
observe v (Summary s) = liftEffect do
  Ref.modify_
    ( \xs ->
        let
          appended = snoc xs v
        in
          if length appended > s.capacity then drop 1 appended
          else appended
    )
    s.samples

-- | Read the `q`th quantile (`0.0 <= q <= 1.0`) of the
-- | retained sample set. Returns `Nothing` if no observation
-- | has been made yet. Out-of-range `q` is clamped to
-- | `[0, 1]`.
-- |
-- | Uses the "nearest-rank" definition: sort the samples,
-- | compute `ceil(q * count)` (with at least index 0), and
-- | return that element. No interpolation, so the returned
-- | value is always one of the observed samples.
quantile :: forall r e. Number -> Summary -> RIO r e (Maybe Number)
quantile q (Summary s) = liftEffect do
  xs <- Ref.read s.samples
  let n = length xs
  if n == 0 then pure Nothing
  else do
    let
      clamped =
        if q < 0.0 then 0.0
        else if q > 1.0 then 1.0
        else q
      idx0 = floor (clamped * toNumber n)
      idx = if idx0 >= n then n - 1 else idx0
      sorted = sort xs
    pure (index sorted idx)

-- | Read a snapshot of the summary's current state.
snapshot :: forall r e. Summary -> RIO r e Snapshot
snapshot (Summary s) = liftEffect do
  xs <- Ref.read s.samples
  let
    total = foldl (+) 0.0 xs
  pure { count: length xs, sum: total, samples: xs }
