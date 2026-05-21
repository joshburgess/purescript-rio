-- | A cumulative-bucket histogram with a configurable bucket
-- | layout.
-- |
-- | Where `RIO.Aff.Metric.Histogram` takes an explicit array of
-- | upper bounds, `BucketHistogram` derives its bounds from a
-- | declarative `BucketLayout` (linear, exponential, or explicit
-- | array) and reports a cumulative-count snapshot in the shape
-- | Prometheus and OpenTelemetry exporters expect.
-- |
-- | Per-bucket counts are stored non-cumulatively in the `Ref`
-- | and converted to the cumulative shape at snapshot time. An
-- | implicit overflow bucket at `+Infinity` is always present, so
-- | every observation lands somewhere.
-- |
-- | Intended import:
-- |
-- | ```purescript
-- | import RIO.Aff.Metric.BucketHistogram (BucketHistogram, BucketLayout(..))
-- | import RIO.Aff.Metric.BucketHistogram as BucketHistogram
-- |
-- | h <- BucketHistogram.make
-- |        (BucketHistogram.Linear { start: 0.0, width: 5.0, count: 10 })
-- | BucketHistogram.observe 3.0 h
-- | snap <- BucketHistogram.snapshot h
-- | -- snap.buckets :: Array { le :: Number, count :: Int }
-- | -- snap.count   :: Int
-- | -- snap.sum     :: Number
-- | ```
module RIO.Aff.Metric.BucketHistogram
  ( BucketHistogram
  , BucketLayout(..)
  , Bucket
  , Snapshot
  , make
  , observe
  , snapshot
  , withTimer
  ) where

import Prelude

import Data.Array (cons, index, length, mapWithIndex, replicate, scanl, snoc, uncons, updateAtIndices, zipWith) as Array
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (infinity, pow)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Cause (ensuringWith)
import RIO.Aff.Clock (Clock, now)
import RIO.Aff.Core (RIO)

-- | How to lay out histogram buckets.
-- |
-- |   * `Linear { start, width, count }` builds `count` buckets at
-- |     `start, start + width, start + 2*width, ...`.
-- |   * `Exponential { start, factor, count }` builds `count` buckets
-- |     at `start, start*factor, start*factor^2, ...`.
-- |   * `Explicit xs` uses the given upper bounds verbatim. The
-- |     array must be sorted ascending; duplicates are tolerated but
-- |     wasteful.
-- |
-- | All three variants get an implicit overflow bucket at `+Infinity`
-- | so every observation lands somewhere.
data BucketLayout
  = Linear { start :: Number, width :: Number, count :: Int }
  | Exponential { start :: Number, factor :: Number, count :: Int }
  | Explicit (Array Number)

-- | A single bucket entry: cumulative count of observations less
-- | than or equal to `le`. Prometheus and OTel both use the
-- | less-than-or-equal convention.
type Bucket = { le :: Number, count :: Int }

-- | A snapshot of a bucket histogram in exporter-ready shape.
-- | `buckets` is the cumulative bucket-count array including the
-- | implicit overflow bucket (its `le` is `+Infinity`).
type Snapshot =
  { count :: Int
  , sum :: Number
  , buckets :: Array Bucket
  }

newtype BucketHistogram = BucketHistogram (Ref BucketState)

type BucketState =
  { boundaries :: Array Number
  , counts :: Array Int
  , count :: Int
  , sum :: Number
  }

-- | Allocate a fresh bucket histogram from a layout. Negative or
-- | non-finite layout parameters are clamped to safe defaults
-- | (count >= 1, width >= 0, factor > 1).
make :: forall r e. BucketLayout -> RIO r e BucketHistogram
make layout = liftEffect do
  let bs = boundariesOf layout
  ref <- Ref.new
    { boundaries: bs
    , counts: Array.replicate (Array.length bs + 1) 0
    , count: 0
    , sum: 0.0
    }
  pure (BucketHistogram ref)

boundariesOf :: BucketLayout -> Array Number
boundariesOf = case _ of
  Linear { start, width, count } ->
    let
      n = max 1 count
      w = max 0.0 width
    in
      Array.mapWithIndex
        (\i _ -> start + toNumber i * w)
        (Array.replicate n unit)
  Exponential { start, factor, count } ->
    let
      n = max 1 count
      f = if factor > 1.0 then factor else 2.0
    in
      Array.mapWithIndex
        (\i _ -> start * (f `pow` toNumber i))
        (Array.replicate n unit)
  Explicit xs -> xs

-- | Record an observation. Increments the count of the lowest
-- | bucket whose upper bound is `>= x`, plus the implicit overflow
-- | bucket if `x` exceeds every boundary. Updates the running total
-- | count and sum.
observe :: forall r e. Number -> BucketHistogram -> RIO r e Unit
observe x (BucketHistogram ref) = liftEffect (Ref.modify_ step ref)
  where
  step st =
    let
      idx = bucketIndex 0 st.boundaries
      counts' = bumpAt idx st.counts
    in
      st { counts = counts', count = st.count + 1, sum = st.sum + x }

  bucketIndex i bounds = case Array.index bounds i of
    Just le | x <= le -> i
    Just _ -> bucketIndex (i + 1) bounds
    Nothing -> i

  bumpAt i counts = case Array.index counts i of
    Just c -> Array.updateAtIndices [ Tuple i (c + 1) ] counts
    Nothing -> counts

-- | Read the current snapshot. The returned `buckets` array is
-- | cumulative (each entry's `count` is "total observations with
-- | value less than or equal to `le`") and includes an overflow
-- | bucket with `le = +Infinity`.
snapshot :: forall r e. BucketHistogram -> RIO r e Snapshot
snapshot (BucketHistogram ref) = liftEffect do
  st <- Ref.read ref
  let
    cum = inclusiveScan st.counts
    entries = Array.zipWith
      (\le c -> { le, count: c })
      (Array.snoc st.boundaries infinity)
      cum
  pure
    { count: st.count
    , sum: st.sum
    , buckets: entries
    }

inclusiveScan :: Array Int -> Array Int
inclusiveScan xs = case Array.uncons xs of
  Nothing -> []
  Just { head, tail } -> Array.cons head (Array.scanl (+) head tail)

-- | Record the wall-clock duration of `action` into `histogram`
-- | regardless of outcome (success, typed failure, or defect). The
-- | recorded value is in milliseconds, sampled from the `Clock`
-- | service, so a virtual test clock makes the timing deterministic.
-- |
-- | The action's value and failure semantics pass through unchanged;
-- | the observation runs inside the uninterruptible finalizer that
-- | `ensuringWith` installs, so a late cancellation cannot skip it.
withTimer
  :: forall r e a
   . BucketHistogram
  -> RIO (clock :: Clock | r) e a
  -> RIO (clock :: Clock | r) e a
withTimer histogram action = do
  Milliseconds startMs <- now
  ensuringWith action \_ -> do
    Milliseconds endMs <- now
    observe (endMs - startMs) histogram

