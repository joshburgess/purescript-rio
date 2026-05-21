-- | In-process metrics primitives.
-- |
-- | Three families, each `Ref`-backed and safe to share between fibers
-- | because every mutating operation goes through a single `Ref.modify`
-- | (atomic in the JS single-threaded runtime):
-- |
-- |   * `Counter` is a monotonic, non-negative integer; useful for
-- |     "number of X processed".
-- |   * `Gauge` is a free-running number; useful for "current depth".
-- |   * `Histogram` records `Number` samples into a fixed-size
-- |     reservoir and reports a small summary (count, sum, min, max,
-- |     plus rough p50 / p95). Best for in-process diagnostics.
-- |   * `BucketHistogram` records `Number` samples into cumulative
-- |     per-bucket counts shaped for Prometheus / OpenTelemetry
-- |     export. Buckets are configured via a `BucketLayout`
-- |     (Linear, Exponential, or Explicit). Picks this variant when
-- |     you intend to ship histograms across a wire.
-- |
-- | These do not push to any backend; they're observation primitives
-- | code can read via `counterValue` / `gaugeValue` / `summary` /
-- | `bucketSnapshot` and a host can export.
module RIO.Fiber.Metrics
  ( Counter
  , Gauge
  , Histogram
  , HistogramSummary
  , BucketHistogram
  , BucketLayout(..)
  , BucketSnapshot
  , Bucket
  , newCounter
  , incr
  , incrBy
  , counterValue
  , newGauge
  , set
  , gaugeIncr
  , gaugeDecr
  , gaugeValue
  , newHistogram
  , record
  , summary
  , newBucketHistogram
  , recordBucket
  , bucketSnapshot
  , withCounter
  , withTimer
  ) where

import Prelude

import Data.Array (length, snoc, sort)
import Data.Array as Array
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (infinity, pow)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Clock (currentEpoch)
import RIO.Fiber.Core (RIO, ensuringWith, liftEffect)

-- | A monotonic, non-negative counter.
newtype Counter = Counter (Ref Int)

-- | A free-running gauge.
newtype Gauge = Gauge (Ref Number)

-- | A bounded reservoir of samples.
newtype Histogram = Histogram (Ref HistogramState)

type HistogramState =
  { capacity :: Int
  , samples :: Array Number
  , count :: Int
  , sum :: Number
  , min :: Maybe Number
  , max :: Maybe Number
  }

-- | A snapshot of a histogram. `count` / `sum` are cumulative; the
-- | percentile-ish fields are computed from the current reservoir
-- | (so they're "recent" rather than "lifetime").
type HistogramSummary =
  { count :: Int
  , sum :: Number
  , min :: Maybe Number
  , max :: Maybe Number
  , p50 :: Maybe Number
  , p95 :: Maybe Number
  }

-- | Allocate a counter starting at zero.
newCounter :: Effect Counter
newCounter = Counter <$> Ref.new 0

-- | Add one.
incr :: forall r e. Counter -> RIO r e Unit
incr c = incrBy c 1

-- | Add `n`. Negative `n` is clamped to zero (the counter is monotonic).
incrBy :: forall r e. Counter -> Int -> RIO r e Unit
incrBy (Counter ref) n = liftEffect
  (Ref.modify_ (\v -> v + max 0 n) ref)

-- | Read the current count.
counterValue :: forall r e. Counter -> RIO r e Int
counterValue (Counter ref) = liftEffect (Ref.read ref)

-- | Allocate a gauge starting at the given value.
newGauge :: Number -> Effect Gauge
newGauge n = Gauge <$> Ref.new n

-- | Set the gauge to an absolute value.
set :: forall r e. Gauge -> Number -> RIO r e Unit
set (Gauge ref) n = liftEffect (Ref.write n ref)

-- | Add `delta`. Pass a negative `delta` to subtract.
gaugeIncr :: forall r e. Gauge -> Number -> RIO r e Unit
gaugeIncr (Gauge ref) delta = liftEffect
  (Ref.modify_ (_ + delta) ref)

-- | Subtract `delta`. Convenience over `gaugeIncr g (-delta)`.
gaugeDecr :: forall r e. Gauge -> Number -> RIO r e Unit
gaugeDecr g delta = gaugeIncr g (negate delta)

-- | Read the current value.
gaugeValue :: forall r e. Gauge -> RIO r e Number
gaugeValue (Gauge ref) = liftEffect (Ref.read ref)

-- | Allocate a histogram with a bounded reservoir of `capacity`
-- | most-recent samples. Lifetime `count` and `sum` are kept
-- | exactly; min / max / percentiles come from the reservoir.
newHistogram :: Int -> Effect Histogram
newHistogram cap = Histogram <$> Ref.new
  { capacity: max 1 cap
  , samples: []
  , count: 0
  , sum: 0.0
  , min: Nothing
  , max: Nothing
  }

-- | Record a sample. Updates `count`, `sum`, `min`, `max`, and pushes
-- | into the reservoir (dropping the oldest if it would overflow).
record :: forall r e. Histogram -> Number -> RIO r e Unit
record (Histogram ref) x = liftEffect
  ( Ref.modify_
      ( \st ->
          let
            samples' =
              if length st.samples >= st.capacity then
                snoc (Array.drop 1 st.samples) x
              else
                snoc st.samples x
            min' = case st.min of
              Nothing -> Just x
              Just m -> Just (min m x)
            max' = case st.max of
              Nothing -> Just x
              Just m -> Just (max m x)
          in
            st
              { samples = samples'
              , count = st.count + 1
              , sum = st.sum + x
              , min = min'
              , max = max'
              }
      )
      ref
  )

-- | Read the current summary.
summary :: forall r e. Histogram -> RIO r e HistogramSummary
summary (Histogram ref) = liftEffect do
  st <- Ref.read ref
  let
    sorted = sort st.samples
    p q = percentile q sorted
  pure
    { count: st.count
    , sum: st.sum
    , min: st.min
    , max: st.max
    , p50: p 0.50
    , p95: p 0.95
    }

-- | Internal: nearest-rank percentile from a sorted array.
percentile :: Number -> Array Number -> Maybe Number
percentile q sorted = case length sorted of
  0 -> Nothing
  n ->
    let
      idx = floor (q * toNumber (n - 1))
    in
      Array.index sorted idx

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
type BucketSnapshot =
  { count :: Int
  , sum :: Number
  , buckets :: Array Bucket
  }

-- | A cumulative-bucket histogram. Stores per-bucket counts plus
-- | total count and sum; uses no rolling reservoir, so it is
-- | exact across the entire history of the process.
newtype BucketHistogram = BucketHistogram (Ref BucketState)

type BucketState =
  { boundaries :: Array Number
  , counts :: Array Int
  , count :: Int
  , sum :: Number
  }

-- | Allocate a bucket histogram from a layout. Negative or
-- | non-finite layout parameters are clamped to safe defaults
-- | (count >= 1, width >= 0, factor > 1).
newBucketHistogram :: BucketLayout -> Effect BucketHistogram
newBucketHistogram layout =
  let bs = boundariesOf layout
  in BucketHistogram <$> Ref.new
    { boundaries: bs
    , counts: Array.replicate (Array.length bs + 1) 0
    , count: 0
    , sum: 0.0
    }

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

-- | Record a sample. Increments the count of the lowest bucket
-- | whose upper bound is `>= x`, plus the implicit overflow
-- | bucket if `x` exceeds every boundary.
recordBucket :: forall r e. BucketHistogram -> Number -> RIO r e Unit
recordBucket (BucketHistogram ref) x = liftEffect
  (Ref.modify_ step ref)
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
-- | bucket with `le = infinity`.
bucketSnapshot :: forall r e. BucketHistogram -> RIO r e BucketSnapshot
bucketSnapshot (BucketHistogram ref) = liftEffect do
  st <- Ref.read ref
  let
    cum = inclusiveScan st.counts
    entries = Array.zipWith
      (\le c -> { le, count: c })
      (snoc st.boundaries infinity)
      cum
  pure
    { count: st.count
    , sum: st.sum
    , buckets: entries
    }

-- | Internal helper: inclusive prefix-sum of an Int array.
-- | `inclusiveScan [a,b,c]` is `[a, a+b, a+b+c]`. Length-preserving.
inclusiveScan :: Array Int -> Array Int
inclusiveScan xs = case Array.uncons xs of
  Nothing -> []
  Just { head, tail } -> Array.cons head (Array.scanl (+) head tail)

-- | Increment `counter` once per completion of `action` regardless
-- | of outcome (success, typed failure, defect, or interrupt). The
-- | action's value and failure semantics pass through unchanged.
-- |
-- | Useful for tagging an effect with a "how many times did this
-- | run" measure without weaving counter increments through every
-- | call site.
withCounter :: forall r e a. Counter -> RIO r e a -> RIO r e a
withCounter counter action =
  ensuringWith action (\_ -> incr counter)

-- | Record the wall-clock duration of `action` into `histogram`
-- | regardless of outcome (success, typed failure, defect, or
-- | interrupt). The recorded value is in milliseconds and is read
-- | from the active clock (so a virtual clock makes the timing
-- | deterministic in tests).
-- |
-- | The action's value and failure semantics pass through
-- | unchanged; the timing measurement runs inside the
-- | uninterruptible finalizer that `ensuringWith` installs, so it
-- | cannot be skipped by a late interrupt.
withTimer :: forall r e a. Histogram -> RIO r e a -> RIO r e a
withTimer histogram action = do
  Milliseconds startMs <- currentEpoch
  ensuringWith action \_ -> do
    Milliseconds endMs <- currentEpoch
    record histogram (endMs - startMs)

