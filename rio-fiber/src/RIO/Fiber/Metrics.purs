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
-- |     plus rough p50 / p95).
-- |
-- | These do not push to any backend; they're observation primitives
-- | code can read via `value` / `summary` and a host can export.
module RIO.Fiber.Metrics
  ( Counter
  , Gauge
  , Histogram
  , HistogramSummary
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
  ) where

import Prelude

import Data.Array (length, snoc, sort)
import Data.Array as Array
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, liftEffect)

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

