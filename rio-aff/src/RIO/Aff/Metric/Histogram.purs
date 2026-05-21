-- | A first-class histogram with user-supplied bucket
-- | boundaries.
-- |
-- | Each observation lands in the lowest-indexed bucket whose
-- | upper bound is greater than or equal to the observed value.
-- | One extra "overflow" bucket counts observations that exceed
-- | every supplied boundary. The instrument also tracks the
-- | running total observation count and sum.
-- |
-- | The boundaries array is taken as-is; pass it in strictly
-- | ascending order. Empty boundaries produce a histogram with
-- | a single overflow bucket (every observation lands there) -
-- | still useful for count/sum, but `Counter` plus a manual
-- | total may be a better fit for that shape.
-- |
-- | Intended import:
-- |
-- | ```purescript
-- | import RIO.Aff.Metric.Histogram (Histogram)
-- | import RIO.Aff.Metric.Histogram as Histogram
-- |
-- | h <- Histogram.make [ 1.0, 5.0, 10.0, 25.0 ]
-- | Histogram.observe 3.0 h
-- | snap <- Histogram.snapshot h
-- | -- snap.buckets  :: Array Int  (one slot per boundary, plus overflow)
-- | -- snap.count    :: Int
-- | -- snap.sum      :: Number
-- | ```
module RIO.Aff.Metric.Histogram
  ( Histogram
  , Snapshot
  , make
  , observe
  , snapshot
  , withTimer
  ) where

import Prelude

import Data.Array (findIndex, length, mapWithIndex, replicate)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Cause (ensuringWith)
import RIO.Aff.Clock (Clock, now)
import RIO.Aff.Core (RIO)

newtype Histogram = Histogram
  { boundaries :: Array Number
  , buckets :: Ref (Array Int)
  , count :: Ref Int
  , sumRef :: Ref Number
  }

-- | The shape of `snapshot`'s result. `buckets` is one entry
-- | longer than `boundaries`: the final slot counts
-- | observations that exceeded every boundary.
type Snapshot =
  { boundaries :: Array Number
  , buckets :: Array Int
  , count :: Int
  , sum :: Number
  }

-- | Allocate a fresh histogram with the given bucket
-- | boundaries. Boundaries should be supplied in strictly
-- | ascending order.
make :: forall r e. Array Number -> RIO r e Histogram
make boundaries = liftEffect do
  buckets <- Ref.new (replicate (length boundaries + 1) 0)
  count <- Ref.new 0
  sumRef <- Ref.new 0.0
  pure (Histogram { boundaries, buckets, count, sumRef })

-- | Record an observation. The value is bucketed by `boundaries`
-- | and the running count and sum are updated atomically (within
-- | a single tick of the Effect.Ref machinery).
observe :: forall r e. Number -> Histogram -> RIO r e Unit
observe v (Histogram h) = liftEffect do
  let idx = fromMaybe (length h.boundaries) (findIndex (_ >= v) h.boundaries)
  Ref.modify_ (\xs -> mapWithIndex (\i n -> if i == idx then n + 1 else n) xs)
    h.buckets
  Ref.modify_ (_ + 1) h.count
  Ref.modify_ (_ + v) h.sumRef

-- | Read a snapshot of the histogram's current state.
snapshot :: forall r e. Histogram -> RIO r e Snapshot
snapshot (Histogram h) = liftEffect do
  buckets <- Ref.read h.buckets
  count <- Ref.read h.count
  sum <- Ref.read h.sumRef
  pure { boundaries: h.boundaries, buckets, count, sum }

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
   . Histogram
  -> RIO (clock :: Clock | r) e a
  -> RIO (clock :: Clock | r) e a
withTimer histogram action = do
  Milliseconds startMs <- now
  ensuringWith action \_ -> do
    Milliseconds endMs <- now
    observe (endMs - startMs) histogram
