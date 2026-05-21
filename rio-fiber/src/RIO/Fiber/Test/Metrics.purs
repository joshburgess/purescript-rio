-- | An in-memory event log for recording metric emissions in tests.
-- |
-- | `RIO.Fiber.Metrics` exposes in-process metric primitives
-- | (`Counter`, `Gauge`, `Histogram`, `BucketHistogram`) as
-- | concrete values rather than service records, so the
-- | rio-aff-style "swap in a recording backend" approach doesn't
-- | apply directly. This module fills the gap with a small
-- | event log: each `recordCounter` / `recordGauge` /
-- | `recordHistogram` call appends a `MetricRecord` to a Ref;
-- | `snapshot` returns the full ordered log.
-- |
-- | The recorder is intended for two cases:
-- |
-- |   1. Asserting on emission order and values in unit tests
-- |      where the unit under test owns the metric handles.
-- |   2. Driving `RIO.Fiber.Metric.OTel.exportMetrics` from
-- |      fixture data without booting a real exporter pipeline.
-- |
-- | Production code that wants both in-process primitives and
-- | export visibility can call the primitive's update
-- | (`Metrics.incrBy`, `Metrics.record`, ...) and the recorder's
-- | `recordCounter` / `recordHistogram` in the same step.
module RIO.Fiber.Test.Metrics
  ( MetricKind(..)
  , MetricRecord
  , RecordingMetrics
  , newRecordingMetrics
  ) where

import Prelude

import Data.Array (snoc) as Array
import Effect (Effect)
import Effect.Ref as Ref

-- | Which family of metric a record belongs to. Tests usually
-- | inspect both the kind and the name when asserting on a
-- | program's emissions.
data MetricKind = Counter | Gauge | Histogram

derive instance eqMetricKind :: Eq MetricKind

instance showMetricKind :: Show MetricKind where
  show = case _ of
    Counter -> "Counter"
    Gauge -> "Gauge"
    Histogram -> "Histogram"

-- | One recorded emission.
type MetricRecord =
  { kind :: MetricKind
  , name :: String
  , value :: Number
  }

-- | The recorder paired with a snapshot reader. The three
-- | `record*` actions are intended to be called from
-- | application code at the same site that updates the
-- | corresponding primitive in `RIO.Fiber.Metrics`.
type RecordingMetrics =
  { recordCounter :: String -> Number -> Effect Unit
  , recordGauge :: String -> Number -> Effect Unit
  , recordHistogram :: String -> Number -> Effect Unit
  , snapshot :: Effect (Array MetricRecord)
  }

-- | Allocate a fresh recording log.
newRecordingMetrics :: Effect RecordingMetrics
newRecordingMetrics = do
  recordsRef <- Ref.new ([] :: Array MetricRecord)
  let
    append :: MetricKind -> String -> Number -> Effect Unit
    append kind name value =
      Ref.modify_
        (\xs -> Array.snoc xs { kind, name, value })
        recordsRef

    snapshot :: Effect (Array MetricRecord)
    snapshot = Ref.read recordsRef
  pure
    { recordCounter: append Counter
    , recordGauge: append Gauge
    , recordHistogram: append Histogram
    , snapshot
    }
