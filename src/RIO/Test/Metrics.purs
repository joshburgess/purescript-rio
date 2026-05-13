-- | An in-memory `Metrics` backend for tests.
-- |
-- | `newRecordingMetrics` returns the service plus a `snapshot`
-- | action that returns every recorded emission in order. The
-- | recorded list contains the metric kind, name, and value so
-- | tests can assert on the full surface.
module RIO.Test.Metrics
  ( MetricKind(..)
  , MetricRecord
  , RecordingMetrics
  , newRecordingMetrics
  ) where

import Prelude

import Data.Array (snoc) as Array
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Metrics (Metrics)

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

-- | A `Metrics` service paired with a snapshot reader.
type RecordingMetrics =
  { metrics :: Metrics
  , snapshot :: Effect (Array MetricRecord)
  }

-- | Allocate a fresh recording backend.
newRecordingMetrics :: Aff RecordingMetrics
newRecordingMetrics = liftEffect do
  recordsRef <- Ref.new ([] :: Array MetricRecord)
  let
    append :: MetricKind -> String -> Number -> Effect Unit
    append kind name value =
      Ref.modify_
        (\xs -> Array.snoc xs { kind, name, value })
        recordsRef

    metrics :: Metrics
    metrics =
      { recordCounter: append Counter
      , recordGauge: append Gauge
      , recordHistogram: append Histogram
      }

    snapshot :: Effect (Array MetricRecord)
    snapshot = Ref.read recordsRef
  pure { metrics, snapshot }
