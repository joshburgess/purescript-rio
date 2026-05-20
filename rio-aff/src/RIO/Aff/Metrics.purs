-- | A `Metrics` service plus a noop implementation.
-- |
-- | The service exposes three record-shaped operations:
-- | `recordCounter` (monotonic count, always increasing),
-- | `recordGauge` (point-in-time value, can go up or down), and
-- | `recordHistogram` (observation for a distribution). Each takes
-- | a metric name and a `Number` value; the backend handles
-- | aggregation, tagging, and emission.
-- |
-- | This module deliberately stays minimal: the service shape plus a
-- | test-friendly recording backend
-- | (`RIO.Aff.Test.Metrics.newRecordingMetrics`). Production backends
-- | (OTel, StatsD, Prometheus push) can sit on top of the same
-- | record type without touching the call sites.
module RIO.Aff.Metrics
  ( Metrics
  , incrementCounter
  , noopMetrics
  , observeHistogram
  , recordCounter
  , recordGauge
  , recordHistogram
  , setGauge
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask)

-- | The service record. Every operation takes a metric name (the
-- | callsite's stable identifier) and a `Number` value; backends
-- | are free to attach process-level tags out of band.
type Metrics =
  { recordCounter :: String -> Number -> Effect Unit
  , recordGauge :: String -> Number -> Effect Unit
  , recordHistogram :: String -> Number -> Effect Unit
  }

-- | A metrics backend that records nothing. Use it as the default
-- | when a program doesn't need metrics in a particular
-- | environment (CLI tools, local builds), or in tests that need
-- | the row satisfied without asserting on emissions.
noopMetrics :: Metrics
noopMetrics =
  { recordCounter: \_ _ -> pure unit
  , recordGauge: \_ _ -> pure unit
  , recordHistogram: \_ _ -> pure unit
  }

-- | Add `delta` to the named counter. Idiomatic call sites use
-- | `incrementCounter` for the `+1` case.
recordCounter
  :: forall r e
   . String
  -> Number
  -> RIO (metrics :: Metrics | r) e Unit
recordCounter name delta = do
  m <- ask (Proxy :: Proxy "metrics")
  liftAff (liftEffect (m.recordCounter name delta))

-- | Add 1 to the named counter.
incrementCounter
  :: forall r e
   . String
  -> RIO (metrics :: Metrics | r) e Unit
incrementCounter name = recordCounter name 1.0

-- | Record the current value of a gauge. The most recent value
-- | wins; backends typically expose this as "the value at the time
-- | of last `set`".
recordGauge
  :: forall r e
   . String
  -> Number
  -> RIO (metrics :: Metrics | r) e Unit
recordGauge name value = do
  m <- ask (Proxy :: Proxy "metrics")
  liftAff (liftEffect (m.recordGauge name value))

-- | Alias for `recordGauge`; idiomatic at call sites that read as
-- | "set the queue depth gauge to N".
setGauge
  :: forall r e
   . String
  -> Number
  -> RIO (metrics :: Metrics | r) e Unit
setGauge = recordGauge

-- | Record an observation for a histogram. Backends bucket these
-- | for percentile / distribution reporting.
recordHistogram
  :: forall r e
   . String
  -> Number
  -> RIO (metrics :: Metrics | r) e Unit
recordHistogram name value = do
  m <- ask (Proxy :: Proxy "metrics")
  liftAff (liftEffect (m.recordHistogram name value))

-- | Alias for `recordHistogram`; idiomatic at call sites that read
-- | as "observe a 47ms latency".
observeHistogram
  :: forall r e
   . String
  -> Number
  -> RIO (metrics :: Metrics | r) e Unit
observeHistogram = recordHistogram
