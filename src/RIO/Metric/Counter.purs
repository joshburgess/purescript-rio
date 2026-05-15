-- | A first-class monotonically-increasing counter.
-- |
-- | Where `RIO.Metrics.incrementCounter` writes name-keyed
-- | emissions to a `Metrics` service backend, a `Counter` is a
-- | concrete in-memory aggregator: allocate one with `make`,
-- | bump it from anywhere that can run an `RIO`, and read its
-- | current total with `value`.
-- |
-- | Counters never decrease; their natural use is for
-- | event-count metrics ("requests served", "messages
-- | published"). For a value that can move in either direction,
-- | reach for `RIO.Metric.Gauge` instead.
-- |
-- | Intended import:
-- |
-- | ```purescript
-- | import RIO.Metric.Counter (Counter)
-- | import RIO.Metric.Counter as Counter
-- |
-- | c <- Counter.make
-- | Counter.increment c
-- | n <- Counter.value c
-- | ```
module RIO.Metric.Counter
  ( Counter
  , make
  , increment
  , incrementBy
  , value
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Core (RIO)

newtype Counter = Counter (Ref Number)

-- | Allocate a fresh counter initialised to zero.
make :: forall r e. RIO r e Counter
make = liftEffect (Counter <$> Ref.new 0.0)

-- | Add 1 to the counter.
increment :: forall r e. Counter -> RIO r e Unit
increment = incrementBy 1.0

-- | Add `delta` to the counter. `delta` is expected to be
-- | non-negative; passing a negative value is not prevented but
-- | will surprise downstream readers who assume monotonicity.
incrementBy :: forall r e. Number -> Counter -> RIO r e Unit
incrementBy delta (Counter ref) =
  liftEffect (Ref.modify_ (_ + delta) ref)

-- | Read the counter's current total.
value :: forall r e. Counter -> RIO r e Number
value (Counter ref) = liftEffect (Ref.read ref)
