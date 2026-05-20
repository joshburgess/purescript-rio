-- | A first-class point-in-time value that can move in either
-- | direction.
-- |
-- | Where a `Counter` only goes up, a `Gauge` represents a
-- | sampled value: queue depth, free memory, open connections.
-- | Use `set` for an absolute write, `increment` and
-- | `decrement` for unit adjustments.
-- |
-- | Intended import:
-- |
-- | ```purescript
-- | import RIO.Aff.Metric.Gauge (Gauge)
-- | import RIO.Aff.Metric.Gauge as Gauge
-- |
-- | g <- Gauge.make
-- | Gauge.set 12.0 g
-- | Gauge.increment g
-- | n <- Gauge.value g
-- | ```
module RIO.Aff.Metric.Gauge
  ( Gauge
  , make
  , set
  , increment
  , decrement
  , adjust
  , value
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Core (RIO)

newtype Gauge = Gauge (Ref Number)

-- | Allocate a fresh gauge initialised to zero.
make :: forall r e. RIO r e Gauge
make = liftEffect (Gauge <$> Ref.new 0.0)

-- | Overwrite the gauge's value.
set :: forall r e. Number -> Gauge -> RIO r e Unit
set v (Gauge ref) = liftEffect (Ref.write v ref)

-- | Add 1 to the gauge.
increment :: forall r e. Gauge -> RIO r e Unit
increment = adjust 1.0

-- | Subtract 1 from the gauge.
decrement :: forall r e. Gauge -> RIO r e Unit
decrement = adjust (-1.0)

-- | Add `delta` to the gauge. Pass a negative value to
-- | subtract.
adjust :: forall r e. Number -> Gauge -> RIO r e Unit
adjust delta (Gauge ref) =
  liftEffect (Ref.modify_ (_ + delta) ref)

-- | Read the gauge's current value.
value :: forall r e. Gauge -> RIO r e Number
value (Gauge ref) = liftEffect (Ref.read ref)
