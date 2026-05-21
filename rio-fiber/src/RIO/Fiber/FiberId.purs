-- | A fiber's unique numeric identifier.
-- |
-- | This module is intentionally small so it can be imported by both
-- | `RIO.Fiber.Cause` (which carries a `FiberId` inside the
-- | `Interrupt` constructor for attribution) and `RIO.Fiber.Inspect`
-- | (which surfaces ids to users) without introducing a cycle through
-- | the runtime.
module RIO.Fiber.FiberId
  ( FiberId(..)
  , unFiberId
  , externalFiberId
  ) where

import Prelude

-- | A fiber's unique numeric identifier, assigned monotonically on
-- | creation. IDs are stable for the life of the fiber but are not
-- | reused across runtime restarts of the JS process.
-- |
-- | A negative id (specifically `-1`) is reserved as the sentinel
-- | `externalFiberId`: it represents "interrupt was issued from outside
-- | any running fiber" (for example via the `Fiber.interrupt` Effect
-- | API rather than from another fiber's `interrupt` op).
newtype FiberId = FiberId Int

derive newtype instance eqFiberId :: Eq FiberId
derive newtype instance ordFiberId :: Ord FiberId
derive newtype instance showFiberId :: Show FiberId

-- | Extract the raw integer from a `FiberId`.
unFiberId :: FiberId -> Int
unFiberId (FiberId n) = n

-- | Sentinel id used for interrupts originating outside any running
-- | fiber (e.g. `Fiber.interrupt` called from `Effect`-land).
externalFiberId :: FiberId
externalFiberId = FiberId (-1)
