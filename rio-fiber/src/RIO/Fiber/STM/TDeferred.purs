-- | A transactional one-shot cell.
-- |
-- | A `TDeferred a` starts empty. The first `complete` wins and
-- | stores a value; subsequent `complete` calls return `false`
-- | without changing the stored value. `await` retries until the
-- | cell is set; `poll` reads the cell non-blockingly.
-- |
-- | Unlike `RIO.Fiber.Deferred`, which is fiber-level and uses
-- | observer callbacks, `TDeferred` is purely STM: completion and
-- | observation participate in transactions and roll back on
-- | retry. Useful when the act of fulfilling a one-shot result
-- | should be atomic with the rest of a transaction (e.g.
-- | dequeuing an item and recording it in a result cell in one
-- | atomic step).
module RIO.Fiber.STM.TDeferred
  ( TDeferred
  , make
  , complete
  , await
  , poll
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM

-- | A write-once transactional cell.
newtype TDeferred a = TDeferred (TVar (Maybe a))

-- | A fresh empty cell.
make :: forall a. Effect (TDeferred a)
make = TDeferred <$> STM.newTVar Nothing

-- | Complete the cell with `a`. Returns `true` if this call won
-- | the race (the cell was empty); `false` if a previous call
-- | already set the cell. In the `false` case the existing value
-- | is preserved.
complete :: forall a. TDeferred a -> a -> STM Boolean
complete (TDeferred tv) a = do
  m <- STM.readTVar tv
  case m of
    Just _ -> pure false
    Nothing -> do
      STM.writeTVar tv (Just a)
      pure true

-- | Wait for the cell to be set, then read its value. Retries
-- | while the cell is empty.
await :: forall a. TDeferred a -> STM a
await (TDeferred tv) = do
  m <- STM.readTVar tv
  case m of
    Nothing -> STM.retry
    Just a -> pure a

-- | Non-blocking read. `Nothing` if the cell is still empty.
poll :: forall a. TDeferred a -> STM (Maybe a)
poll (TDeferred tv) = STM.readTVar tv
