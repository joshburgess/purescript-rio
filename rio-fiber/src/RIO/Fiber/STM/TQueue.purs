-- | A bounded transactional FIFO queue.
-- |
-- | Built on a single `TVar` holding the item array plus the
-- | configured capacity. `writeTQueue` retries when the queue is
-- | full; `readTQueue` retries when it is empty. Composing these
-- | with `orElse` and `check` gives the usual bounded producer-
-- | consumer patterns without external coordination.
module RIO.Fiber.STM.TQueue
  ( TQueue
  , new
  , readTQueue
  , tryReadTQueue
  , writeTQueue
  , tryWriteTQueue
  , peekTQueue
  , lengthTQueue
  , capacityTQueue
  , isEmptyTQueue
  , isFullTQueue
  ) where

import Prelude

import Data.Array (length, snoc, uncons)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM

-- | A bounded FIFO queue carried in a single `TVar`.
newtype TQueue a = TQueue
  { items :: TVar (Array a)
  , capacity :: Int
  }

-- | Allocate a fresh queue with the given positive capacity. Inputs
-- | <= 0 clamp to 1.
new :: forall a. Int -> Effect (TQueue a)
new cap = do
  items <- STM.newTVar []
  pure (TQueue { items, capacity: max 1 cap })

-- | Dequeue the next element. Retries when empty.
readTQueue :: forall a. TQueue a -> STM a
readTQueue (TQueue q) = do
  xs <- STM.readTVar q.items
  case uncons xs of
    Nothing -> STM.retry
    Just { head, tail } -> do
      STM.writeTVar q.items tail
      pure head

-- | Try to dequeue without retrying. `Nothing` if empty.
tryReadTQueue :: forall a. TQueue a -> STM (Maybe a)
tryReadTQueue (TQueue q) = do
  xs <- STM.readTVar q.items
  case uncons xs of
    Nothing -> pure Nothing
    Just { head, tail } -> do
      STM.writeTVar q.items tail
      pure (Just head)

-- | Enqueue an element. Retries when the queue is at capacity.
writeTQueue :: forall a. TQueue a -> a -> STM Unit
writeTQueue (TQueue q) a = do
  xs <- STM.readTVar q.items
  STM.check (length xs < q.capacity)
  STM.writeTVar q.items (snoc xs a)

-- | Try to enqueue without retrying. Returns `false` when full.
tryWriteTQueue :: forall a. TQueue a -> a -> STM Boolean
tryWriteTQueue (TQueue q) a = do
  xs <- STM.readTVar q.items
  if length xs < q.capacity then do
    STM.writeTVar q.items (snoc xs a)
    pure true
  else
    pure false

-- | Peek at the next element without consuming. Retries when empty.
peekTQueue :: forall a. TQueue a -> STM a
peekTQueue (TQueue q) = do
  xs <- STM.readTVar q.items
  case uncons xs of
    Nothing -> STM.retry
    Just { head } -> pure head

-- | Current number of buffered items.
lengthTQueue :: forall a. TQueue a -> STM Int
lengthTQueue (TQueue q) = length <$> STM.readTVar q.items

-- | Configured maximum capacity.
capacityTQueue :: forall a. TQueue a -> Int
capacityTQueue (TQueue q) = q.capacity

-- | Predicate: is the queue currently empty?
isEmptyTQueue :: forall a. TQueue a -> STM Boolean
isEmptyTQueue (TQueue q) = do
  xs <- STM.readTVar q.items
  pure (length xs == 0)

-- | Predicate: is the queue currently full?
isFullTQueue :: forall a. TQueue a -> STM Boolean
isFullTQueue (TQueue q) = do
  xs <- STM.readTVar q.items
  pure (length xs >= q.capacity)
