-- | An unbounded FIFO queue built on a single `TRef (Array a)`.
-- |
-- | Producers enqueue with `writeTQueue`; consumers dequeue with
-- | `readTQueue`, which retries when the queue is empty and wakes
-- | up automatically the next time a producer commits. The
-- | underlying STM semantics carry over directly: every operation
-- | is atomic, and orthogonal `TQueue` operations compose into a
-- | single transaction without interleaving.
-- |
-- | The implementation uses `Array.snoc` for enqueue and
-- | `Array.uncons` for dequeue. On the JS backend both are O(n),
-- | so this queue is best suited to small or medium working sets;
-- | a deque-based implementation can replace it without changing
-- | the public API.
module RIO.STM.TQueue
  ( TQueue
  , isEmptyTQueue
  , lengthTQueue
  , newTQueue
  , peekTQueue
  , readTQueue
  , tryReadTQueue
  , writeTQueue
  ) where

import Prelude

import Data.Array (length, snoc, uncons) as Array
import Data.Maybe (Maybe(..))

import RIO.STM (STM, TRef, modifyTRef, newTRef, readTRef, retry, writeTRef)

-- | A FIFO queue with unbounded capacity. Constructor hidden;
-- | identity is the underlying `TRef`.
newtype TQueue :: Type -> Type
newtype TQueue a = TQueue (TRef (Array a))

-- | Allocate a fresh empty queue.
newTQueue :: forall e a. STM e (TQueue a)
newTQueue = TQueue <$> newTRef []

-- | Enqueue a value at the back. Never blocks.
writeTQueue :: forall e a. TQueue a -> a -> STM e Unit
writeTQueue (TQueue ref) a = modifyTRef ref (\xs -> Array.snoc xs a)

-- | Dequeue a value from the front. Retries (waits) when the
-- | queue is empty; wakes up when a producer enqueues.
readTQueue :: forall e a. TQueue a -> STM e a
readTQueue (TQueue ref) = do
  xs <- readTRef ref
  case Array.uncons xs of
    Nothing -> retry
    Just { head, tail } -> do
      writeTRef ref tail
      pure head

-- | Non-blocking dequeue. `Nothing` when the queue is empty.
tryReadTQueue :: forall e a. TQueue a -> STM e (Maybe a)
tryReadTQueue (TQueue ref) = do
  xs <- readTRef ref
  case Array.uncons xs of
    Nothing -> pure Nothing
    Just { head, tail } -> do
      writeTRef ref tail
      pure (Just head)

-- | Look at the value at the front without removing it. Retries
-- | when the queue is empty.
peekTQueue :: forall e a. TQueue a -> STM e a
peekTQueue (TQueue ref) = do
  xs <- readTRef ref
  case Array.uncons xs of
    Nothing -> retry
    Just { head } -> pure head

-- | True when the queue has no elements.
isEmptyTQueue :: forall e a. TQueue a -> STM e Boolean
isEmptyTQueue (TQueue ref) = do
  xs <- readTRef ref
  pure (Array.length xs == 0)

-- | Current size.
lengthTQueue :: forall e a. TQueue a -> STM e Int
lengthTQueue (TQueue ref) = Array.length <$> readTRef ref
