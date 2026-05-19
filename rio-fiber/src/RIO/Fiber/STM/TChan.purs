-- | An unbounded transactional FIFO channel.
-- |
-- | Built on a pair of `TVar`s that hold pointers into a singly
-- | linked list of cells. Writes append a new "hole" at the tail;
-- | reads advance the read pointer along the chain. The implementation
-- | mirrors the classic GHC `TChan`. `readTChan` retries when empty;
-- | `writeTChan` never blocks (the channel is unbounded).
module RIO.Fiber.STM.TChan
  ( TChan
  , new
  , readTChan
  , tryReadTChan
  , writeTChan
  , peekTChan
  , isEmptyTChan
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM

data TList a = TNil | TCons a (TVar (TList a))

-- | An unbounded FIFO channel. The read pointer points at a TVar
-- | holding the front of the list; the write pointer points at the
-- | TVar holding the tail "hole" (currently `TNil`).
newtype TChan a = TChan
  { readPtr :: TVar (TVar (TList a))
  , writePtr :: TVar (TVar (TList a))
  }

-- | A fresh empty channel.
new :: forall a. Effect (TChan a)
new = do
  hole <- STM.newTVar TNil
  readPtr <- STM.newTVar hole
  writePtr <- STM.newTVar hole
  pure (TChan { readPtr, writePtr })

-- | Read the next element. Retries when the channel is empty.
readTChan :: forall a. TChan a -> STM a
readTChan (TChan ch) = do
  current <- STM.readTVar ch.readPtr
  lst <- STM.readTVar current
  case lst of
    TNil -> STM.retry
    TCons a nextRef -> do
      STM.writeTVar ch.readPtr nextRef
      pure a

-- | Try to read without retrying. `Nothing` if empty.
tryReadTChan :: forall a. TChan a -> STM (Maybe a)
tryReadTChan (TChan ch) = do
  current <- STM.readTVar ch.readPtr
  lst <- STM.readTVar current
  case lst of
    TNil -> pure Nothing
    TCons a nextRef -> do
      STM.writeTVar ch.readPtr nextRef
      pure (Just a)

-- | Append a value. Never blocks.
writeTChan :: forall a. TChan a -> a -> STM Unit
writeTChan (TChan ch) a = do
  newHole <- STM.newTVarSTM TNil
  current <- STM.readTVar ch.writePtr
  STM.writeTVar current (TCons a newHole)
  STM.writeTVar ch.writePtr newHole

-- | Peek at the next element without consuming. Retries when empty.
peekTChan :: forall a. TChan a -> STM a
peekTChan (TChan ch) = do
  current <- STM.readTVar ch.readPtr
  lst <- STM.readTVar current
  case lst of
    TNil -> STM.retry
    TCons a _ -> pure a

-- | Predicate: is the channel currently empty?
isEmptyTChan :: forall a. TChan a -> STM Boolean
isEmptyTChan (TChan ch) = do
  current <- STM.readTVar ch.readPtr
  lst <- STM.readTVar current
  pure case lst of
    TNil -> true
    _ -> false
