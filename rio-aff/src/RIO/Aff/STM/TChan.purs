-- | An unbounded transactional FIFO channel.
-- |
-- | Built on a pair of `TVar`s that hold pointers into a singly
-- | linked list of cells. Writes append a new "hole" at the tail;
-- | reads advance the read pointer along the chain. The implementation
-- | mirrors the classic GHC `TChan`. `readTChan` retries when empty;
-- | `writeTChan` never blocks (the channel is unbounded).
module RIO.Aff.STM.TChan
  ( TChan
  , newTChan
  , readTChan
  , tryReadTChan
  , writeTChan
  , peekTChan
  , isEmptyTChan
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import RIO.Aff.STM (STM, TVar, newTVar, readTVar, retry, writeTVar)

data TList a = TNil | TCons a (TVar (TList a))

-- | An unbounded FIFO channel. The read pointer points at a TVar
-- | holding the front of the list; the write pointer points at the
-- | TVar holding the tail "hole" (currently `TNil`).
newtype TChan a = TChan
  { readPtr :: TVar (TVar (TList a))
  , writePtr :: TVar (TVar (TList a))
  }

-- | A fresh empty channel.
newTChan :: forall e a. STM e (TChan a)
newTChan = do
  hole <- newTVar TNil
  readPtr <- newTVar hole
  writePtr <- newTVar hole
  pure (TChan { readPtr, writePtr })

-- | Read the next element. Retries when the channel is empty.
readTChan :: forall e a. TChan a -> STM e a
readTChan (TChan ch) = do
  current <- readTVar ch.readPtr
  lst <- readTVar current
  case lst of
    TNil -> retry
    TCons a nextRef -> do
      writeTVar ch.readPtr nextRef
      pure a

-- | Try to read without retrying. `Nothing` if empty.
tryReadTChan :: forall e a. TChan a -> STM e (Maybe a)
tryReadTChan (TChan ch) = do
  current <- readTVar ch.readPtr
  lst <- readTVar current
  case lst of
    TNil -> pure Nothing
    TCons a nextRef -> do
      writeTVar ch.readPtr nextRef
      pure (Just a)

-- | Append a value. Never blocks.
writeTChan :: forall e a. TChan a -> a -> STM e Unit
writeTChan (TChan ch) a = do
  newHole <- newTVar TNil
  current <- readTVar ch.writePtr
  writeTVar current (TCons a newHole)
  writeTVar ch.writePtr newHole

-- | Peek at the next element without consuming. Retries when empty.
peekTChan :: forall e a. TChan a -> STM e a
peekTChan (TChan ch) = do
  current <- readTVar ch.readPtr
  lst <- readTVar current
  case lst of
    TNil -> retry
    TCons a _ -> pure a

-- | Predicate: is the channel currently empty?
isEmptyTChan :: forall e a. TChan a -> STM e Boolean
isEmptyTChan (TChan ch) = do
  current <- readTVar ch.readPtr
  lst <- readTVar current
  pure case lst of
    TNil -> true
    _ -> false
