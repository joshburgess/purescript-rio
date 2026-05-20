-- | A transactional array of fixed length, backed by a single
-- | `TRef (Array a)`. Every cell shares one underlying `TRef`, so
-- | a write to any index wakes every transaction that read any
-- | part of the array.
-- |
-- | Use `TArray` for "shared workspace" patterns: a queue of slots
-- | indexed by worker id, a finite ring buffer, a parallel-fold
-- | accumulator with one cell per partition. For map-shaped state
-- | reach for `RIO.Aff.STM.TMap` instead; for an unbounded growable
-- | list use a `TRef (Array a)` directly.
-- |
-- | ## Coarse-grained transactions
-- |
-- | Because every cell lives in one `TRef`, a write to index `i`
-- | causes a `retry` waiting on a read of *any* index `j` to fire
-- | once `i = j` is *not* required for wake-up. This is the
-- | standard STM "any write to a transaction's read set wakes
-- | retry" semantics; it is not an indexed-cell wakeup. For
-- | per-cell granularity you would need one `TRef` per slot.
module RIO.Aff.STM.TArray
  ( TArray
  , fromArrayTArray
  , lengthTArray
  , modifyTArray
  , newTArray
  , readTArray
  , swapTArray
  , toArrayTArray
  , writeTArray
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))

import RIO.Aff.STM (STM, TRef, modifyTRef, newTRef, readTRef)

-- | A transactional array. Constructor hidden; identity is the
-- | underlying `TRef`.
newtype TArray :: Type -> Type
newtype TArray a = TArray (TRef (Array a))

-- | Allocate a fresh transactional array of length `n` with every
-- | cell initialised to `value`. A negative `n` produces an empty
-- | array.
newTArray :: forall e a. Int -> a -> STM e (TArray a)
newTArray n value = TArray <$> newTRef (Array.replicate n value)

-- | Wrap an existing array as a transactional one. The underlying
-- | array is held by value, so subsequent updates to it outside
-- | the transaction do not affect the `TArray`.
fromArrayTArray :: forall e a. Array a -> STM e (TArray a)
fromArrayTArray xs = TArray <$> newTRef xs

-- | Number of cells.
lengthTArray :: forall e a. TArray a -> STM e Int
lengthTArray (TArray ref) = Array.length <$> readTRef ref

-- | Read the cell at index `i`. Returns `Nothing` if `i` is out
-- | of bounds.
readTArray :: forall e a. Int -> TArray a -> STM e (Maybe a)
readTArray i (TArray ref) = Array.index <$> readTRef ref <@> i

-- | Overwrite the cell at index `i`. Returns `True` on success,
-- | `False` when `i` is out of bounds (the transaction is not
-- | aborted; the caller can branch on the result).
writeTArray :: forall e a. Int -> a -> TArray a -> STM e Boolean
writeTArray i value (TArray ref) = do
  xs <- readTRef ref
  case Array.updateAt i value xs of
    Nothing -> pure false
    Just xs' -> do
      modifyTRef ref (\_ -> xs')
      pure true

-- | Apply a pure function to the cell at index `i`. Returns `True`
-- | on success, `False` when `i` is out of bounds.
modifyTArray
  :: forall e a
   . Int
  -> (a -> a)
  -> TArray a
  -> STM e Boolean
modifyTArray i f (TArray ref) = do
  xs <- readTRef ref
  case Array.modifyAt i f xs of
    Nothing -> pure false
    Just xs' -> do
      modifyTRef ref (\_ -> xs')
      pure true

-- | Swap the values at indices `i` and `j`. Returns `True` only
-- | when both indices are in bounds; if either is out of bounds
-- | the array is unchanged and `False` is returned.
swapTArray :: forall e a. Int -> Int -> TArray a -> STM e Boolean
swapTArray i j (TArray ref) = do
  xs <- readTRef ref
  case Array.index xs i, Array.index xs j of
    Just ai, Just aj -> do
      case Array.updateAt i aj xs of
        Nothing -> pure false
        Just xs1 -> case Array.updateAt j ai xs1 of
          Nothing -> pure false
          Just xs2 -> do
            modifyTRef ref (\_ -> xs2)
            pure true
    _, _ -> pure false

-- | Read a snapshot of every cell into a plain `Array`. The
-- | snapshot is consistent because the read happens inside one
-- | transaction.
toArrayTArray :: forall e a. TArray a -> STM e (Array a)
toArrayTArray (TArray ref) = readTRef ref
