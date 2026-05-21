-- | A transactional fixed-length array of `TVar`s.
-- |
-- | `TArray a` is an immutable array of mutable cells: the spine
-- | is fixed at construction time, but each slot can be read and
-- | written transactionally. Out-of-bounds operations return
-- | `Nothing` rather than retrying.
module RIO.Fiber.STM.TArray
  ( TArray
  , make
  , replicate
  , length
  , read
  , write
  , modify
  , swap
  , freeze
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM

-- | A fixed-length array of `TVar`s.
newtype TArray a = TArray (Array (TVar a))

-- | Build a `TArray` from an array of seed values. One TVar is
-- | allocated per element.
make :: forall a. Array a -> Effect (TArray a)
make xs = TArray <$> traverse STM.newTVar xs

-- | Build a `TArray` of the given size, all slots initialized to the
-- | same value.
replicate :: forall a. Int -> a -> Effect (TArray a)
replicate n a = make (Array.replicate n a)

-- | The number of slots in the array. Pure: the spine is fixed.
length :: forall a. TArray a -> Int
length (TArray xs) = Array.length xs

-- | Read the slot at the given index. `Nothing` for out-of-bounds.
read :: forall a. TArray a -> Int -> STM (Maybe a)
read (TArray xs) i = case Array.index xs i of
  Nothing -> pure Nothing
  Just tv -> Just <$> STM.readTVar tv

-- | Write the slot at the given index. No-op for out-of-bounds.
write :: forall a. TArray a -> Int -> a -> STM Unit
write (TArray xs) i a = case Array.index xs i of
  Nothing -> pure unit
  Just tv -> STM.writeTVar tv a

-- | Apply a function to the slot at the given index. No-op for
-- | out-of-bounds.
modify :: forall a. TArray a -> Int -> (a -> a) -> STM Unit
modify (TArray xs) i f = case Array.index xs i of
  Nothing -> pure unit
  Just tv -> STM.modifyTVar tv f

-- | Swap the values at indices `i` and `j` atomically. Returns
-- | `true` only when both indices are in bounds; if either is out
-- | of bounds the array is unchanged and `false` is returned.
swap :: forall a. TArray a -> Int -> Int -> STM Boolean
swap (TArray xs) i j = case Array.index xs i, Array.index xs j of
  Just ti, Just tj -> do
    ai <- STM.readTVar ti
    aj <- STM.readTVar tj
    STM.writeTVar ti aj
    STM.writeTVar tj ai
    pure true
  _, _ -> pure false

-- | Read every slot atomically and return a plain array snapshot.
freeze :: forall a. TArray a -> STM (Array a)
freeze (TArray xs) = traverse STM.readTVar xs
