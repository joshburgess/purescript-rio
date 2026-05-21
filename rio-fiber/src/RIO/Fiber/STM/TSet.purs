-- | A transactional set.
-- |
-- | A `TSet a` is a single `TVar (Set a)`. Every operation runs inside
-- | `STM`, so a transaction that touches several elements is atomic:
-- | a retry rolls back the whole update, including any per-element
-- | writes performed earlier in the transaction.
-- |
-- | Like `TMap`, the implementation stores the entire set inside one
-- | TVar. Writes to disjoint elements still conflict at the version-
-- | vector level. For low-to-moderate contention this is the right
-- | shape; for very high concurrent writes to disjoint elements,
-- | partition the set across multiple `TVar`s.
module RIO.Fiber.STM.TSet
  ( TSet
  , empty
  , insert
  , delete
  , member
  , size
  , toArray
  , null
  ) where

import Prelude

import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM

-- | A transactional set.
newtype TSet a = TSet (TVar (Set a))

-- | A fresh empty set.
empty :: forall a. Effect (TSet a)
empty = TSet <$> STM.newTVar Set.empty

-- | Insert an element. No-op when already present.
insert :: forall a. Ord a => a -> TSet a -> STM Unit
insert a (TSet tv) = STM.modifyTVar tv (Set.insert a)

-- | Remove an element. No-op when absent.
delete :: forall a. Ord a => a -> TSet a -> STM Unit
delete a (TSet tv) = STM.modifyTVar tv (Set.delete a)

-- | `true` iff the set contains the element.
member :: forall a. Ord a => a -> TSet a -> STM Boolean
member a (TSet tv) = Set.member a <$> STM.readTVar tv

-- | Number of elements.
size :: forall a. TSet a -> STM Int
size (TSet tv) = Set.size <$> STM.readTVar tv

-- | Snapshot the set as an array in order.
toArray :: forall a. TSet a -> STM (Array a)
toArray (TSet tv) = Array.fromFoldable <$> STM.readTVar tv

-- | Predicate: is the set empty?
null :: forall a. TSet a -> STM Boolean
null (TSet tv) = Set.isEmpty <$> STM.readTVar tv
