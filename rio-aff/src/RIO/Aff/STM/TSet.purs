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
module RIO.Aff.STM.TSet
  ( TSet
  , deleteTSet
  , insertTSet
  , memberTSet
  , newTSet
  , nullTSet
  , sizeTSet
  , toArrayTSet
  ) where

import Prelude

import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set

import RIO.Aff.STM (STM, TVar, modifyTVar, newTVar, readTVar)

-- | A transactional set.
newtype TSet a = TSet (TVar (Set a))

-- | A fresh empty set.
newTSet :: forall e a. STM e (TSet a)
newTSet = TSet <$> newTVar Set.empty

-- | Insert an element. No-op when already present.
insertTSet :: forall e a. Ord a => a -> TSet a -> STM e Unit
insertTSet a (TSet tv) = modifyTVar tv (Set.insert a)

-- | Remove an element. No-op when absent.
deleteTSet :: forall e a. Ord a => a -> TSet a -> STM e Unit
deleteTSet a (TSet tv) = modifyTVar tv (Set.delete a)

-- | `true` iff the set contains the element.
memberTSet :: forall e a. Ord a => a -> TSet a -> STM e Boolean
memberTSet a (TSet tv) = Set.member a <$> readTVar tv

-- | Number of elements.
sizeTSet :: forall e a. TSet a -> STM e Int
sizeTSet (TSet tv) = Set.size <$> readTVar tv

-- | Snapshot the set as an array in order.
toArrayTSet :: forall e a. TSet a -> STM e (Array a)
toArrayTSet (TSet tv) = Array.fromFoldable <$> readTVar tv

-- | Predicate: is the set empty?
nullTSet :: forall e a. TSet a -> STM e Boolean
nullTSet (TSet tv) = Set.isEmpty <$> readTVar tv
