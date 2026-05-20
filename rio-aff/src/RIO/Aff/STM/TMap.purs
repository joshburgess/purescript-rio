-- | A transactional map keyed by an `Ord` type, backed by a single
-- | `TRef (Map k v)`. All operations are atomic; concurrent
-- | producers and consumers compose without races.
-- |
-- | `awaitKey` is the headline combinator: it retries until a key
-- | is present, then returns its value. Use it for "barrier on
-- | configuration" or "wait for handler registration" patterns.
module RIO.Aff.STM.TMap
  ( TMap
  , awaitKey
  , clearTMap
  , deleteTMap
  , entriesTMap
  , insertTMap
  , keysTMap
  , lookupTMap
  , memberTMap
  , newTMap
  , sizeTMap
  , updateTMap
  , valuesTMap
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)

import RIO.Aff.STM (STM, TRef, modifyTRef, newTRef, readTRef, retry, writeTRef)

-- | A transactional map. Constructor hidden; identity is the
-- | underlying `TRef`.
newtype TMap :: Type -> Type -> Type
newtype TMap k v = TMap (TRef (Map k v))

-- | Allocate a fresh empty map.
newTMap :: forall e k v. STM e (TMap k v)
newTMap = TMap <$> newTRef Map.empty

-- | Insert or overwrite the value at `k`.
insertTMap :: forall e k v. Ord k => k -> v -> TMap k v -> STM e Unit
insertTMap k v (TMap ref) = modifyTRef ref (Map.insert k v)

-- | Lookup `k`. Returns `Nothing` when absent.
lookupTMap :: forall e k v. Ord k => k -> TMap k v -> STM e (Maybe v)
lookupTMap k (TMap ref) = Map.lookup k <$> readTRef ref

-- | Remove the entry at `k`, if any.
deleteTMap :: forall e k v. Ord k => k -> TMap k v -> STM e Unit
deleteTMap k (TMap ref) = modifyTRef ref (Map.delete k)

-- | True when the map contains `k`.
memberTMap :: forall e k v. Ord k => k -> TMap k v -> STM e Boolean
memberTMap k (TMap ref) = Map.member k <$> readTRef ref

-- | Number of entries.
sizeTMap :: forall e k v. TMap k v -> STM e Int
sizeTMap (TMap ref) = Map.size <$> readTRef ref

-- | Retry until `k` is present, then return its value. Wakes up
-- | when any write to the underlying `TRef` fires (so an insert
-- | of a different key will re-check; this is the standard STM
-- | wakeup model, not an indexed one).
awaitKey :: forall e k v. Ord k => k -> TMap k v -> STM e v
awaitKey k (TMap ref) = do
  m <- readTRef ref
  case Map.lookup k m of
    Nothing -> retry
    Just v -> pure v

-- | All keys in ascending order (Map's natural order under `Ord k`).
keysTMap :: forall e k v. TMap k v -> STM e (Array k)
keysTMap t = map fst <$> entriesTMap t

-- | All values in key order.
valuesTMap :: forall e k v. TMap k v -> STM e (Array v)
valuesTMap t = map snd <$> entriesTMap t

-- | All (key, value) pairs in key order.
entriesTMap :: forall e k v. TMap k v -> STM e (Array (Tuple k v))
entriesTMap (TMap ref) = Map.toUnfoldable <$> readTRef ref

-- | Remove every entry from the map.
clearTMap :: forall e k v. TMap k v -> STM e Unit
clearTMap (TMap ref) = writeTRef ref Map.empty

-- | Apply a function to the value at `k`, if present. No-op when
-- | `k` is missing. To turn an absent key into a present one (or
-- | vice versa), use a hand-written transaction with `lookupTMap`
-- | and `insertTMap` / `deleteTMap`.
updateTMap
  :: forall e k v
   . Ord k
  => k
  -> (v -> v)
  -> TMap k v
  -> STM e Unit
updateTMap k f (TMap ref) = modifyTRef ref (Map.update (Just <<< f) k)
