-- | A transactional key-value map.
-- |
-- | A `TMap k v` is a single `TVar (Map k v)`. Every operation runs
-- | inside `STM`, so a transaction that touches several keys is
-- | atomic with the rest of its work: a retry rolls back the whole
-- | update, including any per-key writes performed earlier in the
-- | transaction.
-- |
-- | The implementation stores the entire map inside one TVar. That
-- | makes every write read-modify-write the whole map's reference
-- | (not its contents), so writes to disjoint keys still conflict at
-- | the version-vector level. For a hot path with high concurrent
-- | writes to different keys, prefer keying multiple `TVar`s with a
-- | constant outer index. For typical low-to-moderate contention this
-- | is the right shape.
module RIO.Fiber.STM.TMap
  ( TMap
  , empty
  , insert
  , delete
  , lookup
  , member
  , size
  , toArray
  , values
  , keys
  , modify
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM

-- | A transactional key-value map.
newtype TMap k v = TMap (TVar (Map k v))

-- | A fresh empty map.
empty :: forall k v. Effect (TMap k v)
empty = TMap <$> STM.newTVar Map.empty

-- | Insert / overwrite the value at `k`.
insert :: forall k v. Ord k => k -> v -> TMap k v -> STM Unit
insert k v (TMap tv) = STM.modifyTVar tv (Map.insert k v)

-- | Remove the entry at `k`. No-op when the key is absent.
delete :: forall k v. Ord k => k -> TMap k v -> STM Unit
delete k (TMap tv) = STM.modifyTVar tv (Map.delete k)

-- | Read the value at `k`, if present.
lookup :: forall k v. Ord k => k -> TMap k v -> STM (Maybe v)
lookup k (TMap tv) = Map.lookup k <$> STM.readTVar tv

-- | `true` iff the map has an entry for `k`.
member :: forall k v. Ord k => k -> TMap k v -> STM Boolean
member k (TMap tv) = Map.member k <$> STM.readTVar tv

-- | Number of entries.
size :: forall k v. TMap k v -> STM Int
size (TMap tv) = Map.size <$> STM.readTVar tv

-- | Snapshot the map as an array of `(k, v)` pairs in key order.
toArray :: forall k v. TMap k v -> STM (Array (Tuple k v))
toArray (TMap tv) = Map.toUnfoldable <$> STM.readTVar tv

-- | Snapshot the values in key order.
values :: forall k v. TMap k v -> STM (Array v)
values (TMap tv) = Array.fromFoldable <<< Map.values <$> STM.readTVar tv

-- | Snapshot the keys in order.
keys :: forall k v. TMap k v -> STM (Array k)
keys (TMap tv) = Array.fromFoldable <<< Map.keys <$> STM.readTVar tv

-- | Apply `f` to the value at `k` if present. When the function
-- | returns `Nothing`, the key is removed; when it returns `Just v`,
-- | the key is updated. No-op when `k` is absent.
modify
  :: forall k v
   . Ord k
  => k
  -> (v -> Maybe v)
  -> TMap k v
  -> STM Unit
modify k f (TMap tv) = STM.modifyTVar tv \m ->
  case Map.lookup k m of
    Nothing -> m
    Just v -> maybe (Map.delete k m) (\v' -> Map.insert k v' m) (f v)
