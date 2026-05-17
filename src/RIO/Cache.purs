-- | A memoizing cache with optional TTL and single-flight lookups.
-- |
-- | `Cache k v` wraps a user-supplied lookup function `k -> Aff v`
-- | with a map of stored results. The first `get` for a key runs
-- | the lookup; subsequent `get`s for the same key return the
-- | stored value without re-running it. When `timeToLive` is set,
-- | an entry is treated as a miss once its age exceeds the TTL.
-- |
-- | ## Single-flight semantics
-- |
-- | If two fibers call `get` for the same key concurrently and the
-- | entry is absent, only one runs the lookup; the other awaits
-- | the same in-flight `AVar` and observes the same result. This
-- | prevents the "thundering herd" pattern where many concurrent
-- | misses each trigger an independent expensive lookup.
-- |
-- | If the lookup raises (an `Aff` exception), the entry is
-- | evicted so the next `get` retries; pending awaiters all see
-- | the same error.
-- |
-- | ```purescript
-- | -- a cache that memoizes user-profile fetches for 60s
-- | program = do
-- |   cache <- Cache.make
-- |     { lookup: \userId -> fetchProfile userId
-- |     , timeToLive: Just (Milliseconds 60000.0)
-- |     }
-- |   profile <- Cache.get cache "alice"
-- |   sameProfile <- Cache.get cache "alice"   -- no fetch
-- |   pure { profile, sameProfile }
-- | ```
-- |
-- | ## Eviction
-- |
-- | This cache does not auto-evict expired entries on a timer.
-- | Expiry is observed lazily on `get`: an expired entry is
-- | overwritten by the fresh lookup. Use `invalidate` to evict a
-- | specific key, or `invalidateAll` to clear the whole map. If
-- | you need bounded capacity with LRU eviction, build it as a
-- | wrapper.
module RIO.Cache
  ( Cache
  , CacheConfig
  , make
  , get
  , invalidate
  , invalidateAll
  , size
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.AVar (empty, kill, status) as AVarEff
import Effect.AVar (AVarStatus(..))
import Effect.Aff (Aff, Milliseconds(..), attempt, finally, throwError)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (read, tryPut) as AVar
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now) as Now
import Effect.Ref as ERef
import Data.DateTime.Instant (unInstant)

import RIO.Internal (RIO(..), mkEffectRIO, mkRIO)

-- | Configuration for `make`.
-- |
-- | * `lookup` mints the value for a key. Runs in `Aff` so it can
-- |   block on network / file I/O.
-- | * `timeToLive` is the optional age after which an entry is
-- |   treated as a miss. `Nothing` means entries never expire on
-- |   their own (only `invalidate` / `invalidateAll` remove them).
type CacheConfig k v =
  { lookup :: k -> Aff v
  , timeToLive :: Maybe Milliseconds
  }

-- | An opaque memoizing cache. Construct with `make`.
newtype Cache k v = Cache
  { entries :: ERef.Ref (Map k (Entry v))
  , lookup :: k -> Aff v
  , ttl :: Maybe Milliseconds
  }

-- An entry is either a one-shot AVar awaiting the first lookup or
-- a settled `Either Error v` once the lookup completes. We keep a
-- timestamp for TTL checks.
type Entry v =
  { avar :: AVar (Either String v)
  , storedAt :: Milliseconds
  }

-- | Construct a cache. Pool construction itself does not signal
-- | typed failures, so the error row is left polymorphic.
make
  :: forall r e k v
   . CacheConfig k v
  -> RIO r e (Cache k v)
make config = mkEffectRIO \_ -> do
  entries <- ERef.new Map.empty
  pure
    ( Cache
        { entries
        , lookup: config.lookup
        , ttl: config.timeToLive
        }
    )

-- | Lookup `k`. On a hit, returns the cached value without
-- | running `lookup`. On a miss (or an expired entry), runs the
-- | configured `lookup` and stores the result.
-- |
-- | Concurrent misses for the same key share a single lookup
-- | (single-flight). If the lookup raises, the entry is evicted
-- | and the failure surfaces as an `Aff` exception (a defect on
-- | the caller's `e` row).
get
  :: forall r e k v
   . Ord k
  => Cache k v
  -> k
  -> RIO r e v
get cache@(Cache c) k = mkRIO \_ -> do
  Milliseconds nowMs <- liftEffect currentMs
  -- Synchronous check-or-install. Effect.Ref + Effect.AVar empty
  -- both run in Effect, so the read + insert sequence cannot be
  -- interleaved with another fiber.
  decision <- liftEffect do
    m <- ERef.read c.entries
    case Map.lookup k m of
      Just entry
        | fresh c.ttl nowMs entry.storedAt -> pure (Awaiter entry.avar)
      _ -> do
        avar <- AVarEff.empty
        let
          newEntry =
            { avar, storedAt: Milliseconds nowMs }
        ERef.modify_ (Map.insert k newEntry) c.entries
        pure (Owner avar)
  case decision of
    Awaiter avar -> do
      result <- AVar.read avar
      case result of
        Right v -> pure v
        Left msg -> throwError (error msg)
    Owner avar -> runLookup cache k avar

-- The owning fiber: run the lookup, store the result in the AVar
-- so awaiters wake, and propagate any failure as a defect.
runLookup
  :: forall k v
   . Ord k
  => Cache k v
  -> k
  -> AVar (Either String v)
  -> Aff v
runLookup (Cache c) k avar =
  let
    cleanup = do
      -- If we're killed before completing the lookup, evict the
      -- entry and kill the AVar so awaiters don't block forever.
      st <- liftEffect (AVarEff.status avar)
      case st of
        Empty -> do
          liftEffect (ERef.modify_ (Map.delete k) c.entries)
          liftEffect
            (AVarEff.kill (error "RIO.Cache: lookup interrupted") avar)
        _ -> pure unit

    body = do
      result <- attempt (c.lookup k)
      case result of
        Right v -> do
          _ <- AVar.tryPut (Right v) avar
          pure v
        Left err -> do
          -- Evict so the next `get` retries.
          liftEffect (ERef.modify_ (Map.delete k) c.entries)
          _ <- AVar.tryPut (Left (show err)) avar
          throwError err
  in
    finally cleanup body

data Decision v
  = Awaiter (AVar (Either String v))
  | Owner (AVar (Either String v))

fresh :: Maybe Milliseconds -> Number -> Milliseconds -> Boolean
fresh ttl nowMs (Milliseconds storedAt) = case ttl of
  Nothing -> true
  Just (Milliseconds ms) -> (nowMs - storedAt) < ms

currentMs :: Effect Milliseconds
currentMs = do
  i <- Now.now
  pure (unInstant i)

-- | Remove the entry at `k`, if any. A subsequent `get` will run
-- | a fresh lookup. Has no effect when the key is absent.
invalidate
  :: forall r e k v
   . Ord k
  => Cache k v
  -> k
  -> RIO r e Unit
invalidate (Cache c) k = mkEffectRIO \_ ->
  ERef.modify_ (Map.delete k) c.entries

-- | Drop every entry. A subsequent `get` for any key will run a
-- | fresh lookup.
invalidateAll :: forall r e k v. Cache k v -> RIO r e Unit
invalidateAll (Cache c) = mkEffectRIO \_ ->
  ERef.write Map.empty c.entries

-- | The number of entries currently stored (including expired
-- | entries that have not yet been evicted by a later `get`).
size :: forall k v. Cache k v -> Effect Int
size (Cache c) = Map.size <$> ERef.read c.entries
