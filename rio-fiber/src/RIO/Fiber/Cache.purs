-- | A memoizing cache with optional TTL and single-flight lookups.
-- |
-- | `Cache k v` wraps a user-supplied lookup function
-- | `k -> RIO r e v` with a map of stored results. The first `get`
-- | for a key runs the lookup; subsequent `get`s for the same key
-- | return the stored value without re-running it. When `timeToLive`
-- | is set, an entry is treated as a miss once its age exceeds the
-- | TTL.
-- |
-- | ## Single-flight semantics
-- |
-- | If two fibers call `get` for the same key concurrently and the
-- | entry is absent, only one runs the lookup; the other awaits the
-- | same in-flight `Deferred` and observes the same outcome. This
-- | prevents the "thundering herd" pattern where many concurrent
-- | misses each trigger an independent expensive lookup.
-- |
-- | If the lookup raises (typed failure, defect, or interrupt), the
-- | entry is evicted so the next `get` retries; pending awaiters all
-- | see the same outcome replayed.
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
module RIO.Fiber.Cache
  ( Cache
  , CacheConfig
  , make
  , get
  , invalidate
  , invalidateAll
  , size
  ) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now) as Now
import Effect.Ref as Ref

import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Core (RIO, causeOf, ensuring, fail, failCause, liftEffect)
import RIO.Fiber.Deferred (Deferred)
import RIO.Fiber.Deferred as Deferred
import RIO.Fiber.FiberId as FiberId

-- | Configuration for `make`.
-- |
-- | * `lookup` mints the value for a key. Runs in `RIO` so it can
-- |   block on network / file I/O.
-- | * `timeToLive` is the optional age after which an entry is
-- |   treated as a miss. `Nothing` means entries never expire on
-- |   their own (only `invalidate` / `invalidateAll` remove them).
type CacheConfig r e k v =
  { lookup :: k -> RIO r e v
  , timeToLive :: Maybe Milliseconds
  }

-- | An opaque memoizing cache. Construct with `make`.
newtype Cache r e k v = Cache
  { entries :: Ref.Ref (Map k (Entry e v))
  , lookup :: k -> RIO r e v
  , ttl :: Maybe Milliseconds
  }

-- An entry is a `Deferred` holding the lookup's outcome plus a
-- timestamp for TTL checks. The deferred's failure row is `()`
-- because the structured `Cause` rides in the success channel.
type Entry e v =
  { cell :: Deferred () (Either (Cause e) v)
  , storedAt :: Milliseconds
  }

-- | Construct a cache. Cache construction itself does not signal
-- | typed failures, so the error row is left polymorphic.
make
  :: forall r e e' k v
   . CacheConfig r e k v
  -> RIO r e' (Cache r e k v)
make config = liftEffect do
  entries <- Ref.new Map.empty
  pure
    ( Cache
        { entries
        , lookup: config.lookup
        , ttl: config.timeToLive
        }
    )

-- | Lookup `k`. On a hit, returns the cached value without running
-- | `lookup`. On a miss (or an expired entry), runs the configured
-- | `lookup` and stores the result.
-- |
-- | Concurrent misses for the same key share a single lookup
-- | (single-flight). If the lookup raises, the entry is evicted
-- | and every fiber waiting on the same cell sees the same outcome
-- | replayed.
get
  :: forall r e k v
   . Ord k
  => Cache r e k v
  -> k
  -> RIO r e v
get (Cache c) k = do
  Milliseconds nowMs <- liftEffect currentMs
  -- Synchronous check-or-install. Effect.Ref + Deferred.make both
  -- run in Effect, so the read + insert sequence cannot be
  -- interleaved with another fiber.
  decision <- liftEffect do
    m <- Ref.read c.entries
    case Map.lookup k m of
      Just entry
        | fresh c.ttl nowMs entry.storedAt -> pure (Awaiter entry.cell)
      _ -> do
        cell <- Deferred.make
        let newEntry = { cell, storedAt: Milliseconds nowMs }
        Ref.modify_ (Map.insert k newEntry) c.entries
        pure (Owner cell)
  case decision of
    Awaiter d -> Deferred.awaitPure d >>= reproduce
    Owner d -> runLookup c k d

-- The owning fiber: run the lookup, fill the cell so awaiters wake,
-- and on any non-success outcome evict the entry so future `get`s
-- retry. An ensuring clause handles the case where the owner is
-- interrupted before its causeOf can capture an outcome.
runLookup
  :: forall r e k v
   . Ord k
  => { entries :: Ref.Ref (Map k (Entry e v))
     , lookup :: k -> RIO r e v
     , ttl :: Maybe Milliseconds
     }
  -> k
  -> Deferred () (Either (Cause e) v)
  -> RIO r e v
runLookup c k d =
  ensuring
    ( do
        done <- Deferred.isDone d
        when (not done) do
          liftEffect (Ref.modify_ (Map.delete k) c.entries)
          void (Deferred.succeed d (Left (Cause.interrupt FiberId.externalFiberId)))
    )
    ( do
        outcome <- causeOf (c.lookup k)
        case outcome of
          Right _ -> pure unit
          Left _ -> liftEffect (Ref.modify_ (Map.delete k) c.entries)
        _ <- Deferred.succeed d outcome
        reproduce outcome
    )

reproduce :: forall r e a. Either (Cause e) a -> RIO r e a
reproduce = case _ of
  Right a -> pure a
  Left cause -> case Cause.firstFailure cause of
    Just v -> fail v
    Nothing -> failCause cause

data Decision e v
  = Awaiter (Deferred () (Either (Cause e) v))
  | Owner (Deferred () (Either (Cause e) v))

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
  :: forall r e e' k v
   . Ord k
  => Cache r e k v
  -> k
  -> RIO r e' Unit
invalidate (Cache c) k = liftEffect (Ref.modify_ (Map.delete k) c.entries)

-- | Drop every entry. A subsequent `get` for any key will run a
-- | fresh lookup.
invalidateAll :: forall r e e' k v. Cache r e k v -> RIO r e' Unit
invalidateAll (Cache c) = liftEffect (Ref.write Map.empty c.entries)

-- | The number of entries currently stored (including expired
-- | entries that have not yet been evicted by a later `get`).
size :: forall r e k v. Cache r e k v -> Effect Int
size (Cache c) = Map.size <$> Ref.read c.entries
