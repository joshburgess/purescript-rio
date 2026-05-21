-- | A bounded resource pool keyed by some user-supplied key.
-- |
-- | A `KeyedPool` is a map from key to `Pool`: the first borrow for a
-- | new key lazily constructs the pool for that key (using `acquire`
-- | parameterized by the key and the configured per-key `capacity`);
-- | subsequent borrows reuse the same pool. This is the natural shape
-- | for "connection pool per host" or "worker pool per tenant"
-- | patterns, where you want bounded concurrency per key but
-- | unbounded aggregate concurrency across keys.
-- |
-- | Borrows from different keys do not contend; the internal lock is
-- | held only for the get-or-create lookup. Within a single key,
-- | semantics match `Pool.withResource` exactly.
-- |
-- | Lifecycle is tied to the `Scope` passed to `make`. When that
-- | scope closes, every per-key pool is drained via the
-- | scope-finalizer it registered at construction; `release` runs on
-- | every idle resource, and resources still in flight release on
-- | return. `shutdown` exposes the same drain action explicitly:
-- | every currently-known per-key pool is drained, and subsequent
-- | borrows for the same key rebuild the pool lazily.
-- |
-- | `withResource'` exposes the per-borrow `invalidate` action from
-- | `Pool.withResource'`. `makeWithTTL` constructs a keyed pool whose
-- | per-key pools all share a single idle TTL.
module RIO.Aff.KeyedPool
  ( KeyedPool
  , KeyedPoolConfig
  , KeyedPoolConfigWithTTL
  , make
  , makeWithTTL
  , withResource
  , withResource'
  , shutdown
  , keys
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, ask)
import RIO.Aff.Pool (Pool)
import RIO.Aff.Pool as Pool
import RIO.Aff.Resource (Scope)
import RIO.Aff.Semaphore (Semaphore)
import RIO.Aff.Semaphore as Semaphore

-- | Configuration for `make`.
-- |
-- | * `acquire k` mints a new resource for `k`. Runs in `Aff`.
-- | * `release` returns a resource to the underlying system. Runs
-- |   uninterruptibly when the parent scope closes (and on per-borrow
-- |   eviction when the pool is shutting down).
-- | * `capacity k` caps simultaneous borrowers for `k`.
type KeyedPoolConfig k a =
  { acquire :: k -> Aff a
  , release :: a -> Aff Unit
  , capacity :: k -> Int
  }

-- | Configuration for `makeWithTTL`. Same shape as `KeyedPoolConfig`
-- | with an extra `timeToLive`: every per-key pool will evict an
-- | idle resource whose age exceeds the TTL on the next borrow.
type KeyedPoolConfigWithTTL k a =
  { acquire :: k -> Aff a
  , release :: a -> Aff Unit
  , capacity :: k -> Int
  , timeToLive :: Milliseconds
  }

-- | An opaque keyed resource pool. Construct with `make` or
-- | `makeWithTTL`.
newtype KeyedPool k a = KeyedPool
  { pools :: Ref (Map k (Pool a))
  , sem :: Semaphore
  , scope :: Scope
  , acquire :: k -> Aff a
  , release :: a -> Aff Unit
  , capacity :: k -> Int
  , ttl :: Maybe Milliseconds
  , nowSource :: Aff Milliseconds
  }

-- | Allocate a keyed pool tied to `scope`. Per-key pools are created
-- | lazily on first `withResource` for a new key; every per-key pool
-- | shares `scope`, so closing that scope drains them all. Idle
-- | resources never expire.
make
  :: forall r e k a
   . Scope
  -> KeyedPoolConfig k a
  -> RIO r e (KeyedPool k a)
make scope config = do
  pools <- liftEffect (Ref.new Map.empty)
  sem <- liftEffect (Semaphore.make 1)
  pure
    ( KeyedPool
        { pools
        , sem
        , scope
        , acquire: config.acquire
        , release: config.release
        , capacity: config.capacity
        , ttl: Nothing
        , nowSource: pure (Milliseconds 0.0)
        }
    )

-- | Allocate a keyed pool whose per-key pools each apply the given
-- | idle TTL. The wall-clock source is captured from the active
-- | `Clock` service once at construction, so subsequent borrows
-- | (which run at the caller's row, not necessarily with `clock`
-- | available) all consult the same time source.
makeWithTTL
  :: forall r e k a
   . Scope
  -> KeyedPoolConfigWithTTL k a
  -> RIO (clock :: Clock | r) e (KeyedPool k a)
makeWithTTL scope config = do
  clock <- ask (Proxy :: Proxy "clock")
  pools <- liftEffect (Ref.new Map.empty)
  sem <- liftEffect (Semaphore.make 1)
  pure
    ( KeyedPool
        { pools
        , sem
        , scope
        , acquire: config.acquire
        , release: config.release
        , capacity: config.capacity
        , ttl: Just config.timeToLive
        , nowSource: clock.now
        }
    )

-- | Borrow a resource for the given key. The first borrow for a new
-- | key lazily constructs that key's pool; subsequent borrows reuse
-- | it. Within a single key, semantics match `Pool.withResource`.
withResource
  :: forall r e k a b
   . Ord k
  => KeyedPool k a
  -> k
  -> (a -> RIO r e b)
  -> RIO r e b
withResource kp k use = do
  pool <- getOrCreate kp k
  Pool.withResource pool use

-- | Borrow a resource for the given key, exposing a per-borrow
-- | `invalidate` action. See `Pool.withResource'`.
withResource'
  :: forall r e k a b
   . Ord k
  => KeyedPool k a
  -> k
  -> (a -> RIO r e Unit -> RIO r e b)
  -> RIO r e b
withResource' kp k use = do
  pool <- getOrCreate kp k
  Pool.withResource' pool use

-- | Run `Pool.shutdown` on every currently-known per-key pool. The
-- | map is cleared, so new borrows after `shutdown` rebuild the
-- | pool lazily. As with `Pool.shutdown`, this only opens the drain
-- | sooner: even without an explicit call, the parent scope's
-- | finalizer will run the same drain when the scope exits.
shutdown :: forall r e k a. KeyedPool k a -> RIO r e Unit
shutdown (KeyedPool kp) = Semaphore.withPermit kp.sem do
  m <- liftEffect (Ref.read kp.pools)
  liftEffect (Ref.write Map.empty kp.pools)
  traverse_ Pool.shutdown (Map.values m)

-- | The set of keys for which a per-key pool currently exists.
-- | Useful for inspection in tests.
keys :: forall r e k a. KeyedPool k a -> RIO r e (Set k)
keys (KeyedPool kp) = do
  m <- liftEffect (Ref.read kp.pools)
  pure (Map.keys m)

getOrCreate
  :: forall r e k a
   . Ord k
  => KeyedPool k a
  -> k
  -> RIO r e (Pool a)
getOrCreate (KeyedPool kp) k = Semaphore.withPermit kp.sem do
  m <- liftEffect (Ref.read kp.pools)
  case Map.lookup k m of
    Just p -> pure p
    Nothing -> do
      p <- case kp.ttl of
        Just t -> Pool.makeWithTimeSource kp.scope
          { acquire: kp.acquire k
          , release: kp.release
          , maxSize: kp.capacity k
          , timeToLive: t
          , nowSource: kp.nowSource
          }
        Nothing -> Pool.make kp.scope
          { acquire: kp.acquire k
          , release: kp.release
          , maxSize: kp.capacity k
          }
      liftEffect (Ref.write (Map.insert k p m) kp.pools)
      pure p
