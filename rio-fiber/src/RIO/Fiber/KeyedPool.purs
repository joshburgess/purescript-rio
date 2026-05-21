-- | A bounded resource pool keyed by some user-supplied key.
-- |
-- | A `KeyedPool` is a map from key to `Pool`: the first borrow for a
-- | new key lazily constructs the pool for that key (using `create`
-- | parameterized by the key and the configured per-key capacity);
-- | subsequent borrows reuse the same pool. This is the natural shape
-- | for "connection pool per host" or "worker pool per tenant"
-- | patterns, where you want bounded concurrency per key but unbounded
-- | aggregate concurrency across keys.
-- |
-- | Borrows from different keys do not contend; the internal lock is
-- | held only for the get-or-create lookup. Within a single key,
-- | semantics match `Pool.withResource` exactly.
-- |
-- | The MVP keeps the model simple: there is no per-key shutdown, and
-- | `shutdown` runs `Pool.shutdown` on every currently-known per-key
-- | pool. Idle TTL (when configured) is shared by every per-key pool.
module RIO.Fiber.KeyedPool
  ( KeyedPool
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
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse_)
import Effect.Ref as ERef
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Pool (Pool)
import RIO.Fiber.Pool as Pool
import RIO.Fiber.Semaphore (Semaphore)
import RIO.Fiber.Semaphore as Semaphore

newtype KeyedPool r e k a = KeyedPool
  { pools :: ERef.Ref (Map k (Pool r e a))
  , sem :: Semaphore
  , create :: k -> RIO r e a
  , destroy :: a -> RIO r e Unit
  , capacity :: k -> Int
  , ttl :: Maybe Milliseconds
  }

-- | Allocate a keyed pool. Each per-key pool uses `capacity` slots,
-- | `create key` to mint a resource, and `destroy` on shutdown. Idle
-- | resources never expire.
make
  :: forall r e k a
   . Int
  -> (k -> RIO r e a)
  -> (a -> RIO r e Unit)
  -> RIO r e (KeyedPool r e k a)
make cap create destroy = do
  pools <- liftEffect (ERef.new Map.empty)
  sem <- liftEffect (Semaphore.make 1)
  pure
    ( KeyedPool
        { pools
        , sem
        , create
        , destroy
        , capacity: \_ -> cap
        , ttl: Nothing
        }
    )

-- | Allocate a keyed pool with a per-key capacity function and an
-- | optional idle TTL applied to every per-key pool. When `ttl` is
-- | `Nothing`, idle resources never expire.
makeWithTTL
  :: forall r e k a
   . { capacity :: k -> Int
     , create :: k -> RIO r e a
     , destroy :: a -> RIO r e Unit
     , timeToLive :: Maybe Milliseconds
     }
  -> RIO r e (KeyedPool r e k a)
makeWithTTL opts = do
  pools <- liftEffect (ERef.new Map.empty)
  sem <- liftEffect (Semaphore.make 1)
  pure
    ( KeyedPool
        { pools
        , sem
        , create: opts.create
        , destroy: opts.destroy
        , capacity: opts.capacity
        , ttl: opts.timeToLive
        }
    )

-- | Borrow a resource for the given key. The first borrow for a new
-- | key lazily constructs that key's pool; subsequent borrows reuse
-- | it. Within a single key, semantics match `Pool.withResource`.
withResource
  :: forall r e k a b
   . Ord k
  => KeyedPool r e k a
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
  => KeyedPool r e k a
  -> k
  -> (a -> RIO r e Unit -> RIO r e b)
  -> RIO r e b
withResource' kp k use = do
  pool <- getOrCreate kp k
  Pool.withResource' pool use

-- | Run `Pool.shutdown` on every currently-known per-key pool. New
-- | borrows after `shutdown` rebuild the pool lazily.
shutdown :: forall r e k a. KeyedPool r e k a -> RIO r e Unit
shutdown (KeyedPool kp) = Semaphore.withPermit kp.sem do
  m <- liftEffect (ERef.read kp.pools)
  liftEffect (ERef.write Map.empty kp.pools)
  traverse_ Pool.shutdown (Map.values m)

-- | The set of keys for which a per-key pool currently exists. Useful
-- | for inspection in tests.
keys :: forall r e k a. KeyedPool r e k a -> RIO r e (Set k)
keys (KeyedPool kp) = do
  m <- liftEffect (ERef.read kp.pools)
  pure (Map.keys m)

getOrCreate
  :: forall r e k a
   . Ord k
  => KeyedPool r e k a
  -> k
  -> RIO r e (Pool r e a)
getOrCreate (KeyedPool kp) k = Semaphore.withPermit kp.sem do
  m <- liftEffect (ERef.read kp.pools)
  case Map.lookup k m of
    Just p -> pure p
    Nothing -> do
      p <- case kp.ttl of
        Just t -> Pool.makeWithTTL
          { capacity: kp.capacity k
          , create: kp.create k
          , destroy: kp.destroy
          , timeToLive: t
          }
        Nothing -> Pool.make (kp.capacity k) (kp.create k) kp.destroy
      liftEffect (ERef.write (Map.insert k p m) kp.pools)
      pure p
