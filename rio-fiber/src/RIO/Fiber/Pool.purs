-- | A bounded resource pool.
-- |
-- | `make capacity create destroy` returns a pool that lends out
-- | up to `capacity` resources at once. Resources are allocated
-- | lazily via `create` on first borrow, and returned to the pool
-- | when the borrower finishes. `destroy` runs at shutdown for
-- | every idle resource.
-- |
-- | `makeWithTTL` extends the model with a per-resource idle TTL.
-- | An idle resource whose age exceeds the TTL is destroyed (rather
-- | than handed out) on the next borrow attempt, so a quiet pool
-- | self-recycles instead of holding stale handles indefinitely.
-- |
-- | `withResource'` is the same as `withResource` but exposes a
-- | per-borrow `invalidate` action; calling it inside the body marks
-- | the resource as bad so the pool runs `destroy` on it when the
-- | body exits instead of returning it to the free queue. Higher-
-- | level invariants (e.g. "discard the resource on a typed
-- | failure") can be implemented in terms of `withResource'`.
-- |
-- | The MVP keeps the rest of the model simple: there is no health
-- | check on return, and `shutdown` does not wait for outstanding
-- | borrowers.
module RIO.Fiber.Pool
  ( Pool
  , make
  , makeWithTTL
  , withResource
  , withResource'
  , shutdown
  , size
  , available
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Ref as Ref
import RIO.Fiber.Clock (currentEpoch)
import RIO.Fiber.Core (RIO, bracket, catchAll, causeOf, failCause, liftEffect)
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q
import RIO.Fiber.Semaphore (Semaphore)
import RIO.Fiber.Semaphore as Sem

type Entry a = { item :: a, addedAt :: Milliseconds }

newtype Pool r e a = Pool
  { capacity :: Int
  , permits :: Semaphore
  , free :: Queue (Entry a)
  , create :: RIO r e a
  , destroy :: a -> RIO r e Unit
  , ttl :: Maybe Milliseconds
  }

-- | Allocate a pool with the given positive capacity. `create` is
-- | called on demand when a borrower asks for a resource and no
-- | idle one is available; `destroy` is invoked on every idle
-- | resource at `shutdown` time. Idle resources never expire.
make
  :: forall r e a
   . Int
  -> RIO r e a
  -> (a -> RIO r e Unit)
  -> RIO r e (Pool r e a)
make cap create destroy = do
  let c = max 1 cap
  permits <- liftEffect (Sem.make c)
  free <- liftEffect (Q.make c)
  pure (Pool { capacity: c, permits, free, create, destroy, ttl: Nothing })

-- | Allocate a pool whose idle resources expire after `timeToLive`.
-- | When `withResource` next pulls an idle resource off the free
-- | queue, any entry older than `timeToLive` is destroyed and the
-- | next-newer one is checked; if the queue empties, `create` runs
-- | to mint a fresh resource. The wall-clock comparison is read from
-- | the active `Clock`, so a virtual clock makes the timing
-- | deterministic in tests.
makeWithTTL
  :: forall r e a
   . { capacity :: Int
     , create :: RIO r e a
     , destroy :: a -> RIO r e Unit
     , timeToLive :: Milliseconds
     }
  -> RIO r e (Pool r e a)
makeWithTTL opts = do
  let c = max 1 opts.capacity
  permits <- liftEffect (Sem.make c)
  free <- liftEffect (Q.make c)
  pure
    ( Pool
        { capacity: c
        , permits
        , free
        , create: opts.create
        , destroy: opts.destroy
        , ttl: Just opts.timeToLive
        }
    )

-- | Borrow a resource for the duration of `use`. Acquires a permit
-- | (blocking if the pool is at capacity), then either reuses an
-- | idle resource or runs `create` to mint a new one. The resource
-- | returns to the pool whether `use` succeeds, fails, defects, or
-- | is interrupted.
withResource
  :: forall r e a b
   . Pool r e a
  -> (a -> RIO r e b)
  -> RIO r e b
withResource pool use = withResource' pool (\r _ -> use r)

-- | Borrow a resource and pass the body a second action that marks
-- | the resource as bad. If `invalidate` is called inside the body
-- | the pool runs `destroy` on the resource when the body exits and
-- | the slot opens up for a fresh `create` next time. If
-- | `invalidate` is not called the resource returns to the free
-- | queue as usual.
-- |
-- | Useful for "discard the connection if the call failed" patterns:
-- |
-- |   withResource' pool \conn invalidate -> do
-- |     r <- query conn `onError` \_ -> invalidate
-- |     pure r
withResource'
  :: forall r e a b
   . Pool r e a
  -> (a -> RIO r e Unit -> RIO r e b)
  -> RIO r e b
withResource' (Pool p) use = bracket
  ( do
      Sem.acquireN 1 p.permits
      resource <- rescue (Sem.releaseN 1 p.permits) (borrowOne (Pool p))
      flag <- liftEffect (Ref.new false)
      pure { resource, flag }
  )
  ( \{ resource, flag } -> do
      invalidated <- liftEffect (Ref.read flag)
      if invalidated then do
        _ <- catchAll (\_ -> pure unit) (p.destroy resource)
        Sem.releaseN 1 p.permits
      else do
        now <- currentEpoch
        Q.offer p.free { item: resource, addedAt: now }
        Sem.releaseN 1 p.permits
  )
  ( \{ resource, flag } ->
      use resource (liftEffect (Ref.write true flag))
  )

-- | Borrow one resource. If the pool has a TTL configured, evict
-- | expired entries one at a time until either a fresh enough entry
-- | is found or the queue is empty; allocate via `create` if the
-- | queue runs dry.
borrowOne :: forall r e a. Pool r e a -> RIO r e a
borrowOne (Pool p) = go
  where
  go = do
    mEntry <- Q.tryTake p.free
    case mEntry of
      Nothing -> p.create
      Just { item, addedAt: Milliseconds added } -> case p.ttl of
        Nothing -> pure item
        Just (Milliseconds ttl) -> do
          Milliseconds now <- currentEpoch
          if (now - added) <= ttl then pure item
          else do
            _ <- catchAll (\_ -> pure unit) (p.destroy item)
            go

-- | Run an `Effect`-style cleanup on a failure within an `RIO`.
-- | Used to roll back the permit if `create` itself fails.
rescue
  :: forall r e a
   . RIO r e Unit
  -> RIO r e a
  -> RIO r e a
rescue cleanup action = do
  result <- causeOf action
  case result of
    Right a -> pure a
    Left cause -> do
      cleanup
      failCause cause

-- | Destroy every currently-idle resource and reset the free
-- | queue. Borrowers in flight keep their resource until they
-- | return it; that return then sits in a still-bounded queue.
-- | Calling `shutdown` more than once is safe; subsequent calls
-- | are no-ops once the free queue is empty.
shutdown :: forall r e a. Pool r e a -> RIO r e Unit
shutdown (Pool p) = drainLoop
  where
  drainLoop = do
    mNext <- Q.tryTake p.free
    case mNext of
      Nothing -> pure unit
      Just { item } -> do
        _ <- catchAll (\_ -> pure unit) (p.destroy item)
        drainLoop

-- | Configured capacity.
size :: forall r e a. Pool r e a -> RIO r e Int
size (Pool p) = pure p.capacity

-- | Number of resources currently idle in the pool.
available :: forall r e a. Pool r e a -> RIO r e Int
available (Pool p) = Q.size p.free
