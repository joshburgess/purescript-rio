-- | A bounded resource pool.
-- |
-- | `make capacity create destroy` returns a pool that lends out
-- | up to `capacity` resources at once. Resources are allocated
-- | lazily via `create` on first borrow, and returned to the pool
-- | when the borrower finishes. `destroy` runs at shutdown for
-- | every idle resource.
-- |
-- | The MVP keeps the model simple: there is no health-check on
-- | return, no eviction by idle time, and `shutdown` does not wait
-- | for outstanding borrowers. Higher-level invariants (e.g.
-- | "discard the resource on failure") belong in the caller's
-- | acquire / use code for now.
module RIO.Fiber.Pool
  ( Pool
  , make
  , withResource
  , shutdown
  , size
  , available
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import RIO.Fiber.Core (RIO, bracket, catchAll, causeOf, failCause, liftEffect)
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q
import RIO.Fiber.Semaphore (Semaphore)
import RIO.Fiber.Semaphore as Sem

newtype Pool r e a = Pool
  { capacity :: Int
  , permits :: Semaphore
  , free :: Queue a
  , create :: RIO r e a
  , destroy :: a -> RIO r e Unit
  }

-- | Allocate a pool with the given positive capacity. `create` is
-- | called on demand when a borrower asks for a resource and no
-- | idle one is available; `destroy` is invoked on every idle
-- | resource at `shutdown` time.
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
  pure (Pool { capacity: c, permits, free, create, destroy })

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
withResource (Pool p) use = bracket
  ( do
      Sem.acquireN 1 p.permits
      mIdle <- Q.tryTake p.free
      case mIdle of
        Just r -> pure r
        Nothing -> rescue (Sem.releaseN 1 p.permits) p.create
  )
  ( \r -> do
      Q.offer p.free r
      Sem.releaseN 1 p.permits
  )
  use

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
      Just r -> do
        _ <- catchAll (\_ -> pure unit) (p.destroy r)
        drainLoop

-- | Configured capacity.
size :: forall r e a. Pool r e a -> RIO r e Int
size (Pool p) = pure p.capacity

-- | Number of resources currently idle in the pool.
available :: forall r e a. Pool r e a -> RIO r e Int
available (Pool p) = Q.size p.free
