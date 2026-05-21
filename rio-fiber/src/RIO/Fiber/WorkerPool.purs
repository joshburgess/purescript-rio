-- | A worker-pool over `Queue` + scope-bounded fork semantics.
-- |
-- | A `WorkerPool e a b` runs `n` long-lived worker fibers in
-- | parallel, each pulling jobs of type `a` off a shared queue and
-- | producing results of type `b` (or a typed failure on row `e`).
-- | Callers `submit` work and either get back a `Deferred e b` to
-- | await later, or use `submitAndAwait` to block on completion.
-- |
-- | Compared to `parTraverse`, which forks a fresh fiber per
-- | element and finishes when all of them do, `WorkerPool` reuses a
-- | fixed worker fleet across many submissions: useful for long-
-- | running services that take work off a stream or external queue
-- | and want bounded concurrency without the per-call setup cost of
-- | a fresh fork.
-- |
-- | ## Lifecycle: tied to a `Scope`
-- |
-- | Workers are forked inside the supplied scope. Scope exit
-- | (success, typed failure, defect, fiber kill) shuts down the
-- | pool and kills the workers. An explicit `shutdown` is also
-- | available; it signals workers to exit the next time they would
-- | otherwise pull a job. In-flight jobs at shutdown leave their
-- | `Deferred` unfilled; awaiters will block until they are
-- | themselves interrupted by their own scope.
-- |
-- | ## Typed failures
-- |
-- | If the handler raises a typed failure on row `e`, the worker
-- | catches it and surfaces it through the job's `Deferred` (via
-- | `Deferred.fail`); the worker itself stays alive and processes
-- | the next job. Defects and interrupts propagate normally and
-- | kill the worker fiber; the corresponding `Deferred` never
-- | resolves.
-- |
-- | ```purescript
-- | program = scoped \scope -> do
-- |   pool <- WorkerPool.make scope
-- |     { workers: 4, queueCapacity: Just 100 }
-- |     processItem
-- |   for_ items \item -> do
-- |     d <- WorkerPool.submit pool item
-- |     _ <- fork (handleResult d)
-- |     pure unit
-- | ```
module RIO.Fiber.WorkerPool
  ( WorkerPool
  , WorkerPoolConfig
  , make
  , submit
  , submitAndAwait
  , shutdown
  ) where

import Prelude

import Data.Array (range)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

import RIO.Fiber.Core (RIO, catchAll, fork, liftEffect, race)
import RIO.Fiber.Deferred (Deferred)
import RIO.Fiber.Deferred as Deferred
import RIO.Fiber.Internal (interruptFiber)
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Queue
import RIO.Fiber.Scope (Scope, addFinalizer)

-- | A scope-bounded worker pool.
-- |
-- | Construct with `make`; submit work with `submit` /
-- | `submitAndAwait`. The pool is implicitly shut down at scope
-- | exit; an explicit `shutdown` is also available.
newtype WorkerPool :: Row Type -> Type -> Type -> Type
newtype WorkerPool e a b = WorkerPool
  { queue :: Queue (Job e a b)
  , shutdownSignal :: Deferred () Unit
  }

type Job :: Row Type -> Type -> Type -> Type
type Job e a b =
  { input :: a
  , result :: Deferred e b
  }

-- | Configuration for `make`.
-- |
-- |   * `workers` is the number of long-lived worker fibers.
-- |     Clamped to at least `1`.
-- |   * `queueCapacity` bounds the in-flight queue. `Nothing` is
-- |     treated as a very large capacity (effectively unbounded for
-- |     practical workloads); a `Just n` bounds the queue so
-- |     `submit` backpressures while the queue is at capacity.
type WorkerPoolConfig =
  { workers :: Int
  , queueCapacity :: Maybe Int
  }

-- The rio-fiber Queue is bounded-only and has no shutdown signal.
-- We pick a large default for the "unbounded" case rather than
-- inventing a new queue variant.
defaultCapacity :: Int
defaultCapacity = 1_000_000

-- | Construct a worker pool tied to `scope`. The pool spins up
-- | `workers` worker fibers; each repeatedly takes a job off the
-- | queue, runs `handler` on the input, and stores the result in
-- | the job's `Deferred`. When `scope` exits, the workers receive a
-- | shutdown signal and are then interrupted.
make
  :: forall r e' e a b
   . Scope
  -> WorkerPoolConfig
  -> (a -> RIO r e b)
  -> RIO r e' (WorkerPool e a b)
make scope config handler = do
  let
    cap = case config.queueCapacity of
      Just n -> max 1 n
      Nothing -> defaultCapacity
    workerCount = max 1 config.workers
  queue <- liftEffect (Queue.make cap)
  shutdownSignal <- liftEffect Deferred.make
  let
    loop :: RIO r e' Unit
    loop = do
      result <- race
        (Just <$> Queue.take queue)
        (Nothing <$ Deferred.awaitPure shutdownSignal)
      case result of
        Nothing -> pure unit
        Just j -> do
          _ <- catchAll
            ( \v -> do
                _ <- Deferred.fail j.result v
                pure unit
            )
            ( do
                b <- handler j.input
                _ <- Deferred.succeed j.result b
                pure unit
            )
          loop

  workers <- traverse (\_ -> fork loop) (range 1 workerCount)
  -- Scope finalizer: signal shutdown so workers exit cleanly the
  -- next time they would block on `take`, then interrupt any worker
  -- that is mid-handler.
  addFinalizer scope do
    _ <- Deferred._succeed shutdownSignal unit
    for_ workers interruptFiber
  pure (WorkerPool { queue, shutdownSignal })

-- | Enqueue a job and return the cell that the worker will fill.
-- | The caller can `await` it directly, hand it off to another
-- | fiber, or store it for later.
-- |
-- | Blocks (via the queue) when the queue is at capacity.
submit
  :: forall r e' e a b
   . WorkerPool e a b
  -> a
  -> RIO r e' (Deferred e b)
submit (WorkerPool { queue }) input = do
  result <- liftEffect Deferred.make
  Queue.offer queue { input, result }
  pure result

-- | Convenience: `submit` immediately followed by `await`. The
-- | caller's error row picks up `e`.
submitAndAwait
  :: forall r e a b
   . WorkerPool e a b
  -> a
  -> RIO r e b
submitAndAwait pool input = do
  d <- submit pool input
  Deferred.await d

-- | Explicit shutdown. Signals all workers to exit the next time
-- | they would otherwise block on `take`. Workers currently inside
-- | a handler will not be interrupted by this call alone; closing
-- | the owning scope kills the worker fibers outright.
shutdown :: forall r e' e a b. WorkerPool e a b -> RIO r e' Unit
shutdown (WorkerPool { shutdownSignal }) = do
  _ <- Deferred.succeed shutdownSignal unit
  pure unit
