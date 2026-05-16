-- | A worker-pool over `Queue` + `Pool`-style scope-bounded
-- | fork semantics.
-- |
-- | A `WorkerPool e a b` runs `n` long-lived worker fibers in
-- | parallel, each pulling jobs of type `a` off a shared queue
-- | and producing results of type `b` (or a typed failure on
-- | row `e`). Callers `submit` work and either get back a
-- | `Deferred e b` to await later, or use `submitAndAwait` to
-- | block on completion.
-- |
-- | Compared to `parTraverseN`, which forks a fresh fiber per
-- | element and finishes when all of them do, `WorkerPool`
-- | reuses a fixed worker fleet across many submissions: useful
-- | for long-running services that take work off a stream or
-- | external queue and want bounded concurrency without the
-- | per-call setup cost of a fresh fork.
-- |
-- | ## Lifecycle: tied to a `Scope`
-- |
-- | Workers are forked inside the supplied scope. Scope exit
-- | (success, typed failure, defect, fiber kill) shuts down
-- | the queue and kills the workers. In-flight jobs leave
-- | their `Deferred` unfilled; awaiters will block until they
-- | are themselves interrupted by their own scope.
-- |
-- | ## Typed failures
-- |
-- | If the handler raises a typed failure on row `e`, the
-- | worker catches it and surfaces it through the job's
-- | `Deferred` (via `failDeferred`); the worker itself stays
-- | alive and processes the next job. Defects (untyped
-- | exceptions from `Aff`) propagate normally and kill the
-- | worker fiber; the corresponding `Deferred` never resolves.
-- |
-- | ```purescript
-- | program = scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   pool <- WorkerPool.make scope
-- |     { workers: 4, queueCapacity: Just 100 }
-- |     processItem
-- |   for_ items \item -> do
-- |     d <- WorkerPool.submit pool item
-- |     _ <- fork (handleResult d)
-- |     pure unit
-- | ```
module RIO.WorkerPool
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
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import RIO.Concurrency (forkScoped) as Conc
import RIO.Deferred
  ( Deferred
  , awaitDeferred
  , failDeferred
  , makeDeferred
  , succeedDeferred
  )
import RIO.Error (catchAll) as Error
import RIO.Internal (RIO(..))
import RIO.Queue (Queue)
import RIO.Queue as Queue
import RIO.Resource (Scope, addFinalizer)

-- | A scope-bounded worker pool.
-- |
-- | Construct with `make`; submit work with `submit` /
-- | `submitAndAwait`. The pool is implicitly shut down at
-- | scope exit; an explicit `shutdown` is also available.
newtype WorkerPool :: Row Type -> Type -> Type -> Type
newtype WorkerPool e a b = WorkerPool
  { queue :: Queue (Job e a b)
  }

-- | A job carried through the internal queue: the input value
-- | and the cell the worker fills with the result.
type Job :: Row Type -> Type -> Type -> Type
type Job e a b =
  { input :: a
  , result :: Deferred e b
  }

-- | Configuration for `make`.
-- |
-- |   * `workers` is the number of long-lived worker fibers.
-- |     Clamped to at least `1`.
-- |   * `queueCapacity` bounds the in-flight queue (use
-- |     `Nothing` for an unbounded queue). A bounded queue
-- |     gives backpressure for free: `submit` blocks while
-- |     the queue is at capacity.
type WorkerPoolConfig =
  { workers :: Int
  , queueCapacity :: Maybe Int
  }

-- | Construct a worker pool tied to `scope`. The pool spins
-- | up `workers` worker fibers; each repeatedly takes a job
-- | off the queue, runs `handler` on the input, and stores
-- | the result in the job's `Deferred`. When `scope` exits,
-- | the queue is shut down and the worker fibers are killed.
make
  :: forall r e' e a b
   . Scope
  -> WorkerPoolConfig
  -> (a -> RIO r e b)
  -> RIO r e' (WorkerPool e a b)
make scope config handler = do
  queue <- liftEffect case config.queueCapacity of
    Just n -> Queue.bounded n
    Nothing -> Queue.unbounded
  let
    workerCount = max 1 config.workers

    loop :: RIO r e' Unit
    loop = do
      job <- Queue.take queue
      case job of
        Nothing -> pure unit
        Just j -> do
          _ <- Error.catchAll
            ( \v -> do
                _ <- failDeferred j.result v
                pure unit
            )
            ( do
                b <- handler j.input
                _ <- succeedDeferred j.result b
                pure unit
            )
          loop

  for_ (range 1 workerCount) \_ -> do
    _ <- Conc.forkScoped scope loop
    pure unit

  addFinalizer scope (shutdownAff queue)
  pure (WorkerPool { queue })

-- | Internal: shut down the queue from a finalizer.
shutdownAff
  :: forall e a b
   . Queue (Job e a b)
  -> Aff Unit
shutdownAff queue = case Queue.shutdown queue of
  RIO action -> do
    _ <- action {}
    pure unit

-- | Enqueue a job and return the cell that the worker will
-- | fill. The caller can `awaitDeferred` it directly, hand it
-- | off to another fiber, or store it for later.
-- |
-- | Blocks (via the queue) when the queue is bounded and
-- | full.
submit
  :: forall r e' e a b
   . WorkerPool e a b
  -> a
  -> RIO r e' (Deferred e b)
submit (WorkerPool { queue }) input = do
  result <- makeDeferred
  _ <- Queue.offer queue { input, result }
  pure result

-- | Convenience: `submit` immediately followed by
-- | `awaitDeferred`. The caller's error row picks up `e`.
submitAndAwait
  :: forall r e a b
   . WorkerPool e a b
  -> a
  -> RIO r e b
submitAndAwait pool input = do
  d <- submit pool input
  awaitDeferred d

-- | Explicit shutdown. Equivalent to scope exit for the
-- | queue half: blocked workers wake on the next `take`,
-- | further `submit`s land in a shut-down queue and the
-- | corresponding `Deferred`s never resolve.
shutdown :: forall r e' e a b. WorkerPool e a b -> RIO r e' Unit
shutdown (WorkerPool { queue }) = Queue.shutdown queue
