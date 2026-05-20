-- | A small fan-out worker pool that pulls jobs off an `RIO.Aff.Queue`
-- | and runs them through a `Semaphore`-gated handler with a retry
-- | `Schedule`.
-- |
-- | This module exists to show how the framework's pieces compose:
-- |
-- |   * `RIO.Aff.Queue` carries the inbox; a producer offers jobs and
-- |     can `shutdown` when finished, which wakes blocked takers
-- |     with `Nothing`.
-- |   * `RIO.Aff.Semaphore` bounds in-flight work below `concurrency`
-- |     even when there are more workers than permits.
-- |   * `RIO.Aff.Schedule.retry` re-runs failing jobs with bounded
-- |     exponential backoff; each retry stays inside the worker's
-- |     permit, so a thrashing job doesn't release-then-reacquire
-- |     and starve siblings.
-- |   * `RIO.Aff.Concurrency.fork` and `join` give us "fan out N
-- |     workers, await all of them" without leaving fibers behind.
-- |   * `RIO.Aff.Metrics` records "jobs done" / "jobs failed" counters
-- |     and a wall-clock histogram per job.
-- |   * `RIO.Aff.Tracer.withSpan` brackets each job in a span so OTel
-- |     traces show one root per job with the retries nested.
module Example.WorkerPool.Workers
  ( Job
  , JobError
  , PoolConfig
  , runWorkers
  ) where

import Prelude hiding (join)

import Data.Array as Array
import Data.DateTime.Instant (unInstant)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse, traverse_)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (now) as Now
import Type.Proxy (Proxy(..))

import Example.WorkerPool.Logger (Logger, info, warn)
import RIO.Aff.Clock (Clock)
import RIO.Aff.Concurrency (fork, join)
import RIO.Aff.Core (RIO, catchAll)
import RIO.Aff.Metrics (Metrics, incrementCounter, observeHistogram)
import RIO.Aff.Queue (Queue)
import RIO.Aff.Queue as Queue
import RIO.Aff.Schedule (exponential, intersect, recurs, retry)
import RIO.Aff.Semaphore (Semaphore)
import RIO.Aff.Semaphore as Semaphore
import RIO.Aff.Tracer (Tracer, withSpan)

-- | A job is just a labelled `RIO` action. The label is what shows
-- | up in logs, metrics names, and span attributes.
type Job r =
  { name :: String
  , run :: RIO r JobError Unit
  }

-- | Typed failures for jobs. Anything we want to retry on goes in
-- | here; defects (`die`) bypass the retry loop and surface as a
-- | logged worker error.
type JobError =
  ( jobFailed :: String
  )

-- | Knobs for the pool. `workers` is the number of fibers we fan
-- | out; `concurrency` is the semaphore size that bounds how many
-- | of them can be doing real work at once. `maxRetries` is the
-- | total number of retries (so `1` means "two tries total").
type PoolConfig =
  { workers :: Int
  , concurrency :: Int
  , maxRetries :: Int
  }

-- | The base environment we read from. A caller can extend this
-- | row freely; the worker only needs to project these fields.
-- |
-- | Note we don't list `clock :: Clock` here even though we use the
-- | clock under the hood (via `Schedule.retry`): `retry` adds the
-- | row label itself, so listing it twice would force the compiler
-- | to unify duplicate labels and trips inference.
type WorkerEnv r =
  ( logger :: Logger
  , metrics :: Metrics
  , tracer :: Tracer
  | r
  )

-- | Fan out `workers` fibers, give each one a handle to the same
-- | inbox queue, and join every fiber before returning. Callers
-- | typically `fork` the producer side first, offer jobs onto the
-- | queue, then `Queue.shutdown` when there are no more.
-- |
-- | The Job's inner row is `WorkerEnv r` (no clock), but the
-- | runner needs `clock :: Clock | WorkerEnv r` because
-- | `Schedule.retry` adds the clock label.
runWorkers
  :: forall r
   . PoolConfig
  -> Queue (Job (WorkerEnv r))
  -> RIO (clock :: Clock | WorkerEnv r) () Unit
runWorkers cfg inbox = do
  sem <- liftEffect (Semaphore.make cfg.concurrency)
  fibers <- traverse (\i -> fork (worker cfg sem i inbox))
    (Array.range 1 cfg.workers)
  traverse_ join fibers
  info "[pool] all workers done"

-- | One worker. Loops on `Queue.take`; `Nothing` means the queue
-- | was shut down and we should exit.
worker
  :: forall r
   . PoolConfig
  -> Semaphore
  -> Int
  -> Queue (Job (WorkerEnv r))
  -> RIO (clock :: Clock | WorkerEnv r) () Unit
worker cfg sem n inbox = loop
  where
  tag = "[worker " <> show n <> "]"

  loop = do
    mJob <- Queue.take inbox
    case mJob of
      Nothing -> info (tag <> " inbox closed, exiting")
      Just job -> do
        runOne job
        loop

  runOne :: Job (WorkerEnv r) -> RIO (clock :: Clock | WorkerEnv r) () Unit
  runOne job = Semaphore.withPermit sem do
    info (tag <> " starting " <> job.name)
    startMs <- liftEffect epochMs
    -- The retry stays inside the permit. A flaky job holds onto
    -- its slot while it backs off rather than releasing it and
    -- re-acquiring it, which keeps total in-flight work bounded.
    let
      attempts :: RIO (clock :: Clock | WorkerEnv r) JobError Unit
      attempts = retry retrySched (withSpan job.name job.run)

    -- `catchAll` reduces the row, so the post-failure branch runs
    -- with `()`. Success records its own metrics in the body.
    catchAll
      ( \v -> do
          elapsed <- liftEffect (elapsedMsFrom startMs)
          warn
            ( tag <> " " <> job.name <> " failed after "
                <> show elapsed
                <> "ms (gave up after retries): "
                <> describe v
            )
          incrementCounter "worker_pool.jobs_failed"
          observeHistogram "worker_pool.job_duration_ms" elapsed
      )
      ( do
          attempts
          elapsed <- liftEffect (elapsedMsFrom startMs)
          info
            ( tag <> " " <> job.name <> " ok in "
                <> show elapsed
                <> "ms"
            )
          incrementCounter "worker_pool.jobs_done"
          observeHistogram "worker_pool.job_duration_ms" elapsed
      )

  retrySched = intersect (recurs cfg.maxRetries)
    (exponential (Milliseconds 50.0) 2.0)

-- | Render whatever payload the job carries on its failure tag.
-- | `JobError` only has a single tag today, but this is shaped so
-- | adding more tags is just one more `Variant.on` away.
describe :: Variant JobError -> String
describe = Variant.on (Proxy :: Proxy "jobFailed") identity
  (\_ -> "<unknown failure>")

-- | Wall-clock millis since the Unix epoch. We bypass the `Clock`
-- | service here because these are log-line and histogram values,
-- | not anything the test harness needs to virtualize.
epochMs :: Effect Number
epochMs = do
  i <- Now.now
  pure (unwrap (unInstant i))

elapsedMsFrom :: Number -> Effect Number
elapsedMsFrom start = do
  current <- epochMs
  pure (current - start)
