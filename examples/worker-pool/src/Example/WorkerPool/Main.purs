-- | End-to-end worker-pool demo.
-- |
-- | The shape of this program is the convincing part: producer +
-- | bounded queue + fan-out of N workers + per-job retry schedule
-- | + spans + metrics, all sitting on top of pure record-and-row
-- | services that the test harness or a different binary could
-- | swap out wholesale.
-- |
-- | Run it with:
-- |
-- | ```
-- | npx spago run -p rio-example-worker-pool
-- | ```
module Example.WorkerPool.Main where

import Prelude hiding (join)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import Example.WorkerPool.Logger (Logger, consoleLogger, info)
import Example.WorkerPool.Workers (Job, runWorkers)
import RIO.Clock (Clock, liveClock)
import RIO.Concurrency (fork, join)
import RIO.Core (RIO, fail, provideAll, runRIO)
import RIO.Metrics (Metrics)
import RIO.Queue (Queue)
import RIO.Queue as Queue
import RIO.Tracer (Tracer, noopTracer)
import RIO.Test.Metrics (MetricRecord, newRecordingMetrics) as TestMetrics

-- | The full environment the top-level program runs in. The Job's
-- | inner row is the smaller `JobEnv`: `Schedule.retry` adds the
-- | clock label, so jobs themselves don't carry it in their row.
type Env =
  ( clock :: Clock
  , logger :: Logger
  , metrics :: Metrics
  , tracer :: Tracer
  )

type JobEnv =
  ( logger :: Logger
  , metrics :: Metrics
  , tracer :: Tracer
  )

main :: Effect Unit
main = launchAff_ do
  recording <- TestMetrics.newRecordingMetrics
  let
    env =
      { clock: liveClock
      , logger: consoleLogger
      , metrics: recording.metrics
      , tracer: noopTracer
      }
  result <- runRIO (provideAll env program)
  liftEffect case result of
    Right _ -> Console.log "worker-pool: ok"
    Left _ -> Console.log "worker-pool: failed"
  records <- liftEffect recording.snapshot
  liftEffect (printRecords records)

program :: RIO Env () Unit
program = do
  info "[main] booting worker pool"
  inbox <- liftEffect (Queue.bounded 4)
  -- Producer: feed 8 jobs onto the queue, then shut it down so the
  -- workers see `Nothing` and exit.
  producer <- fork (producerLoop inbox)
  -- Workers: 3 fibers, only 2 may run in parallel, each retries up
  -- to twice on typed failure.
  runWorkers
    { workers: 3, concurrency: 2, maxRetries: 2 }
    inbox
  join producer
  info "[main] shutdown clean"

producerLoop :: Queue (Job JobEnv) -> RIO Env () Unit
producerLoop inbox = do
  -- We mix three kinds of jobs:
  --   * fast: succeeds first try
  --   * slow: succeeds first try after a small delay
  --   * flaky: typed-fails twice, succeeds on the third try (so
  --     the retry schedule's three-attempt budget catches it)
  let
    fastJobs = map (makeJob "fast") (Array.range 1 3)
    slowJobs =
      map (\i -> makeSlowJob ("slow-" <> show i)) (Array.range 1 2)
  flakyState <- liftEffect (Ref.new 0)
  let
    flakyJobs =
      [ makeFlakyJob "flaky-A" flakyState
      , makeFlakyJob "flaky-B" flakyState
      ]
  let
    jobs = fastJobs <> slowJobs <> flakyJobs
  traverse_ (\j -> Queue.offer inbox j) jobs
  -- Once everything is on the queue, signal end-of-input.
  Queue.shutdown inbox

makeJob :: String -> Int -> Job JobEnv
makeJob label i =
  { name: label <> "-" <> show i
  , run: pure unit
  }

makeSlowJob :: String -> Job JobEnv
makeSlowJob name =
  { name
  , run: liftAff (delay (Milliseconds 30.0))
  }

-- | A job that fails the first two attempts and succeeds on the
-- | third. We thread the attempt counter through a shared `Ref`,
-- | keyed by name, so two flaky jobs don't interfere.
makeFlakyJob :: String -> Ref.Ref Int -> Job JobEnv
makeFlakyJob name attemptsRef =
  { name
  , run: do
      n <- liftEffect (Ref.modify (_ + 1) attemptsRef)
      if n `mod` 3 == 0 then pure unit
      else fail (Proxy :: Proxy "jobFailed") (name <> " attempt " <> show n)
  }

-- | One-line summary per recorded metric. Stays a plain `Effect`
-- | so it works after `runRIO` returns.
printRecords :: Array TestMetrics.MetricRecord -> Effect Unit
printRecords =
  traverse_ \r ->
    Console.log
      ( "  " <> show r.kind <> " " <> r.name <> " = " <> show r.value
      )
