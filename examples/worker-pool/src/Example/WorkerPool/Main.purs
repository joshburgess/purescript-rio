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
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import Example.WorkerPool.Logger (Logger, consoleLogger, info, warn)
import Example.WorkerPool.Workers (Job, JobError, runWorkers)
import RIO.Aff.Cause (parTraverseCause, prettyCause)
import RIO.Aff.Clock (Clock, liveClock)
import RIO.Aff.Concurrency (fork, join)
import RIO.Aff.Core (RIO, fail, provideAll, runRIO)
import RIO.Aff.Metrics (Metrics)
import RIO.Aff.Queue (Queue)
import RIO.Aff.Queue as Queue
import RIO.Aff.Tracer (Tracer, noopTracer)
import RIO.Aff.Test.Metrics (MetricRecord, newRecordingMetrics) as TestMetrics

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
  -- Before the worker pool runs, validate each job name. The
  -- validations run in parallel under `parTraverseCause`, so every
  -- bad name surfaces (rather than just the first one) and the
  -- resulting `Cause` tree is rendered with `prettyCause`. Two
  -- names are intentionally bad to make the parallel-failures
  -- header visible.
  causeDemo
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

-- | A small "show what `prettyCause` looks like" pre-flight: a
-- | handful of validation steps run under `parTraverseCause`, two
-- | of which fail. The combined Parallel cause is rendered and
-- | logged so the example produces visible Cause output on every
-- | run.
causeDemo :: RIO Env () Unit
causeDemo = do
  let
    validate :: String -> RIO Env JobError String
    validate name
      | name == "" = fail (Proxy :: Proxy "jobFailed") "empty name"
      | name == "bad" =
          fail (Proxy :: Proxy "jobFailed") "name 'bad' is reserved"
      | otherwise = pure name
  outcome <- parTraverseCause validate [ "alpha", "", "beta", "bad" ]
  case outcome of
    Right names ->
      info ("[demo] all validations passed: " <> show names)
    Left cause ->
      warn
        ( "[demo] pre-flight validation failed:\n"
            <> prettyCause renderJobError cause
        )

-- | Render whatever payload a `JobError` carries on its tag. Mirrors
-- | the renderer used inside the worker, kept local here so this
-- | file is self-contained.
renderJobError :: Variant JobError -> String
renderJobError = Variant.on (Proxy :: Proxy "jobFailed")
  (\s -> "jobFailed: " <> s)
  (\_ -> "<unknown failure>")

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
