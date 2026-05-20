-- | Canonical production wire-up.
-- |
-- | This example pulls together every piece a long-running RIO
-- | daemon would touch on Node:
-- |
-- |   * `Layer` to wire the service graph (`Logger`, `Tracer`,
-- |     `Metrics`, plus an app-specific `tickCounter`).
-- |   * `Runtime` to bundle the resolved environment record and
-- |     run many programs against it.
-- |   * A long-running worker loop (`workerLoop`) that emits a
-- |     heartbeat every second through `Logger`, opens a
-- |     `Tracer.withSpan` per tick, and increments a counter.
-- |   * `RIO.Aff.Node.Shutdown.withShutdown` to race the worker
-- |     against `SIGINT` / `SIGTERM`. On a signal the worker
-- |     stops, the layer's finalizers fire (LIFO), and the
-- |     process exits cleanly.
-- |
-- | Run with:
-- |
-- | ```
-- | npx spago run -p rio-example-production-app
-- | ```
-- |
-- | Hit Ctrl-C to send `SIGINT`. The process prints the final
-- | tick count and exits without orphaning resources.
module Example.ProductionApp.Main
  ( main
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO)
import RIO.Aff.Env (ask)
import RIO.Aff.Layer (Layer, andThen, buildLayer, fromRIO)
import RIO.Aff.Logger (Logger, consoleLogger, logInfo, withFields)
import RIO.Aff.Metrics (Metrics, incrementCounter, noopMetrics)
import RIO.Aff.Runtime (Runtime)
import RIO.Aff.Runtime as Runtime
import RIO.Aff.Tracer (Tracer, addAttribute, noopTracer, withSpan)

import RIO.Aff.Node.Shutdown (defaultShutdownSignals, withShutdown)

-- | The full service graph the app runs against. `tickCounter`
-- | is an app-specific service (a `Ref`-backed integer); the
-- | rest are stock RIO infrastructure.
type AppEnv =
  ( logger :: Logger
  , tracer :: Tracer
  , metrics :: Metrics
  , tickCounter :: Ref Int
  )

-- | The infrastructure layer: Logger, Tracer, Metrics. Each is
-- | a stateless service in this example; in production the
-- | Tracer would be wired to an exporter and Metrics to a real
-- | backend, both of which would register finalizers via
-- | `addFinalizer`.
infraLayer :: forall e. Layer () e (logger :: Logger, tracer :: Tracer, metrics :: Metrics)
infraLayer = fromRIO do
  logger <- liftAff (liftEffect consoleLogger)
  pure
    { logger
    , tracer: noopTracer
    , metrics: noopMetrics
    }

-- | The app layer: builds on top of the infrastructure layer and
-- | adds the per-process `tickCounter`. Composed with `>>>`
-- | (`andThen`) so the counter sits in the same scope as the
-- | infra services.
appLayer :: forall e. Layer () e AppEnv
appLayer = andThen infraLayer counterLayer

counterLayer
  :: forall e
   . Layer
       (logger :: Logger, tracer :: Tracer, metrics :: Metrics)
       e
       AppEnv
counterLayer = fromRIO do
  logger <- ask (Proxy :: Proxy "logger")
  tracer <- ask (Proxy :: Proxy "tracer")
  metrics <- ask (Proxy :: Proxy "metrics")
  ref <- liftAff (liftEffect (Ref.new 0))
  pure
    { logger
    , tracer
    , metrics
    , tickCounter: ref
    }

-- | One unit of work. Each tick opens a tracer span, structures
-- | the log line with the tick number, increments the metric
-- | counter, and bumps the in-process `tickCounter`.
tick :: RIO AppEnv () Unit
tick = do
  ref <- ask (Proxy :: Proxy "tickCounter")
  n <- liftAff (liftEffect (Ref.modify (_ + 1) ref))
  withFields [ Tuple "tick" (show n) ]
    ( withSpan "production-app.tick" do
        addAttribute "tick.number" (show n)
        incrementCounter "production_app.ticks"
        logInfo "tick"
    )

-- | The worker loop: tick, sleep one second, repeat forever.
-- | The "forever" is closed off by `withShutdown` racing this
-- | loop against an OS signal.
workerLoop :: RIO AppEnv () Unit
workerLoop = do
  tick
  liftAff (delay (Milliseconds 1000.0))
  workerLoop

-- | Wire everything together: build the layer, snapshot the env
-- | into a `Runtime`, run the worker under `withShutdown`, and
-- | print the final tick count once the signal fires.
main :: Effect Unit
main = launchAff_ do
  built <- buildLayer (appLayer :: Layer () () AppEnv)
  case built of
    Left _ ->
      liftEffect (log "production-app: failed to build infrastructure layer")
    Right envRec -> do
      let
        runtime :: Runtime AppEnv
        runtime = Runtime.make envRec
      liftEffect do
        log "production-app: started; emitting heartbeats. Ctrl-C to stop."
      outcome <- withShutdown defaultShutdownSignals
        (Runtime.runOrThrow runtime workerLoop)
      finalCount <- liftEffect (Ref.read envRec.tickCounter)
      liftEffect do
        case outcome of
          Just _ -> log "production-app: worker returned (unexpected)"
          Nothing -> log "production-app: shutdown signal received"
        log ("production-app: " <> show finalCount <> " ticks emitted")
