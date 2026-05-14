-- | Streaming pipeline demo.
-- |
-- | Three independent "partition" sources emit events at random
-- | cadences. `mergeAll` fans them into a single merged stream.
-- | `broadcast` then fans that out to two downstream consumers:
-- | one logs each event, the other counts events per source via
-- | `Metrics`. Both consumers run on forked fibers and the main
-- | program joins them before reporting.
-- |
-- | Run with:
-- |
-- | ```
-- | npx spago run -p rio-example-stream-pipeline
-- | ```
module Example.StreamPipeline.Main where

import Prelude hiding (join)

import Data.Array as Array
import Data.Foldable (foldl, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

import RIO.Concurrency (fork, join)
import RIO.Core (RIO, provideAll, runRIO)
import RIO.Metrics (Metrics, incrementCounter)
import RIO.Stream (Stream, mapM, runDrain, unfoldM)
import RIO.Stream.Par (broadcast, mergeAll)
import RIO.Test.Metrics (MetricRecord, newRecordingMetrics) as TestMetrics

type Event = { source :: String, payload :: Int }

type Env = (metrics :: Metrics)

main :: Effect Unit
main = launchAff_ do
  recording <- TestMetrics.newRecordingMetrics
  let env = { metrics: recording.metrics }
  _ <- runRIO (provideAll env program)
  records <- liftEffect recording.snapshot
  liftEffect (printSummary records)

program :: RIO Env () Unit
program = do
  -- Three independent sources, each emitting four events with
  -- different inter-event delays. Cadences are staggered so the
  -- merge picks up an interleaving that visibly comes from all
  -- three partitions.
  let
    sourceA = partitionStream "partition-A" 15.0 [ 100, 101, 102, 103 ]
    sourceB = partitionStream "partition-B" 25.0 [ 200, 201, 202, 203 ]
    sourceC = partitionStream "partition-C" 40.0 [ 300, 301, 302, 303 ]

  -- `mergeAll` runs every source concurrently and yields values
  -- as they land on a shared bounded queue; output order across
  -- sources is non-deterministic but each source's internal order
  -- is preserved.
  let merged = mergeAll [ sourceA, sourceB, sourceC ]

  -- Hand `merged` to `broadcast 2 16`: two consumer streams, each
  -- with its own 16-slot bounded buffer. The slowest consumer
  -- applies backpressure to the producer.
  consumers <- broadcast 2 16 merged
  case consumers of
    [ logStream, countStream ] -> do
      logFiber <- fork (runDrain (mapM logEvent logStream))
      countFiber <- fork (runDrain (mapM countEvent countStream))
      join logFiber
      join countFiber
    _ ->
      liftAff (liftEffect (Console.log "broadcast produced wrong arity"))

-- | Build a stream that emits each value with `delayMs` between
-- | yields and tags it with the source label.
partitionStream
  :: forall r e
   . String
  -> Number
  -> Array Int
  -> Stream r e Event
partitionStream label delayMs values = unfoldM 0 \i ->
  case Array.index values i of
    Nothing -> pure Nothing
    Just payload -> do
      liftAff (delay (Milliseconds delayMs))
      pure (Just (Tuple { source: label, payload } (i + 1)))

-- | Consumer #1: log each event as it arrives.
logEvent :: forall r. Event -> RIO r () Unit
logEvent ev = liftEffect
  (Console.log ("  [" <> ev.source <> "] " <> show ev.payload))

-- | Consumer #2: bump a per-source counter via the `Metrics`
-- | service.
countEvent :: Event -> RIO Env () Unit
countEvent ev =
  incrementCounter ("events." <> ev.source)

-- | After `runRIO` returns, aggregate per-name totals from the
-- | recording metrics snapshot and print the per-source counts.
-- | The recording backend captures every individual call; we
-- | sum them by name so the summary is a one-line-per-source
-- | rollup.
printSummary :: Array TestMetrics.MetricRecord -> Effect Unit
printSummary records = do
  let
    totals :: Map String Number
    totals = foldl
      ( \acc r ->
          Map.insert r.name (fromMaybe 0.0 (Map.lookup r.name acc) + r.value)
            acc
      )
      Map.empty
      records
  Console.log ""
  Console.log "Per-source counts:"
  traverse_
    ( \(Tuple name total) ->
        Console.log ("  " <> name <> " = " <> show total)
    )
    (Map.toUnfoldable totals :: Array _)
