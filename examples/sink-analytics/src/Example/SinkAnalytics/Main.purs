-- | Single-pass analytics over a stream of HTTP request records,
-- | using `RIO.Aff.Sink` and `zipPar` to compose five independent
-- | aggregations into one sink that runs in lockstep against the
-- | same stream.
-- |
-- | The point of this example: each individual sink (`count`,
-- | `filterIn isError count`, `foldL`-max, `foldL` building a Set,
-- | `find`) is small and obvious. `zipPar` is what lets them all
-- | observe the same stream without re-iterating it, without
-- | spawning fan-out fibers, and without the call site having to
-- | thread the per-input dispatch by hand.
-- |
-- | Run with:
-- |
-- | ```
-- | npx spago run -p rio-example-sink-analytics
-- | ```
module Example.SinkAnalytics.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console

import RIO.Aff.Core (runRIO')
import RIO.Aff.Sink
  ( Sink
  , count
  , filterIn
  , find
  , foldL
  , mapInput
  , mapResult
  , runSink
  , zipPar
  )
import RIO.Aff.Stream (fromArray)

type Request =
  { id :: Int
  , path :: String
  , status :: Int
  , latencyMs :: Int
  }

type Summary =
  { total :: Int
  , errors :: Int
  , maxLatency :: Int
  , distinctPaths :: Set String
  , firstSlow :: Maybe Request
  }

-- | Synthetic access log. Mixed status codes, a couple of slow
-- | requests, and repeated paths so the distinct-paths sink has
-- | something to dedupe.
requests :: Array Request
requests =
  [ { id: 1, path: "/health", status: 200, latencyMs: 5 }
  , { id: 2, path: "/users", status: 200, latencyMs: 23 }
  , { id: 3, path: "/users", status: 200, latencyMs: 18 }
  , { id: 4, path: "/orders", status: 500, latencyMs: 410 }
  , { id: 5, path: "/users/42", status: 404, latencyMs: 12 }
  , { id: 6, path: "/orders", status: 200, latencyMs: 65 }
  , { id: 7, path: "/health", status: 200, latencyMs: 3 }
  , { id: 8, path: "/users", status: 500, latencyMs: 220 }
  , { id: 9, path: "/orders", status: 200, latencyMs: 1100 }
  , { id: 10, path: "/health", status: 200, latencyMs: 4 }
  ]

isError :: Request -> Boolean
isError r = r.status >= 500

isSlow :: Request -> Boolean
isSlow r = r.latencyMs > 500

-- | One sink per aggregation. Each is small and self-describing.
totalSink :: forall r e. Sink r e Request Int
totalSink = count

errorSink :: forall r e. Sink r e Request Int
errorSink = filterIn isError count

maxLatencySink :: forall r e. Sink r e Request Int
maxLatencySink = mapInput _.latencyMs (foldL 0 max)

distinctPathsSink :: forall r e. Sink r e Request (Set String)
distinctPathsSink = foldL Set.empty (\acc r -> Set.insert r.path acc)

firstSlowSink :: forall r e. Sink r e Request (Maybe Request)
firstSlowSink = find isSlow

-- | Compose the five sinks with `zipPar`. The result type is a
-- | nested tuple; `mapResult` flattens it into the named `Summary`
-- | record.
summarySink :: forall r e. Sink r e Request Summary
summarySink = mapResult assemble combined
  where
  combined =
    totalSink
      `zipPar` errorSink
      `zipPar` maxLatencySink
      `zipPar` distinctPathsSink
      `zipPar` firstSlowSink

  assemble
    ( Tuple
        ( Tuple
            ( Tuple
                ( Tuple total errors
                )
                maxLat
            )
            paths
        )
        firstSlow
    ) =
    { total
    , errors
    , maxLatency: maxLat
    , distinctPaths: paths
    , firstSlow
    }

main :: Effect Unit
main = launchAff_ do
  summary <- runRIO' (runSink summarySink (fromArray requests))
  liftEffect (printSummary summary)

printSummary :: Summary -> Effect Unit
printSummary s = do
  Console.log "Request log summary (one stream pass via zipPar):"
  Console.log ("  total requests  : " <> show s.total)
  Console.log ("  error responses : " <> show s.errors)
  Console.log ("  max latency ms  : " <> show s.maxLatency)
  Console.log ("  distinct paths  : " <> show (Set.size s.distinctPaths))
  Console.log
    ( "  paths seen      : "
        <> joinWith ", " (Set.toUnfoldable s.distinctPaths :: Array String)
    )
  case s.firstSlow of
    Nothing ->
      Console.log "  first slow req  : none (>500ms)"
    Just r ->
      Console.log
        ( "  first slow req  : #" <> show r.id
            <> " "
            <> r.path
            <> " ("
            <> show r.latencyMs
            <> "ms)"
        )
