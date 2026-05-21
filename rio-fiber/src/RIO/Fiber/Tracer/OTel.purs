-- | Shape spans recorded by `RIO.Fiber.Test.Tracer` into the
-- | OTLP/JSON envelope used by OpenTelemetry collectors.
-- |
-- | The exporter is pure: it converts an in-process span list
-- | (the kind `RIO.Fiber.Test.Tracer.newRecordingTracer`'s
-- | `snapshot` action returns) into the JSON document that an
-- | OTLP receiver expects on its `/v1/traces` HTTP endpoint.
-- | Actually performing the POST is the caller's job; pair the
-- | output with `RIO.Fiber.HttpClient.send` and a JSON body.
-- |
-- | The output is a *minimal* OTLP/JSON document covering the
-- | fields a typical viewer cares about: trace and span IDs,
-- | start and end times in Unix nanoseconds, the span name,
-- | string attributes, the `SpanKind`, and a status code
-- | (`1 = Ok`, `2 = Error`). Optional fields the library does
-- | not model (events as a first-class collection, link
-- | attributes, schema URL) are omitted. Receivers that accept
-- | partial documents (the OTel Collector, Tempo, Jaeger's OTLP
-- | intake) render them correctly; bespoke validators may need
-- | patches.
-- |
-- | The in-process `SpanId` (whatever shape the producing tracer
-- | chooses; the recording tracer emits decimal sequence numbers)
-- | and the W3C 16-hex `spanId` / 32-hex `traceId` are different
-- | objects. This module accepts a `SpanIdMap` argument that maps
-- | every in-process id to its external hex string; the caller is
-- | responsible for assigning those ids (typically at the boundary
-- | where a request first enters the system, via
-- | `RIO.Fiber.Tracer.Propagation.newSpanId`). Parents and link
-- | targets whose ids are missing from the map are dropped from
-- | the emitted span rather than emitted with a placeholder.
-- |
-- | ## Tick time vs. wall clock
-- |
-- | `RecordedSpan.startTick` / `endTick` are virtual tick
-- | counters from the recording tracer, not real timestamps.
-- | The exporter scales them into nanoseconds by treating each
-- | tick as one millisecond. For exports that need real
-- | timestamps (e.g. correlation with other observability
-- | systems), wire a production tracer instead of the recording
-- | one; the in-memory recorder is for testing the export
-- | shape, not for shipping live data.
module RIO.Fiber.Tracer.OTel
  ( ExportConfig
  , SpanIdMap
  , exportSpans
  , renderOTLP
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

import RIO.Fiber.Test.Tracer (RecordedSpan)
import RIO.Fiber.Tracer (SpanId, SpanKind(..), SpanStatus(..))

-- | Static metadata the exporter attaches to every batch:
-- | `resource` describes the emitting service (the OTel
-- | "resource"); `scope` describes the instrumentation library
-- | (the OTel "instrumentation scope"); `traceId` is the W3C
-- | trace identifier that every exported span belongs to.
type ExportConfig =
  { resource :: { serviceName :: String, serviceVersion :: String }
  , scope :: { name :: String, version :: String }
  , traceId :: String
  }

-- | A mapping from in-process `SpanId`s to W3C-shaped (16-hex)
-- | external span ids. Spans whose id is missing from the map
-- | are dropped by `exportSpans` to keep the OTLP output
-- | consistent; the test suite covers that fallback.
type SpanIdMap = Map SpanId String

-- | Build an OTLP/JSON document from a list of recorded spans.
-- | The resulting `Json` matches the shape an OTel collector
-- | expects on `POST /v1/traces`:
-- |
-- | ```json
-- | { "resourceSpans": [ { "resource": ..., "scopeSpans": [ ... ] } ] }
-- | ```
exportSpans :: ExportConfig -> SpanIdMap -> Array RecordedSpan -> Json
exportSpans cfg ids spans =
  Json.fromObject
    ( Object.singleton "resourceSpans"
        ( Json.fromArray
            [ Json.fromObject
                ( Object.fromFoldable
                    [ Tuple "resource" (resourceJson cfg.resource)
                    , Tuple "scopeSpans"
                        ( Json.fromArray
                            [ scopeSpansJson cfg.scope spansJson ]
                        )
                    ]
                )
            ]
        )
    )
  where
  spansJson =
    Json.fromArray (Array.mapMaybe (spanToJson cfg.traceId ids) spans)

-- | Render an OTLP/JSON document as a compact string. The same
-- | output `Json.stringify` would produce; named for symmetry
-- | with the rest of the exporter API.
renderOTLP :: Json -> String
renderOTLP = Json.stringify

resourceJson
  :: { serviceName :: String, serviceVersion :: String } -> Json
resourceJson r =
  Json.fromObject
    ( Object.singleton "attributes"
        ( Json.fromArray
            [ kv "service.name" r.serviceName
            , kv "service.version" r.serviceVersion
            ]
        )
    )

scopeSpansJson
  :: { name :: String, version :: String }
  -> Json
  -> Json
scopeSpansJson s spansJson =
  Json.fromObject
    ( Object.fromFoldable
        [ Tuple "scope"
            ( Json.fromObject
                ( Object.fromFoldable
                    [ Tuple "name" (Json.fromString s.name)
                    , Tuple "version" (Json.fromString s.version)
                    ]
                )
            )
        , Tuple "spans" spansJson
        ]
    )

spanToJson :: String -> SpanIdMap -> RecordedSpan -> Maybe Json
spanToJson traceId ids sp = case Map.lookup sp.id ids of
  Nothing -> Nothing
  Just spanHex -> Just
    ( Json.fromObject
        ( Object.fromFoldable
            ( [ Tuple "traceId" (Json.fromString traceId)
              , Tuple "spanId" (Json.fromString spanHex)
              ]
                <> parentSpanField
                <>
                  [ Tuple "name" (Json.fromString sp.name)
                  , Tuple "kind" (Json.fromNumber (kindCode sp.kind))
                  , Tuple "startTimeUnixNano"
                      (Json.fromString (unixNanos sp.startTick))
                  , Tuple "endTimeUnixNano"
                      ( Json.fromString
                          (unixNanos (fromMaybe sp.startTick sp.endTick))
                      )
                  , Tuple "attributes" (attributesJson sp.attributes)
                  , Tuple "status" (statusJson sp.status)
                  ]
                <> linksField
            )
        )
    )
  where
  parentSpanField = case sp.parent of
    Nothing -> []
    Just pid -> case Map.lookup pid ids of
      Nothing -> []
      Just hex -> [ Tuple "parentSpanId" (Json.fromString hex) ]

  linksField = case Array.mapMaybe (linkJson traceId ids) sp.links of
    [] -> []
    ls -> [ Tuple "links" (Json.fromArray ls) ]

linkJson :: String -> SpanIdMap -> SpanId -> Maybe Json
linkJson traceId ids target = case Map.lookup target ids of
  Nothing -> Nothing
  Just hex -> Just
    ( Json.fromObject
        ( Object.fromFoldable
            [ Tuple "traceId" (Json.fromString traceId)
            , Tuple "spanId" (Json.fromString hex)
            ]
        )
    )

attributesJson :: Array (Tuple String String) -> Json
attributesJson attrs =
  Json.fromArray (map (\(Tuple k v) -> kv k v) attrs)

kv :: String -> String -> Json
kv k v =
  Json.fromObject
    ( Object.fromFoldable
        [ Tuple "key" (Json.fromString k)
        , Tuple "value"
            ( Json.fromObject
                (Object.singleton "stringValue" (Json.fromString v))
            )
        ]
    )

-- | OTLP span kind enum: `Internal = 1`, `Server = 2`,
-- | `Client = 3`, `Producer = 4`, `Consumer = 5`. (`0` is
-- | reserved for "unspecified" and is never emitted here.)
kindCode :: SpanKind -> Number
kindCode = case _ of
  Internal -> 1.0
  Server -> 2.0
  Client -> 3.0
  Producer -> 4.0
  Consumer -> 5.0

statusJson :: SpanStatus -> Json
statusJson = case _ of
  StatusUnset -> Json.fromObject (Object.singleton "code" (Json.fromNumber 0.0))
  StatusOk -> Json.fromObject (Object.singleton "code" (Json.fromNumber 1.0))
  StatusError msg ->
    Json.fromObject
      ( Object.fromFoldable
          [ Tuple "code" (Json.fromNumber 2.0)
          , Tuple "message" (Json.fromString msg)
          ]
      )

unixNanos :: Number -> String
unixNanos ms = show (Int.floor ms * 1000000)
