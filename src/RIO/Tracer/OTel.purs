-- | Shape spans recorded by `RIO.Tracer` into the OTLP/JSON
-- | envelope used by OpenTelemetry collectors.
-- |
-- | The exporter is pure: it converts an in-process span list
-- | (the kind `RIO.Test.Tracer.snapshot` returns) into the JSON
-- | document that an OTLP receiver expects on its `/v1/traces`
-- | HTTP endpoint. Actually performing the POST is the caller's
-- | job; pair the output with `RIO.HttpClient.post` and a
-- | `withJsonBody`.
-- |
-- | The output is a *minimal* OTLP/JSON document covering the
-- | fields a typical viewer cares about: trace and span IDs,
-- | start and end times in Unix nanoseconds, the span name,
-- | string attributes, and a status code (`1 = Ok`, `2 = Error`).
-- | Optional fields the library does not model (links, events,
-- | kind, schema URL) are omitted. Receivers that accept partial
-- | documents (the OTel Collector, Tempo, Jaeger's OTLP intake)
-- | render them correctly; bespoke validators may need patches.
-- |
-- | In-process `SpanId` (`Int`) and the W3C 16-hex `spanId` /
-- | 32-hex `traceId` are different objects. This module accepts
-- | a `SpanIdMap` argument that maps every in-process id to its
-- | external hex string; the caller is responsible for assigning
-- | those ids (typically at the boundary where a request first
-- | enters the system, via `RIO.Tracer.Propagation.newSpanId`).
module RIO.Tracer.OTel
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

import RIO.Tracer (Span, SpanId, SpanStatus(..))

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
exportSpans :: ExportConfig -> SpanIdMap -> Array Span -> Json
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

spanToJson :: String -> SpanIdMap -> Span -> Maybe Json
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
                  , Tuple "startTimeUnixNano"
                      (Json.fromString (unixNanos sp.startMs))
                  , Tuple "endTimeUnixNano"
                      ( Json.fromString
                          (unixNanos (fromMaybe sp.startMs sp.endMs))
                      )
                  , Tuple "attributes" (attributesJson sp.attributes)
                  , Tuple "status" (statusJson sp.status)
                  ]
            )
        )
    )
  where
  parentSpanField = case sp.parent of
    Nothing -> []
    Just pid -> case Map.lookup pid ids of
      Nothing -> []
      Just hex -> [ Tuple "parentSpanId" (Json.fromString hex) ]

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

statusJson :: SpanStatus -> Json
statusJson = case _ of
  SpanOk -> Json.fromObject (Object.singleton "code" (Json.fromNumber 1.0))
  SpanFailed ->
    Json.fromObject
      ( Object.fromFoldable
          [ Tuple "code" (Json.fromNumber 2.0)
          , Tuple "message" (Json.fromString "span failed")
          ]
      )
  SpanInterrupted ->
    Json.fromObject
      ( Object.fromFoldable
          [ Tuple "code" (Json.fromNumber 2.0)
          , Tuple "message" (Json.fromString "span interrupted")
          ]
      )

unixNanos :: Number -> String
unixNanos ms = show (Int.floor ms * 1000000)
