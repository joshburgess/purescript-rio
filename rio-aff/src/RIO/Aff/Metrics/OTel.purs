-- | Shape `RIO.Aff.Metrics` observations into the OTLP/JSON envelope
-- | used by OpenTelemetry collectors.
-- |
-- | The companion piece of `RIO.Aff.Tracer.OTel`: the exporter is pure,
-- | takes an in-process list of recorded observations (the kind
-- | `RIO.Aff.Test.Metrics.newRecordingMetrics`'s `snapshot` action
-- | returns), and produces the JSON document an OTLP receiver
-- | expects on its `/v1/metrics` HTTP endpoint. Performing the POST
-- | is the caller's job; pair the output with
-- | `RIO.Aff.HttpClient.post` and a `withJsonBody`.
-- |
-- | Aggregation policy across the input record list, keyed by
-- | `(name, kind)`:
-- |
-- | * Counter records (`MetricKind.Counter`) are summed into a
-- |   single cumulative monotonic data point per name.
-- | * Gauge records (`MetricKind.Gauge`) collapse to the last
-- |   recorded value per name (the OTel "last value wins"
-- |   semantics for gauges).
-- | * Histogram records (`MetricKind.Histogram`) are aggregated
-- |   per name into a single histogram data point that carries
-- |   `count`, `sum`, `min`, and `max`. Per-bucket counts are
-- |   omitted (no explicit bounds modelled here); receivers that
-- |   require bucket counts can derive them from the count/sum or
-- |   accept the simplified shape.
-- |
-- | The output is a minimal OTLP/JSON document covering the fields
-- | a typical viewer (the OTel Collector, Prometheus remote write
-- | bridges, Grafana Mimir) actually inspects: the metric name,
-- | the kind block, and a single data point per metric stamped
-- | with the configured time. Optional fields the library does
-- | not model (descriptions, units, attributes per data point,
-- | exemplars) are omitted.
module RIO.Aff.Metrics.OTel
  ( ExportConfig
  , exportMetrics
  , renderOTLP
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object

import RIO.Aff.Test.Metrics (MetricKind(..), MetricRecord)

-- | Static metadata the exporter attaches to every batch:
-- | `resource` describes the emitting service (the OTel
-- | "resource"); `scope` describes the instrumentation library
-- | (the OTel "instrumentation scope"); `timeUnixNano` stamps
-- | every emitted data point with the same observation time
-- | (the snapshot moment). Pass `show <ns>` where the integer
-- | is the current time in nanoseconds, or any agreed
-- | quantisation.
type ExportConfig =
  { resource :: { serviceName :: String, serviceVersion :: String }
  , scope :: { name :: String, version :: String }
  , timeUnixNano :: String
  }

-- | Build an OTLP/JSON metrics document from a list of recorded
-- | observations. The resulting `Json` matches the shape an OTel
-- | collector expects on `POST /v1/metrics`:
-- |
-- | ```json
-- | { "resourceMetrics": [ { "resource": ..., "scopeMetrics": [ ... ] } ] }
-- | ```
-- |
-- | Observations are grouped by `(name, kind)` and reduced as
-- | described in the module docstring. Input order across
-- | different metrics is preserved (first-mention wins in the
-- | output's metric array).
exportMetrics :: ExportConfig -> Array MetricRecord -> Json
exportMetrics cfg records =
  Json.fromObject
    ( Object.singleton "resourceMetrics"
        ( Json.fromArray
            [ Json.fromObject
                ( Object.fromFoldable
                    [ Tuple "resource" (resourceJson cfg.resource)
                    , Tuple "scopeMetrics"
                        ( Json.fromArray
                            [ scopeMetricsJson cfg.scope metricsJson ]
                        )
                    ]
                )
            ]
        )
    )
  where
  aggregated = aggregate records
  metricsJson =
    Json.fromArray (map (aggregateToJson cfg.timeUnixNano) aggregated)

-- | Render an OTLP/JSON document as a compact string. The same
-- | output `Json.stringify` would produce; named for symmetry
-- | with the rest of the exporter API.
renderOTLP :: Json -> String
renderOTLP = Json.stringify

-- | Per-metric aggregate produced by `aggregate`. The kind drives
-- | which OTLP block (`sum` / `gauge` / `histogram`) is emitted
-- | in `aggregateToJson`.
data Aggregate
  = CounterAgg String Number
  | GaugeAgg String Number
  | HistogramAgg
      String
      { count :: Int, sum :: Number, min :: Number, max :: Number }

aggregateName :: Aggregate -> String
aggregateName = case _ of
  CounterAgg n _ -> n
  GaugeAgg n _ -> n
  HistogramAgg n _ -> n

aggregateKey :: Aggregate -> Tuple String String
aggregateKey = case _ of
  CounterAgg n _ -> Tuple "counter" n
  GaugeAgg n _ -> Tuple "gauge" n
  HistogramAgg n _ -> Tuple "histogram" n

-- | Collapse a list of `MetricRecord`s into one aggregate per
-- | `(kind, name)` pair, preserving first-mention order across
-- | distinct metrics. Counter values are summed; gauge values
-- | keep the last; histogram values fold into a `{ count, sum,
-- | min, max }` record.
aggregate :: Array MetricRecord -> Array Aggregate
aggregate records =
  let
    Tuple order grouped =
      foldl step (Tuple [] Map.empty) records
  in
    Array.mapMaybe (\k -> Map.lookup k grouped) order
  where
  step
    :: Tuple (Array (Tuple String String)) (Map (Tuple String String) Aggregate)
    -> MetricRecord
    -> Tuple (Array (Tuple String String)) (Map (Tuple String String) Aggregate)
  step (Tuple order grouped) rec =
    let
      seed = freshAggregate rec
      key = aggregateKey seed
      grouped' = case Map.lookup key grouped of
        Nothing -> Map.insert key seed grouped
        Just existing -> Map.insert key (combine existing rec) grouped
      order' =
        if Map.member key grouped then order
        else Array.snoc order key
    in
      Tuple order' grouped'

  freshAggregate :: MetricRecord -> Aggregate
  freshAggregate { kind, name, value } = case kind of
    Counter -> CounterAgg name value
    Gauge -> GaugeAgg name value
    Histogram -> HistogramAgg name
      { count: 1, sum: value, min: value, max: value }

  combine :: Aggregate -> MetricRecord -> Aggregate
  combine agg rec = case agg, rec.kind of
    CounterAgg n total, Counter ->
      CounterAgg n (total + rec.value)
    GaugeAgg n _, Gauge ->
      GaugeAgg n rec.value
    HistogramAgg n h, Histogram ->
      HistogramAgg n
        { count: h.count + 1
        , sum: h.sum + rec.value
        , min: if rec.value < h.min then rec.value else h.min
        , max: if rec.value > h.max then rec.value else h.max
        }
    -- Kind mismatch can't happen: aggregateKey separates by kind,
    -- so combine only ever sees same-kind pairs. Keep the existing
    -- aggregate on the impossible branch rather than fabricate a
    -- new one.
    _, _ -> agg

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

scopeMetricsJson
  :: { name :: String, version :: String }
  -> Json
  -> Json
scopeMetricsJson s metricsJson =
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
        , Tuple "metrics" metricsJson
        ]
    )

aggregateToJson :: String -> Aggregate -> Json
aggregateToJson timeNs agg =
  Json.fromObject
    ( Object.fromFoldable
        [ Tuple "name" (Json.fromString (aggregateName agg))
        , Tuple (kindKey agg) (kindBlock timeNs agg)
        ]
    )
  where
  kindKey :: Aggregate -> String
  kindKey = case _ of
    CounterAgg _ _ -> "sum"
    GaugeAgg _ _ -> "gauge"
    HistogramAgg _ _ -> "histogram"

kindBlock :: String -> Aggregate -> Json
kindBlock timeNs = case _ of
  CounterAgg _ total ->
    Json.fromObject
      ( Object.fromFoldable
          [ Tuple "aggregationTemporality" (Json.fromNumber 2.0)
          , Tuple "isMonotonic" (Json.fromBoolean true)
          , Tuple "dataPoints"
              ( Json.fromArray
                  [ Json.fromObject
                      ( Object.fromFoldable
                          [ Tuple "startTimeUnixNano"
                              (Json.fromString timeNs)
                          , Tuple "timeUnixNano"
                              (Json.fromString timeNs)
                          , Tuple "asDouble" (Json.fromNumber total)
                          ]
                      )
                  ]
              )
          ]
      )
  GaugeAgg _ value ->
    Json.fromObject
      ( Object.singleton "dataPoints"
          ( Json.fromArray
              [ Json.fromObject
                  ( Object.fromFoldable
                      [ Tuple "timeUnixNano"
                          (Json.fromString timeNs)
                      , Tuple "asDouble" (Json.fromNumber value)
                      ]
                  )
              ]
          )
      )
  HistogramAgg _ h ->
    Json.fromObject
      ( Object.fromFoldable
          [ Tuple "aggregationTemporality" (Json.fromNumber 2.0)
          , Tuple "dataPoints"
              ( Json.fromArray
                  [ Json.fromObject
                      ( Object.fromFoldable
                          [ Tuple "startTimeUnixNano"
                              (Json.fromString timeNs)
                          , Tuple "timeUnixNano"
                              (Json.fromString timeNs)
                          , Tuple "count"
                              (Json.fromString (show h.count))
                          , Tuple "sum" (Json.fromNumber h.sum)
                          , Tuple "min" (Json.fromNumber h.min)
                          , Tuple "max" (Json.fromNumber h.max)
                          ]
                      )
                  ]
              )
          ]
      )

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
