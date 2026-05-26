## Tracing and metrics

`RIO.Fiber.Tracer` / `RIO.Fiber.Metrics` (and their `RIO.Aff.*`
siblings) are the observability hooks. Tracing is wired through
a per-fiber `FiberRef` in rio-fiber so the currently-active
span follows the fiber across nesting and forks (with copy-on-
fork inheritance), while rio-aff threads the currently-active
span through an environment-record swap over a private `Ref`,
so sibling forked fibers share the underlying cell rather than
each getting an independent copy. Metrics are a record of
operations that backends fill in, with a recording backend for
tests. Production OTel / StatsD / Prometheus backends plug into
the same surface without touching call sites.

> **Naming convention.** The tracing walkthrough below uses
> rio-fiber-shaped code samples: the `Span` is passed
> explicitly to `addAttribute` / `addEvent` / `addLink`, and
> the parent / child story is described in `FiberRef` terms.
> rio-aff's tracer surface differs in two ways worth knowing
> up front: (1) `addAttribute :: String -> String -> RIO
> (tracer :: Tracer | r) e Unit` takes no explicit `Span`
> argument and operates on the currently-active span via the
> `tracer` service in the row; (2) inheritance across forks
> uses a shared `Ref` rather than a copy-on-fork `FiberRef`,
> so a `withSpan` inside one forked fiber does not affect a
> sibling fiber only because the swap is scoped to the body,
> not because the two fibers see independent cells.

## Tracing

A `Tracer` lets you wrap a region of `RIO` work in a named span:

```purescript
import RIO.Fiber.Tracer (addAttribute, withSpan)

handleRequest
  :: forall r e
   . Request
  -> RIO r e Response
handleRequest req = withSpan "handle-request" [] \span -> do
  addAttribute span "request.id" (show req.id)
  addAttribute span "request.path" req.path
  ...
```

`withSpan name attrs body` opens a span before `body` runs and
finishes it on exit (success, failure, interrupt, or defect)
via `ensuring`. While `body` runs, the new span is the current
span for this fiber. Use `withSpanWith` for an explicit
`SpanKind` (`Server`, `Client`, `Producer`, `Consumer`,
`Internal`).

Span status is independent of `withSpan`'s success / failure
in rio-fiber: the span finishes with `StatusUnset` by default
unless the body calls `setStatus`:

- `StatusUnset` (rio-fiber default): most exporters treat as
  implicit OK.
- `StatusOk`: explicitly mark success.
- `StatusError msg`: explicitly mark failure with a message.

If you want a failed `withSpan` body to mark the span as
`StatusError` automatically, call `setStatus` in a `catchAll`
before re-raising.

rio-aff uses a different, finer-grained status shape that
`withSpan` sets automatically from the outcome: `SpanOk` on
success, `SpanFailed` on typed failure, `SpanInterrupted` if
the fiber is killed mid-action. There is no explicit-message
form on the aff side.

### Parent / child relationships

The currently-active span lives in a `FiberRef`. `withSpan`
sets it to the new span for the body's duration and restores
the previous value on exit; nested calls see the outer span as
their parent:

```purescript
withSpan "outer" [] \_ -> do
  withSpan "inner-a" [] \_ -> stepA  -- parent = outer
  withSpan "inner-b" [] \_ -> stepB  -- parent = outer
```

Fork inherits the current span by `FiberRef` copy-on-fork: the
child fiber starts with the parent's current span as its
initial value, but the two refs are independent after that, so
a `withSpan` inside the child doesn't push spans onto the
parent and vice versa. This is the structured form of the
implicit-context model OTel uses.

### Attributes, events, links

`addAttribute span key value` attaches a single string
key/value pair to the given span. `addEvent span name attrs`
records a timestamped event on the span (e.g. `"cache.miss"`
with `[ { key: "user.id", value: "42" } ]`); each `attrs`
entry is `{ key :: String, value :: String }`. `addLink span
target` adds a non-parent reference to another span (used for
cross-trace correlation, e.g. a batch span linking to the
producer spans of the records it consumed).

```purescript
withSpan "checkout" [] \span -> do
  addAttribute span "cart.size" (show (Array.length items))
  addEvent span "inventory.miss" [ { key: "sku", value: sku } ]
  ...
```

The `Span` is passed explicitly to every accessor so that
nested spans, forked children, and detached span handles can
all be addressed without ambiguity. `currentSpan` is available
when you want the active span without naming it.

### Backends

- `RIO.Fiber.Tracer.defaultTracer`: discards everything; every
  span carries the empty `SpanId`. The default.
- `RIO.Fiber.Test.Tracer.newRecordingTracer`: returns a
  `RecordingTracer` record `{ tracer :: Tracer, snapshot ::
  Effect (Array RecordedSpan) }`. Pass `rec.tracer` to
  `withTracer`; call `rec.snapshot` to read the captured span
  list. Each `RecordedSpan` carries id, parent id (if any),
  name, kind, virtual `startTick` / `endTick`, attributes,
  events, link target ids, and status. Virtual time starts at
  `0` and advances by `1` on every `startSpan` / `finish`.
- `RIO.Fiber.Tracer.OTel.exportSpans` / `renderOTLP`: pure
  helpers that shape a `RecordedSpan` snapshot into an
  OTLP/JSON document (emits `parentSpanId` and a `links`
  array). `exportSpans :: ExportConfig -> SpanIdMap -> Array
  RecordedSpan -> Json` takes a `SpanIdMap = Map SpanId String`
  that maps in-process `SpanId`s to W3C 16-hex external ids;
  spans absent from the map are dropped from the output. Pair
  with `RIO.Fiber.HttpClient.send` to ship to an OTel
  collector.
- `RIO.Fiber.Tracer.OTel.Adapter.makeOTelTracer` (from the
  `rio-fiber-otel` package): forwards every span lifecycle,
  attribute, event, link, and status write to an
  `@opentelemetry/api` tracer. Install an OpenTelemetry SDK
  (`sdk-node`, `sdk-trace-base`, etc.) and register a tracer
  provider at application startup; the adapter delegates from
  there. With no SDK registered the OTel API returns a no-op
  tracer and the adapter is silent. See `examples/otel-demo/`
  for end-to-end wiring.

Any backend (Honeycomb, Jaeger, custom) implements the same
`Tracer` record. Call sites do not change.

The aff package mirrors most of this surface: `RIO.Aff.Tracer`,
`RIO.Aff.Test.Tracer`, `RIO.Aff.Tracer.OTel`, with
`rio-aff-otel` for the live adapter. Two differences worth
flagging: the aff `Span` exposes attributes but no `addEvent`
or `addLink` operations, and the aff OTel adapter
(`RIO.Aff.Tracer.OTel.Adapter`) forwards lifecycle, attributes,
and status but does NOT forward span events or links to the
OTel SDK. For full event / link fidelity over OTel, use
rio-fiber + rio-fiber-otel.

## Metrics

A `Metrics` service lets you record counters, gauges, and
histograms. `rio-fiber` exposes the primitives directly as
stateful objects you allocate once and then mutate:

```purescript
import RIO.Fiber.Metrics
  ( Counter, Gauge, Histogram
  , newCounter             -- :: Effect Counter            (no args)
  , incr                   -- :: Counter -> RIO r e Unit
  , newGauge               -- :: Number -> Effect Gauge    (initial value)
  , set                    -- :: Gauge -> Number -> RIO r e Unit
  , newHistogram           -- :: Int -> Effect Histogram   (reservoir capacity)
  , record                 -- :: Histogram -> Number -> RIO r e Unit
  )
```

`newGauge` takes the initial gauge value; `newHistogram` takes
the reservoir capacity (the number of samples retained for
quantile / summary reads); only `newCounter` is zero-argument.

The `rio-aff` package uses a service-shaped form instead: the
same operations behind an `(metrics :: Metrics | r)` row,
keyed by `String` name rather than by allocated object
(`recordCounter name delta`, `recordGauge name value`,
`recordHistogram name value`).

### Backends

- `RIO.Fiber.Test.Metrics.newRecordingMetrics`: returns a
  `RecordingMetrics` record (`recordCounter :: String -> Number
  -> Effect Unit`, `recordGauge`, `recordHistogram`, and a
  `snapshot :: Effect (Array MetricRecord)` action). The
  recording backend is name-keyed, not object-keyed: it does
  *not* drop in at the same call site as production
  `incr counter` / `set gauge` / `record histogram`. To exercise
  it from a test, either wire your service layer through a
  metrics-service indirection that swaps between the production
  primitives and `recordCounter name value` writes, or call the
  recording actions directly from the test body at the points
  where production code mutates a primitive. Each captured
  record carries the kind (`Counter` / `Gauge` / `Histogram`),
  the name, and the value.
- `RIO.Fiber.Metric.OTel.exportMetrics` / `renderOTLP`: pure
  shaping into an OTLP/JSON metrics document. Counters sum
  per name, gauges keep last-value-wins, histograms aggregate
  into `{ count, sum, min, max }` per name.

A production backend (StatsD client, Prometheus push gateway,
OTel metrics exporter) implements the same record.

## What this cut does not give you

- **Sampled tracing.** The tracer records every span; sampling
  decisions are the backend's job.
- **Per-bucket histogram counts in the OTLP exporter.** The
  metrics exporter emits `count` / `sum` / `min` / `max` per
  histogram; per-bucket counts are not modelled. For real
  bucket counts, use the `BucketHistogram` type exported from
  `RIO.Fiber.Metrics` and consume its `bucketSnapshot`
  directly. (On the aff side, the equivalent lives in
  `RIO.Aff.Metric.BucketHistogram`.)
- **Async backends.** All recording operations are `Effect`-
  typed and run synchronously. A production backend that needs
  to push over the network should fork its own emitter fiber
  internally.

## Pointers

- `rio-fiber/src/RIO/Fiber/Tracer.purs`,
  `rio-fiber/src/RIO/Fiber/Test/Tracer.purs`: the tracer
  surface, opaque `Span` newtype, `SpanId`, `SpanKind`,
  `SpanStatus`, and the recording backend.
- `rio-fiber/src/RIO/Fiber/Tracer/OTel.purs`,
  `rio-fiber-otel/src/RIO/Fiber/Tracer/OTel/Adapter.purs`: pure
  OTLP shaping and the live OTel adapter.
- `rio-fiber/src/RIO/Fiber/Metrics.purs`,
  `rio-fiber/src/RIO/Fiber/Test/Metrics.purs`,
  `rio-fiber/src/RIO/Fiber/Metric/OTel.purs`: metrics
  primitives, recording backend, and OTLP exporter.
- The aff equivalents: `rio-aff/src/RIO/Aff/Tracer.purs`,
  `rio-aff/src/RIO/Aff/Test/Tracer.purs`,
  `rio-aff-otel/src/RIO/Tracer/OTel/Adapter.purs` (module
  `RIO.Aff.Tracer.OTel.Adapter`), and the metrics counterparts
  under `rio-aff/src/RIO/Aff/Metric*.purs`.
