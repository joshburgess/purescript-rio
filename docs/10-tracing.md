## Tracing and metrics

`RIO.Tracer` and `RIO.Metrics` are the observability hooks. Both
follow the standard service convention: a record of operations
that backends fill in, smart constructors on top that take an
`(observability :: ...)` row through the environment, and a
recording test backend that lets you assert on what a program
emitted.

The surface area is deliberately small; the shape is stable so
a production OTel / StatsD / Prometheus backend can sit on top
without touching call sites.

## Tracing

A `Tracer` lets you wrap a region of `RIO` work in a named span:

```purescript
import RIO.Tracer (Tracer, addAttribute, withSpan)

handleRequest
  :: forall r e
   . Request
  -> RIO (tracer :: Tracer | r) e Response
handleRequest req = withSpan "handle-request" do
  addAttribute "request.id" (show req.id)
  addAttribute "request.path" req.path
  ...
```

The span opens before the action runs and closes when it ends,
with one of three terminal statuses:

- `SpanOk` if the action returned a value.
- `SpanFailed` if the action raised a typed failure.
- `SpanInterrupted` if the fiber was killed before the action
  completed (the `withSpan` body never observed a success or a
  failure).

The close is guaranteed by `Aff.finally` so the span is closed
on every termination path, including an interrupt that lands
mid-action.

### Parent / child relationships

The `Tracer` service is responsible for tracking which span is
currently "active". `startSpan` makes the new span a child of
the active one and pushes it; `endSpan` pops it. `withSpan`
nests cleanly:

```purescript
withSpan "outer" do
  withSpan "inner-a" stepA  -- parent = outer
  withSpan "inner-b" stepB  -- parent = outer (after inner-a ended)
```

Behaviour under `fork`: the forked fiber inherits whichever span
was current at the point of fork, because the same `Tracer`
record (and its `Effect.Ref`s) is shared. After the fork, the
parent fiber's subsequent spans continue to land under the same
parent until the parent's `endSpan` runs. This is the implicit-
context model used by OTel-style tracers in single-threaded
runtimes; it works correctly for the common case of "fire a
background fiber and let it inherit the current span" but does
not survive a fork that outlives its parent's `withSpan`. When
you need explicit context, capture `currentSpan` at the fork
point and reattach with your own `withSpan` at the start of the
fiber.

### Attributes

`addAttribute key value` attaches a string key/value pair to
the currently-active span. It is a no-op outside any `withSpan`
region:

```purescript
withSpan "checkout" do
  addAttribute "cart.size" (show (Array.length items))
  addAttribute "user.tier" tier
  ...
```

Attributes accumulate in call order; the recording backend
preserves that order so tests can assert on it.

### Backends

- `RIO.Tracer.noopTracer`: discards everything. Useful as the
  default in environments that don't want tracing.
- `RIO.Test.Tracer.newRecordingTracer`: returns a `tracer` plus
  a `snapshot :: Effect (Array Span)` action. Virtual time
  starts at 0 and advances by 1 on every `startSpan` / `endSpan`,
  so tests can assert on the start/end order without depending
  on wall-clock timing.
- `RIO.Tracer.OTel.makeOTelTracer` (from the `rio-otel`
  package): forwards every span lifecycle, attribute write,
  and parent / child relationship to an `@opentelemetry/api`
  tracer. Install an OpenTelemetry SDK (`sdk-node`,
  `sdk-trace-base`, etc.) and register a tracer provider at
  application startup; the adapter delegates from there. With
  no SDK registered the OTel API returns a no-op tracer and
  the adapter is silent (the `Tracer` row is still satisfied,
  so program structure is unaffected). See
  `examples/otel-demo/` for a worked end-to-end wiring.

Any backend (Honeycomb, Jaeger, custom) implements the same
`Tracer` record. The call sites do not change.

## Metrics

A `Metrics` service lets you record counters, gauges, and
histograms:

```purescript
import RIO.Metrics
  ( Metrics
  , incrementCounter
  , observeHistogram
  , setGauge
  )

processBatch
  :: forall r e
   . Batch
  -> RIO (metrics :: Metrics | r) e Unit
processBatch batch = do
  incrementCounter "batch.received"
  setGauge "batch.size" (Number.fromInt batch.size)
  startMs <- liftEffect now
  ...do work...
  endMs <- liftEffect now
  observeHistogram "batch.latency.ms" (endMs - startMs)
```

The three operations take a metric name and a `Number` value;
the backend handles aggregation, tagging, and emission.
`incrementCounter` is `recordCounter name 1.0`; `setGauge` is
an alias for `recordGauge`; `observeHistogram` is an alias for
`recordHistogram`. The aliases are there because the verb at
the call site is what reads naturally for each metric kind.

### Backends

- `RIO.Metrics.noopMetrics`: discards everything.
- `RIO.Test.Metrics.newRecordingMetrics`: returns the service
  plus a `snapshot :: Effect (Array MetricRecord)` action that
  returns every emission in order. Each record carries the kind
  (`Counter` / `Gauge` / `Histogram`), the name, and the value.

A production backend (StatsD client, Prometheus push gateway,
OTel metrics exporter) implements the same `Metrics` record.

## What this cut does not give you

- **Span events / span links.** The current `Span` record has
  `attributes` but no separate event log or link list. If you
  need OTel-grade span data, the next iteration will add them.
- **Sampled tracing.** The tracer records every span; sampling
  decisions are the backend's job.
- **Aggregating histograms in the test backend.** The recording
  metrics backend captures raw emissions, not buckets. If you
  need to assert on percentiles, do the bucketing in the test.
- **Async backends.** All recording operations are `Effect`-
  typed and run synchronously inside the recording fiber. A
  production backend that needs to push over the network should
  fork its own emitter fiber internally.

## Pointers

- `src/RIO/Tracer.purs`, `src/RIO/Test/Tracer.purs`: the tracer
  service, span type, and the recording backend.
- `src/RIO/Metrics.purs`, `src/RIO/Test/Metrics.purs`: the
  metrics service and the recording backend.
- `test/Test/RIO/TracerSpec.purs`, `test/Test/RIO/MetricsSpec.purs`:
  tests for span lifecycle, parent/child nesting, attributes,
  failure status, and metric emission ordering.
