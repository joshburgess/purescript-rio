# rio-aff-otel

OpenTelemetry adapter for [`rio-aff`](../rio-aff/README.md)'s `RIO.Aff.Tracer`
service.

```purescript
import RIO.Aff.Tracer (Tracer, addAttribute, withSpan)
import RIO.Aff.Tracer.OTel.Adapter (makeOTelTracer)

main = launchAff_ do
  tracer <- liftEffect (makeOTelTracer "my-service")
  runRIO' (provideAll { tracer } program)
```

`makeOTelTracer` returns a `Tracer` record that delegates span
lifecycle, attribute writes, and parent / child relationships
to an `@opentelemetry/api` tracer. Every call site that uses
`withSpan`, `addAttribute`, or `currentSpan` keeps working
verbatim; only the value placed into the environment record
changes.

## Wiring the SDK

`@opentelemetry/api` is the only npm dependency this package
needs at compile time. To actually export spans, install an
OpenTelemetry SDK (`@opentelemetry/sdk-node`,
`@opentelemetry/sdk-trace-base`, etc.) and register a tracer
provider at application startup, before the first `RIO`
program runs.

A worked example wiring `BasicTracerProvider` with an
`InMemorySpanExporter` lives in
[`../examples/otel-demo`](../examples/otel-demo). Run it with:

```sh
npx spago run -p rio-example-otel-demo
```

If no SDK is registered the OpenTelemetry API returns a no-op
tracer and this adapter is silent. The `Tracer` row is still
satisfied so program structure is unaffected; the difference
is purely "spans are emitted" vs "spans are dropped on the
floor."

## Status mapping

| RIO span status   | OTel `SpanStatusCode` | Notes                                     |
| ----------------- | --------------------- | ----------------------------------------- |
| `SpanOk`          | `OK`                  | the wrapped action returned a value       |
| `SpanFailed`      | `ERROR`               | the wrapped action raised a typed failure |
| `SpanInterrupted` | `ERROR`               | additionally sets message `"interrupted"` |

## Limitations

- **Span events and links are not forwarded.** The `RIO.Aff.Tracer`
  record intentionally exposes only attributes and status, so the
  adapter has nothing to forward. For full event / link fidelity
  over OTel, use the premier `rio-fiber` + `rio-fiber-otel` pair,
  which model `addEvent`, `addLink`, and an explicit `SpanKind`
  end-to-end.
- **No sampler configuration knob.** The adapter delegates
  sampling to whatever sampler the SDK configures globally.
  Configure it in your SDK setup, not at the call site.

## License

MIT. See the [top-level `LICENSE`](../LICENSE).
