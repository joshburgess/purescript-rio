# rio

A ZIO / Effect-TS style effect type for PureScript: typed
environment, typed errors, resource safety, layers, and
structural concurrency, all sitting on top of `Aff`.

```purescript
newtype RIO r e a = RIO (Record r -> Aff (Either (Variant e) a))
```

- `r` is a **row of services** the computation needs.
- `e` is a **row of typed failures** it may raise.
- `a` is the **success value** on the happy path.

Both rows are open by default, so requirements aggregate
automatically on composition and shrink as services are
provided and failures are handled.

## Why

If you've used ZIO in Scala or Effect-TS in TypeScript, you
already know the shape. PureScript's row types let the same
three-channel design feel native: the requirements channel is
a record row, the error channel is a `Variant` row, and the
type inferer aggregates and shrinks both as your program
composes.

If you have not used those libraries, the short version: `RIO`
is a description of an asynchronous computation that knows what
it needs, what it can fail with, and what it produces, and
gives you uniform plumbing to wire those together. You write
handlers, services, and tests against typed interfaces; you
provide implementations once at the edge.

## A 30-second tour

```purescript
import RIO.Core
import Type.Proxy (Proxy(..))

type Logger = { log :: String -> Aff Unit }

greet
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
greet name = do
  logger <- ask (Proxy :: Proxy "logger")
  liftAff (logger.log ("hello, " <> name))

main :: Effect Unit
main = launchAff_ do
  let consoleLogger = { log: \s -> liftEffect (Console.log s) }
  runRIO' (provideAll { logger: consoleLogger } (greet "world"))
```

A handler that may fail picks up its tag automatically:

```purescript
findTodo :: Int -> RIO (store :: TodoStore) (notFound :: { id :: Int }) Todo
findTodo tid = do
  store <- ask (Proxy :: Proxy "store")
  row <- liftAff (store.get tid)
  case row of
    Nothing -> fail (Proxy :: Proxy "notFound") { id: tid }
    Just todo -> pure todo
```

The `notFound` tag flows up through callers until a
`catchTag (Proxy :: _ "notFound")` removes it from the row,
or `runRIO` surfaces it as a `Left` for the entry point to
render.

## Install

`rio` is published to the PureScript registry. Add it to your
`spago.yaml` as a dependency:

```yaml
dependencies:
  - rio
```

Then `npx spago install`.

## What's in v0.1.0

- `RIO.Core`: the type, runners, and the most common
  re-exports.
- `RIO.Env`: `ask` / `asks` to read services, `provide` /
  `provideAll` to inject them.
- `RIO.Error`: `fail`, `catchTag`, `catchAll`, `mapError`,
  plus `die` / `sandbox` / `unsandbox` for the defect channel.
- `RIO.Resource`: `acquireRelease`, `ensuring`, and `Scope` with
  LIFO finalizers; release runs on success, typed failure,
  defect, and interruption.
- `RIO.Layer`: `Layer rIn e rOut`, with sequential (`>>>`)
  and horizontal (`<+>`) composition, `buildLayer`,
  `provideLayer`, and `passthrough` for keeping a layer's input
  services visible downstream.
- `RIO.Concurrency`: `fork`, `forkScoped`, `join`, `interrupt`,
  `uninterruptible`, `timeout`, `parTraverse` (short-circuit on
  first failure), `parTraverseN` (bounded), `parSequence`,
  `zipPar`, `race`, `raceAll`. Cancellation is cooperative
  (`Aff`-style); resources held by losers in a race or a
  short-circuited traversal are released.
- `RIO.Deferred`: one-shot write-once cell over
  `Effect.Aff.AVar` for fiber handshakes (`makeDeferred`,
  `succeedDeferred`, `failDeferred`, `awaitDeferred`,
  `pollDeferred`).
- `RIO.Clock`: the `Clock` service plus `liveClock`.
- `RIO.Schedule`: pure scheduling policies (`recurs`, `spaced`,
  `exponential`, `jittered`, `intersect`, `andThen`, `whileInput`)
  with runners `repeat`, `retry`, `retryOrElse` that sleep through
  `Clock` so the virtual-time test clock can drive them.
- `RIO.STM`: software-transactional memory. `TRef`, `STM e a`,
  `newTRef` / `readTRef` / `writeTRef` / `modifyTRef`, `retry` /
  `check`, `orElse`, `failSTM`, and `atomically`. Single-event-loop
  atomicity: no version checks, no spinning.
- `RIO.STM.TQueue`, `RIO.STM.TMap`, `RIO.STM.TSemaphore`: derived
  transactional structures. Blocking FIFO queue, keyed map with
  `awaitKey`, counting semaphore with `withTSemaphore` bracketing.
- `RIO.Tracer`: spans with `withSpan` and `addAttribute`. Implicit
  parent / child context via a tracer-held current-span pointer.
  `noopTracer` for production opt-out.
- `RIO.Metrics`: counter / gauge / histogram service.
  `noopMetrics` plus `RIO.Test.Metrics` recording backend.
- `RIO.Local`: ambient state with scoped overrides. `Local a`
  cells with `get` / `set` / `update` and a `locally fl value
  action` combinator whose restore is guaranteed by
  `Aff.finally`. The v0.3 analogue of ZIO `FiberRef`.
- `RIO.Logger`: structured logging service. Five levels, smart
  constructors per level, and `withField` / `withFields` for
  scoped ambient annotations attached to every emission inside
  a block. Backends: `noopLogger`, `consoleLogger`, and
  `RIO.Test.Logger.newRecordingLogger` for assertion-friendly
  tests.
- `RIO.Spec`: `itRIO` / `itRIO_` adapters for
  `purescript-spec`.
- `RIO.Test`: `mockService`, `recording` for service-call
  assertions.
- `RIO.Test.Clock`: `newTestClock` for deterministic
  virtual-time testing.
- `RIO.Test.Tracer`, `RIO.Test.Metrics`: recording backends for
  the observability services.

## Documentation

Walkthrough docs:

- [`docs/01-core-type.md`](./docs/01-core-type.md): the three
  parameters, how the channels compose.
- [`docs/02-services.md`](./docs/02-services.md): service
  convention and idiomatic provision.
- [`docs/03-errors.md`](./docs/03-errors.md): typed failures,
  catching, and the defect channel.
- [`docs/06-concurrency.md`](./docs/06-concurrency.md): fork,
  race, parallel traversal, cancellation caveats.
- [`docs/07-testing.md`](./docs/07-testing.md): the spec
  adapters and the virtual-time clock.
- [`docs/08-scheduling.md`](./docs/08-scheduling.md): retry and
  repeat policies, combinators, and how to drive them
  deterministically in tests.
- [`docs/09-stm.md`](./docs/09-stm.md): the STM type, primitives,
  atomicity semantics on the JS event loop, and `retry` / `orElse`.
- [`docs/10-tracing.md`](./docs/10-tracing.md): tracing and
  metrics services, parent / child span context, and the
  recording test backends.
- [`docs/11-fiber-local.md`](./docs/11-fiber-local.md): ambient
  state via `RIO.Local`, scoped overrides with `locally`, and
  the fork-inheritance trade-off relative to ZIO `FiberRef`.
- [`docs/12-logging.md`](./docs/12-logging.md): structured
  logging via `RIO.Logger`, scoped annotations with
  `withFields`, the shipped backends, and the comparison to
  ZIO `ZLogger` / Effect-TS `Effect.logAnnotations`.
- [`docs/performance.md`](./docs/performance.md): benchmark
  baselines and dominant costs.

Migration guides for users coming from other ecosystems:

- [`docs/migrating-from-zio.md`](./docs/migrating-from-zio.md)
- [`docs/migrating-from-effect-ts.md`](./docs/migrating-from-effect-ts.md)

Worked example:

- [`examples/todo-api/`](./examples/todo-api/): a small
  HTTPurple service demonstrating layers, typed failures,
  in-memory persistence, and JSON codec bridging.

## Build

```sh
npm install
npx spago build
npx spago test
```

Run the example:

```sh
npx spago run -p rio-example-todo-api
```

Run the benchmark suite:

```sh
npx spago run -p rio-benchmarks
```

## Status

v0.1.0 covers the production core: services, typed errors,
resource safety, layers, concurrency, and a testing toolkit.
The v0.2 work has landed on `main`: bounded-concurrency
traversal (`parTraverseN`), `timeout`, `uninterruptible`,
`forkScoped`, `Deferred`, `ensuring`, `Layer.passthrough`,
short-circuiting parallel traversal, `RIO.Schedule` (retry /
repeat policies), `RIO.STM` (`TRef` + `atomically`, plus derived
`TQueue` / `TMap` / `TSemaphore`), the benchmark regression
gate (`Benchmarks.Gate`, profile-driven, running on CI in
informational mode), tracing / metrics (`RIO.Tracer`,
`RIO.Metrics` with recording test backends), and an
OpenTelemetry adapter for `RIO.Tracer` (`rio-otel`). The only
remaining v0.2 follow-up is capturing the `ci-ubuntu-latest`
baseline from the first informational gate run and promoting
the CI step to required.

v0.3 work in progress: `RIO.Local` (ambient state with scoped
overrides via `locally`, the v0.3 analogue of ZIO `FiberRef`;
see `docs/11-fiber-local.md`) and `RIO.Logger` (structured
logging with scoped annotations via `withFields`, console and
in-memory backends; see `docs/12-logging.md`). See
[`PROJECT_BUILD_PLAN.md`](./PROJECT_BUILD_PLAN.md).

## License

MIT. See [`LICENSE`](./LICENSE).
