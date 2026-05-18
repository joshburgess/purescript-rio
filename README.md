# rio

A ZIO / Effect style effect type for PureScript: typed
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

> **Before you commit**, read
> [`docs/aff-constraints.md`](./docs/aff-constraints.md). `rio`
> sits on top of `Effect.Aff`; that is a deliberate choice and a
> hard upper bound. The doc spells out what the bound looks like
> in practice (typed errors as wrapping, no fiber identity or
> supervision, scheduling on the JS event loop, `Local` as shared
> rather than per-fiber state, virtual time via `Clock`
> discipline, and so on) and what a future custom fiber runtime
> would have to provide to escape it. It is the most useful
> single page in this repository if you are evaluating `rio` for
> production.

## Why

If you've used ZIO in Scala or Effect in TypeScript, you
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

`rio` is not yet published to the PureScript registry. To try it
out, clone the repository and use it as a local workspace package,
or point your `spago.yaml` at the git remote directly.

## What's included

- `RIO.Core`: the type, runners, and the most common
  re-exports.
- `RIO.Env`: `ask` / `asks` to read services, `provide` /
  `provideAll` to inject them.
- `RIO.Error`: `fail`, `catchTag`, `catchAll`, `mapError`,
  plus `die` / `sandbox` / `unsandbox` for the defect channel.
- `RIO.Cause`: parallel + sequential failure trees.
  `attemptCause` reifies any outcome; `parTraverseCause` /
  `parSequenceCause` collect every failure from N parallel
  branches into a left-leaning `Parallel` tree; `raceCause` and
  `acquireReleaseCause` are the cause-tracking variants of
  `race` and `acquireRelease`. `prettyCause` /
  `prettyCauseWithStack` render the tree with JS stacks under
  each `Die` leaf when one is available.
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
- `RIO.Random`: a small randomness service (`int`, `number`,
  `boolean`, `oneOf`) with `liveRandom`. `RIO.Test.Random`
  ships a seeded recording backend for deterministic tests.
- `RIO.Config`: a small Config DSL. Applicative descriptors
  (`string`, `int`, `boolean`, `secret`, `optional`,
  `withDefault`, `nested`) that pull from a `Source`; ships
  `envSource` (live process env) and `mapSource` (pure, for
  tests). Errors accumulate so one bad key doesn't hide the
  rest. `Secret` redacts under `Show`. The `rio-config-file`
  companion adds `dotenvFileSource` and `jsonFileSource`.
  `RIO.Config.Rotating` provides a refreshable cell with a
  `withRotation` helper that wires a `Config` loader to an
  on-demand `refresh` action.
- `RIO.Schedule`: pure scheduling policies (`recurs`, `spaced`,
  `exponential`, `jittered`, `intersect`, `andThen`, `whileInput`)
  with runners `repeat`, `retry`, `retryOrElse` that sleep through
  `Clock` so the virtual-time test clock can drive them.
- `RIO.STM`: software-transactional memory. `TRef` (with a
  `TVar` alias for `ZIO`/`Effect` muscle memory), `STM e a`,
  `newTRef` / `readTRef` / `writeTRef` / `modifyTRef`, `retry` /
  `check`, `orElse`, `failSTM`, and `atomically`. Single-event-loop
  atomicity: no version checks, no spinning.
- `RIO.STM.TQueue`, `RIO.STM.TMap`, `RIO.STM.TSemaphore`,
  `RIO.STM.THub`: derived transactional structures. Blocking
  FIFO queue, keyed map with `awaitKey`, counting semaphore
  with `withTSemaphore` bracketing, and a pub/sub hub with
  per-subscriber buffers and four back-pressure strategies
  (`Bounded`, `Sliding`, `Dropping`, `Unbounded`).
- `RIO.Queue`, `RIO.Hub`, `RIO.Semaphore`: non-STM async
  primitives. `Queue.bounded` / `Queue.unbounded` with blocking
  `offer` / `take` and `shutdown`-as-end-of-stream; `Hub` as a
  pub/sub fan-out with per-subscriber queues; `Semaphore` with
  `withPermit` bracketing.
- `RIO.Stream`: a pull-based effectful stream. `fromArray`,
  `single`, `unfoldM`, `repeatM`, `map`, `filter`, `take`,
  `drop`, `concat`, `flatMap`, `mapM`, plus runners
  (`runDrain`, `runCollect`, `runFold`, `runFoldM`). Each step
  is a single `RIO` action that returns either `Yield a rest` or
  `Done`.
- `RIO.Stream.Par`: parallel stream combinators. `mergeAll`,
  `merge`, `mergeMap` fan in N producers onto a shared bounded
  queue; `broadcast` fans one upstream out to N consumer streams
  with end-to-end backpressure; `partition` routes each element
  to one of N buckets via a key function. First failure shuts
  the queue down and is propagated by every consumer.
- `RIO.Stream.Resource`: `bracketStream`, a single-element
  resource-acquiring stream whose release is registered with the
  enclosing `scoped` block. Compose with `flatMap` to thread the
  acquired resource through a multi-element downstream.
- `RIO.Sink`: composable terminating consumers for `RIO.Stream`.
  Primitive sinks (`drain`, `head`, `last`, `count`, `collect`,
  `foldL`, `foldM`), short-circuiting sinks (`take`, `find`,
  `any`, `all`), and combinators (`mapResult`, `mapInput`,
  `filterIn`, `andThen`, `zipPar`, `zipParWith`) with a single
  `runSink` runner.
- `RIO.Channel`: a minimal unified producer / consumer /
  transducer shape `Channel r e i o d`. `fromStream` / `fromSink`
  bridge into existing pipelines; `pipe` (`upstream >>>
  downstream`) and `run` compose and execute a closed pipeline.
  The Channel layer is the bedrock primitive in ZIO and
  Effect, exposed here so users can write stream-to-stream
  transducers as first-class values when `Stream.mapM` /
  `Stream.flatMap` / `Sink.andThen` are not enough.
- `RIO.Tracer`: spans with `withSpan` and `addAttribute`. Implicit
  parent / child context via a tracer-held current-span pointer.
  `noopTracer` for production opt-out.
- `RIO.Metrics`: counter / gauge / histogram service.
  `noopMetrics` plus `RIO.Test.Metrics` recording backend.
- `RIO.Local`: ambient state with scoped overrides. `Local a`
  cells with `get` / `set` / `update` and a `locally fl value
  action` combinator whose restore is guaranteed by
  `Aff.finally`. The RIO analogue of ZIO `FiberRef`.
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

Foundational reading:

- [`docs/aff-constraints.md`](./docs/aff-constraints.md): the
  hard constraint that `rio` is built on `Effect.Aff`, what that
  gives us for free, what it cannot give us, and what a future
  custom fiber runtime would have to provide to lift each
  ceiling. Read this before adopting `rio` for anything
  load-bearing.

Walkthrough docs:

- [`docs/01-core-type.md`](./docs/01-core-type.md): the three
  parameters, how the channels compose.
- [`docs/02-services.md`](./docs/02-services.md): service
  convention and idiomatic provision.
- [`docs/03-errors.md`](./docs/03-errors.md): typed failures,
  catching, and the defect channel.
- [`docs/04-layers.md`](./docs/04-layers.md): the `Layer` type,
  vertical (`>>>`) and horizontal (`<+>`) composition,
  `provideLayer`, and the resource-safe layer story.
- [`docs/05-resources.md`](./docs/05-resources.md):
  `acquireRelease`, `ensuring`, `Scope` / `scoped`, and the
  `RIO.Resource.Do` qualified-do sugar.
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
  ZIO `ZLogger` / Effect `Effect.logAnnotations`.
- [`docs/13-streams.md`](./docs/13-streams.md): pull-based
  `RIO.Stream`, parallel combinators in `RIO.Stream.Par`
  (`mergeAll`, `broadcast`, `partition`), and resource-safe
  streams via `RIO.Stream.Resource.bracketStream`. Includes a
  ZStream comparison table.
- [`docs/14-causes.md`](./docs/14-causes.md): the `Cause`
  algebra (`Fail`, `Die`, `Parallel`, `Sequential`), the
  cause-collecting combinators (`parTraverseCause`,
  `bothPar`, `raceCause`, `acquireReleaseCause`), and the
  `prettyCause` renderer.
- [`docs/15-config.md`](./docs/15-config.md): the `Config`
  descriptor type, the `Source` adapter set (env, dotenv,
  JSON), `Secret` redaction, error accumulation, and the
  `RIO.Config.Rotating` refreshable cell.
- [`docs/performance.md`](./docs/performance.md): benchmark
  baselines and dominant costs.

Migration guides for users coming from other ecosystems:

- [`docs/migrating-from-zio.md`](./docs/migrating-from-zio.md)
- [`docs/migrating-from-effect-ts.md`](./docs/migrating-from-effect-ts.md)

Worked examples:

- [`examples/logger/`](./examples/logger/): the smallest
  end-to-end demo. Defines a tiny `Logger` service, runs an
  `info` / `warn` / `err` program against it via
  `provideAll` + `runRIO`. A useful first read.
- [`examples/notify/`](./examples/notify/): exercises
  `rio-postgres`' `RIO.Postgres.Notify` end-to-end against the
  workspace's docker-compose Postgres: subscribes via
  `withListen`, fires `NOTIFY` payloads on the pool, and lets
  the scope finalizer drain both clients on exit.
- [`examples/otel-demo/`](./examples/otel-demo/): wires
  `RIO.Tracer.OTel.makeOTelTracer` into a real OpenTelemetry
  SDK with an in-memory exporter. Runs a nested-span workload
  through the same `RIO.Tracer` API and dumps the captured
  spans so the parent / child / attribute / status round-trip
  is visible.
- [`examples/todo-api/`](./examples/todo-api/): a small
  HTTPurple service demonstrating layers, typed failures, real
  Postgres persistence via `rio-postgres` (with a startup
  schema bootstrap through `rio-postgres-migrate`), middleware
  that stamps `request.id` / `request.method` / `request.path`
  on every emission, bearer-token auth, and JSON codec
  bridging.
- [`examples/worker-pool/`](./examples/worker-pool/): a
  producer + bounded queue + fan-out of N workers + per-job
  retry schedule + spans + metrics, with a `parTraverseCause`
  pre-flight that demonstrates `prettyCause`.
- [`examples/stream-pipeline/`](./examples/stream-pipeline/):
  three partition sources merged via `RIO.Stream.Par.mergeAll`
  and fanned out to two consumers via `broadcast`, with the
  metrics consumer aggregating per-source counts.
- [`examples/sink-analytics/`](./examples/sink-analytics/):
  one stream pass over an HTTP request log computes total,
  error, max-latency, distinct-path, and first-slow summaries
  via `RIO.Sink.zipPar`, showing how five small sinks compose
  into one lockstep consumer.
- [`examples/config-loader/`](./examples/config-loader/):
  loads typed configuration from a sample `.env` file via
  `rio-config-file`, showing the `Config` DSL end-to-end with
  `dotenvFileSource`, `withDefault`, `Secret` redaction, and a
  `prettyConfigError` failure path.

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

Pre-release. Nothing has been published to the PureScript
registry or Pursuit yet; the surface is being developed
on `main`. Treat any version string in `spago.yaml` as a
placeholder.

What's in `main` today: the production core (services, typed
errors, resource safety, layers, concurrency, virtual-time
testing), plus the larger surface listed above. The cause tree
(`RIO.Cause`) ships `parTraverseCause`, `raceCause`,
`acquireReleaseCause`, and `prettyCauseWithStack`. The stream
modules ship `RIO.Stream` (pull-based, single-channel),
`RIO.Stream.Par` (`mergeAll` / `broadcast` / `partition`),
`RIO.Stream.Resource` (`bracketStream`), `RIO.Sink`
(first-class composable consumers with `zipPar`), and
`RIO.Channel` (the unified producer / consumer / transducer
primitive that `Stream` and `Sink` specialise). `RIO.STM` (with
the `TVar` alias) and its derived structures, `RIO.Tracer` and
`RIO.Metrics` with an OpenTelemetry adapter (`rio-otel`),
`RIO.Random`, `RIO.Config` with `RIO.Config.Rotating` for
refreshable cells, `RIO.Schedule`, `RIO.Local`, `RIO.Logger`,
the qualified-do sugar (`RIO.Resource.Do`, `RIO.Concurrency.Par`),
the `rio-http` companion package (an HTTPurple adapter), the
`rio-postgres` adapter (wraps `purescript-postgresql` /
`node-postgres`), the `rio-config-file` adapter
(`dotenvFileSource`, `jsonFileSource`), and the `rio-node`
platform-adapter (Buffer, ChildProcess, EventEmitter,
FileSystem, HTTP, HTTP2, Net, OS, Path, Process, ReadLine,
Shutdown, Stream, URL).

What's open: a `rio-aws` integration package (scoped, additive
work that can land on its own schedule), real-Postgres CI
coverage for `rio-postgres` (currently builds against the
driver but has no integration tests; Docker-backed locally is
the intended setup), custom `Fail` instances for the worst row
/ variant error messages, and a property-testing harness tuned
for RIO. See [`FUTURE_WORK.md`](./FUTURE_WORK.md).

## License

MIT. See [`LICENSE`](./LICENSE).
