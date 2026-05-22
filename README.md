# purescript-rio

A ZIO / Effect style effect type for PureScript: typed
environment, typed errors, resource safety, layers, and
structured concurrency.

```purescript
newtype RIO r e a = RIO (Op r e a)
```

- `r` is a **row of services** the computation needs.
- `e` is a **row of typed failures** it may raise.
- `a` is the **success value** on the happy path.

Both rows are open by default, so requirements aggregate
automatically on composition and shrink as services are
provided and failures are handled.

This repository ships **two runtimes** for the same
three-channel design. They share vocabulary, layer algebra,
service convention, and almost every combinator name. What
differs is how `RIO r e a` is interpreted at the bottom.

| | `rio-fiber` (default, recommended) | `rio-aff` (ecosystem alternative) |
|---|---|---|
| Runtime | Custom hand-rolled fiber interpreter | Sits on top of `Effect.Aff` |
| Module prefix | `RIO.Fiber.*` | `RIO.Aff.*` |
| Fiber identity | Numeric id with supervisor hooks | None |
| Per-fiber state | `FiberRef` with copy-on-write to children | Shared `Effect.Ref` cells |
| Failure model | First-class `Cause e` everywhere | Single `Variant e` (or `Cause` reified at boundaries) |
| Virtual time | `TestClock` wakes sleeping fibers directly | Discipline-based via `Clock` service |
| STM semantics | Event-loop atomicity plus `retry` parks on `TVar` change | Same atomicity model, no `TVar`-park retry |
| Bind hot path | ~10 ns per `bind` (BIND fuses common leaf ops) | ~33 ns per `bind` |
| Array fan-out | `forkAll x16 + joinAll` runs ~5x faster than `forkAff x16 + joinFiber` | Same as `Aff` |

`rio-fiber` is the default because it owns the runtime end-to-end
and unlocks behaviour the `Aff` foundation structurally cannot
give us (fiber identity, FiberRef, full `Cause` tree, a TestClock
that actually wakes parked fibers, and structured concurrency in
the proper sense). `rio-aff` exists for codebases that want the
three-channel design without leaving the `Effect.Aff` ecosystem,
and for the interop story: an `Aff` program can call into a
`rio-fiber` subroutine at the boundary via
[`RIO.Fiber.Aff`](./rio-fiber/src/RIO/Fiber/Aff.purs).

The "why" for each runtime is spelled out in
[Why rio-fiber is the default](#why-rio-fiber-is-the-default) and
[When `rio-aff` is the right choice](#when-rio-aff-is-the-right-choice)
below; the precise cost analysis for the `Aff` ceiling is in
[`docs/aff-constraints.md`](./docs/aff-constraints.md).

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

## A 30-second tour (rio-fiber)

```purescript
import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console as Console
import RIO.Fiber.Aff (runAff)
import RIO.Fiber.Core (RIO, asks, liftEffect)

type Logger = { log :: String -> Effect Unit }

greet
  :: forall e
   . String
  -> RIO (logger :: Logger) e Unit
greet name = do
  log <- asks _.logger.log
  liftEffect (log ("hello, " <> name))

main :: Effect Unit
main = launchAff_ do
  let consoleLogger = { log: Console.log }
  _ <- runAff (greet "world") { logger: consoleLogger }
  pure unit
```

A handler that may fail picks up its tag automatically:

```purescript
import RIO.Fiber.Core (RIO, asks, fail, liftEffect)
import Data.Maybe (Maybe(..))
import Data.Variant as Variant
import Type.Proxy (Proxy(..))

notFound :: Proxy "notFound"
notFound = Proxy

findTodo
  :: Int
  -> RIO (store :: TodoStore) (notFound :: { id :: Int }) Todo
findTodo tid = do
  store <- asks _.store
  row <- liftEffect (store.get tid)
  case row of
    Nothing -> fail (Variant.inj notFound { id: tid })
    Just todo -> pure todo
```

The `notFound` tag flows up through callers until a
`catchTag (Proxy :: _ "notFound")` removes it from the row,
or a runner surfaces it on the `Outcome` value the entry
point inspects.

## Why `rio-fiber` is the default

These are not "we haven't written them yet" features: each one
is either impossible to add on top of `Aff` without rewriting
its interpreter, or has been tried and found to be unworkable
inside `Aff`'s canceler protocol.

- **First-class `Cause e`.** Every failure carries a structured
  cause tree (`Empty`, `Fail`, `Die`, `Interrupt`, `Then`, `Both`)
  that distinguishes typed failures from defects from interrupts,
  and composes sequential / parallel error trees losslessly.
  `Aff` collapses everything into one error channel; you cannot
  ask "was this failure also accompanied by an interrupt?" or
  "did both branches of a `race` fail?".
- **Numeric fiber identity and supervisor hooks.** Every fiber
  has a stable id, and the runtime exposes a registration point
  so an observer can see every fiber start, end, and link.
  Tracing, metrics, and structured logging are built on this.
  `Aff` has no fiber id at all; you cannot correlate a span to
  a fiber.
- **`FiberRef` with copy-on-write fork semantics.** Per-fiber
  state that is copied to children on `fork` and shared until
  either side mutates. This is what makes scoped log
  annotations, span context inheritance, and trace propagation
  work without threading them through every signature. The
  `Aff`-backed equivalent (`Effect.Ref` cells) is process-global;
  forked fibers share it whether you wanted that or not.
- **Structured concurrency via `Scope`.** `forkScoped` ties a
  child's lifetime to a scope; `forkSupervised` ties it to the
  nearest `supervised` block via an ambient `FiberRef`. When the
  scope closes (success, failure, or interrupt), every child is
  interrupted and every finalizer runs in LIFO order. `Aff`
  forks are detached by default and there is no protocol to
  attach them to a parent's lifetime.
- **A `TestClock` that actually drives sleeping fibers.**
  `TestClock.advance n` finds every fiber that registered a
  sleep with a deadline `<= now + n` and resumes it. Deterministic
  simulation of timers, retries, debounces, and schedules drops
  out for free. An `Aff`-style simulated clock cannot wake a
  fiber that is parked inside `Aff.delay`; the harness has to
  fake the parked state itself.
- **STM with event-loop atomicity *and* `TVar`-park retry.**
  `TVar` / `TMVar` / `TChan` / `TQueue` / `TArray` with
  `atomically`, `retry`, `orElse`, `check`. Commits are atomic
  across an entire event-loop turn, so a transaction never
  observes another fiber's half-write. `retry` parks the fiber
  on the set of `TVar`s it read and resumes only when one of
  them changes; there is no version check, no spinning, and no
  per-`TVar` lock.
- **Bounded-concurrency parallel traversal.** `parTraverseN n`
  via `Semaphore`, plus `Hub`, `Queue`, `Latch`, `Deferred`,
  `Layer`, and `Pool` as proper first-class concurrent
  primitives instead of ad-hoc `MVar` recipes.
- **`Stream` / `Sink` / `Pipe` with structured concurrency.**
  Pull-based streams with `mapPar`, `merge`, `broadcast`,
  `share`, `throttle`, `debounce`, `timeoutPerPull`, plus
  `Sink` and `Pipe` as composable terminals and transducers.
  Each pulls under a real `Scope` so leaked producers are
  interrupted automatically.
- **`Schedule`-driven `repeat` / `retry`.** `recurs`, `spaced`,
  `exponential`, `jittered`, `intersect`, `andThen`,
  `whileInput`, all driven by the active `Clock` so the same
  policy runs against real time in production and virtual time
  in tests.
- **Tracing / metrics / logging with span context propagation.**
  `withSpan`, counter / gauge / histogram metrics, scoped log
  annotations. Each ships a noop and at least one recording or
  live backend, and child fibers inherit the active span by
  virtue of `FiberRef` copy-on-write.
- **An interruption model that distinguishes "asked to stop"
  from "in the middle of stopping" from "finalizer running".**
  `uninterruptible`, `ensuring`, and `bracket` compose at the
  Cause layer, so a finalizer that itself fails is visible in
  the resulting `Cause.Then`. `Aff`'s canceler protocol cannot
  represent a finalizer-failed-while-cleaning-up scenario.

On the workspace benchmarks the bind hot path is roughly 4x
faster than `Aff`; `fork x16 + join each` is at parity with
`forkAff` once V8's JIT is warm; and the specialised
`forkAll x16 + joinAll` array fan-out runs at roughly 5x the
speed of `forkAff x16 + joinFiber` (it walks the array in a
single op dispatch instead of the per-element bind chain that
`traverse fork` builds). See
[`rio-fiber/README.md`](./rio-fiber/README.md) for the full
surface and the side-by-side comparison table.

## When `rio-aff` is the right choice

`rio-aff` is a complete implementation of the same three-channel
design that delegates the runtime to `Effect.Aff`. It is the
right pick when:

- **You're already on `Aff`.** Existing code, libraries you
  depend on, and frameworks you target are written against
  `Aff`. Picking the `Aff`-backed runtime keeps your effect
  graph homogeneous: no boundary conversions, no `runAffThrow`
  bridges, no two-runtime composition story.
- **You want the smallest possible surface to learn first.**
  `rio-aff` is roughly half the module count of `rio-fiber` and
  shares its vocabulary with the `purescript-aff` mental model
  many users already have.
- **You're embedding into a host that hands you `Aff`.** Some
  PureScript HTTP servers, GraphQL frameworks, and SDKs accept
  an `Aff a` handler. `rio-aff` lets you write that handler in
  the three-channel style and discharge to `Aff` at the call
  boundary with `runRIO` / `runRIO'`.

What you give up by staying on `Aff` is documented in detail in
[`docs/aff-constraints.md`](./docs/aff-constraints.md); the
short version is the column on the right of the table at the
top of this README. Specifically: no fiber identity, no
per-fiber state, no full `Cause` tree, no test clock that wakes
sleeping fibers, no first-class structured concurrency, and a
~3x bind hot path overhead relative to `rio-fiber`.

Note that `rio-aff` still ships its own `Cause` reifier
(`attemptCause`, `parTraverseCause`, `prettyCauseWithStack`) and
its own simulated `Clock` (`RIO.Aff.Test.Clock`), so most of the
ZIO-shaped vocabulary survives the trip. But `Cause` exists at
the boundaries, not as the native failure carrier the way it
does in `rio-fiber`.

The two runtimes interop at the boundary via
[`RIO.Fiber.Aff`](./rio-fiber/src/RIO/Fiber/Aff.purs). A common
production shape is a top-level `Aff` program that calls
`RIO.Fiber.Aff.runAffThrow` to run a fiber-backed subroutine,
keeping the host on `Aff` while taking advantage of the fiber
runtime inside.

## Install

`purescript-rio` is not yet published to the PureScript registry. To try it
out, clone the repository and use it as a local workspace package,
or point your `spago.yaml` at the git remote directly. Pick
`rio-fiber` for new code; pick `rio-aff` only if one of the
considerations in the previous section applies.

## What's included

### `rio-fiber` (recommended)

Programs are written against `RIO.Fiber.*`.

- **`RIO.Fiber.Core`**: the entry point. `RIO r e a` plus the
  primitives every program needs (`pure`, `bind`, `liftEffect`,
  `ask`, `asks`, `fail`, `catchAll`, `async`, `fork`,
  `forkInline`, `forkAll`, `join`, `joinAll`, `interrupt`,
  `uninterruptible`, `bracket`, `ensuring`, `race`, `raceAll`,
  `parTraverse`, `zipPar`, `validatePar`, `timeout`) and three
  runners (`runRIO`, `runRIO'`, `runRIOCallback`).
- **`RIO.Fiber.Aff`**: bridge to `purescript-aff`. `fromAff` lifts
  an `Aff` into `RIO`; `runAff`, `runAffEither`, and
  `runAffThrow` run an `RIO` inside `Aff` at three projection
  levels.
- **`RIO.Fiber.Cause`**: the failure algebra and its
  introspection set (`firstFailure`, `firstDefect`,
  `interruptCount`, `stripInterrupts`, `mapFailures`, `flatten`,
  `squash`, `fold`, `prettyPrint`).
- **`RIO.Fiber.Scope`**: resource scopes with LIFO finalizers,
  plus the structured-concurrency primitives `forkScoped` and
  `forkSupervised`.
- **`RIO.Fiber.Ref`**: `FiberRef` with copy-on-write fork
  semantics, plus `RIO.Fiber.Ref.Synchronized` for atomic
  effectful updates over an `Effect.Ref`.
- **`RIO.Fiber.Deferred`**: one-shot write-once cell.
- **`RIO.Fiber.Semaphore`**, **`RIO.Fiber.Latch`**: counting
  semaphore with `withPermit` and `parTraverseN`, and a one-shot
  count-down latch.
- **`RIO.Fiber.Queue`**, **`RIO.Fiber.Hub`**: async queue and
  pub/sub hub.
- **`RIO.Fiber.STM`** plus `STM.TArray`, `STM.TChan`,
  `STM.TDeferred`, `STM.TMap`, `STM.TMVar`, `STM.TPubSub`,
  `STM.TQueue`, `STM.TSemaphore`, `STM.TSet`: software
  transactional memory with `retry` / `orElse` / `check`.
  `TPubSub` is the transactional pub/sub primitive (`STM.THub`
  on the aff side).
- **`RIO.Fiber.Stream`**, **`RIO.Fiber.Sink`**,
  **`RIO.Fiber.Pipe`**: pull-based stream, composable
  terminating consumers, and stream-to-stream transducers.
- **`RIO.Fiber.Layer`**, **`RIO.Fiber.Pool`**: layered service
  composition and a fixed-size object pool.
- **`RIO.Fiber.Schedule`**: recursion policies driven by the
  active `Clock`.
- **`RIO.Fiber.Clock`**, **`RIO.Fiber.TestClock`**: real wall
  clock and virtual-time clock that wakes sleeping fibers.
- **`RIO.Fiber.Tracer`**, **`RIO.Fiber.Metrics`**,
  **`RIO.Fiber.Logger`**: observability services with span
  context propagation, counter / gauge / histogram metrics, and
  scoped log annotations.
- **`RIO.Fiber.Config`** plus `Config.Rotating` for refreshable
  cells.
- **`RIO.Fiber.Memo`**, **`RIO.Fiber.Cache`**,
  **`RIO.Fiber.RateLimiter`**, **`RIO.Fiber.CircuitBreaker`**,
  **`RIO.Fiber.WorkerPool`**: higher-level resilience primitives
  built on the fiber runtime.
- **`RIO.Fiber.Supervisor`**: global supervisor registration for
  observability.
- **`RIO.Fiber.Random`**: randomness service with a seeded
  deterministic backend.

### `rio-aff` (ecosystem alternative)

Programs are written against `RIO.Aff.*`.

- `RIO.Aff.Core`: the type, runners, and the most common
  re-exports.
- `RIO.Aff.Env`: `ask` / `asks` to read services, `provide` /
  `provideAll` to inject them.
- `RIO.Aff.Error`: `fail`, `catchTag`, `catchAll`, `mapError`,
  plus `die` / `sandbox` / `unsandbox` for the defect channel.
- `RIO.Aff.Cause`: parallel + sequential failure trees,
  `attemptCause`, `parTraverseCause`, `raceCause`,
  `acquireReleaseCause`, `prettyCause` / `prettyCauseWithStack`.
  Reified at boundaries rather than carried natively.
- `RIO.Aff.Resource`: `acquireRelease`, `ensuring`, and `Scope`
  with LIFO finalizers.
- `RIO.Aff.Layer`: `Layer rIn e rOut` with sequential (`>>>`)
  and horizontal (`<+>`) composition.
- `RIO.Aff.Concurrency`: `fork`, `forkScoped`, `join`,
  `interrupt`, `uninterruptible`, `timeout`, `parTraverse`,
  `parTraverseN`, `parSequence`, `zipPar`, `race`, `raceAll`.
  Cancellation is cooperative (`Aff`-style).
- `RIO.Aff.Deferred`, `RIO.Aff.Semaphore`, `RIO.Aff.Queue`,
  `RIO.Aff.Hub`: async coordination primitives over
  `Effect.Aff.AVar` and `Effect.Ref`.
- `RIO.Aff.Clock`, `RIO.Aff.Random`: the service shapes plus
  live and seeded backends.
- `RIO.Aff.Config` plus `RIO.Aff.Config.Rotating`: the Config
  DSL and a refreshable cell.
- `RIO.Aff.Schedule`: pure scheduling policies that sleep
  through `Clock` so the virtual-time test clock can drive them.
- `RIO.Aff.STM` plus `STM.TArray`, `STM.TChan`, `STM.TDeferred`,
  `STM.THub`, `STM.TMap`, `STM.TMVar`, `STM.TQueue`,
  `STM.TSemaphore`, `STM.TSet`: software-transactional memory
  derived from single-event-loop atomicity.
- `RIO.Aff.Stream`, `RIO.Aff.Stream.Par`,
  `RIO.Aff.Stream.Concurrent`, `RIO.Aff.Stream.Resource`,
  `RIO.Aff.Stream.Timed`, `RIO.Aff.Sink`, `RIO.Aff.Channel`:
  pull-based effectful streams, parallel stream combinators,
  composable sinks, and the unified Channel primitive.
- `RIO.Aff.Tracer`, `RIO.Aff.Metrics`, `RIO.Aff.Logger`:
  observability services (each with noop, live, and recording
  backends).
- `RIO.Aff.Local`: ambient state with scoped overrides. The
  `RIO.Aff` analogue of ZIO `FiberRef`, but process-global
  rather than per-fiber.
- `RIO.Aff.Spec`, `RIO.Aff.Test.*`: `itRIO` / `itRIO_` adapters
  for `purescript-spec`, plus recording test backends.

## Documentation

Foundational reading:

- [`docs/aff-constraints.md`](./docs/aff-constraints.md): the
  precise cost analysis for `rio-aff`'s `Effect.Aff` foundation.
  What it gives us for free, what it cannot give us, and why
  `rio-fiber` exists. Read this before adopting `rio-aff` for
  anything load-bearing.

Walkthrough docs (the shared three-channel design; examples
mostly use `RIO.Aff.*` because the docs predate `rio-fiber`,
but the concepts apply to both):

- [`docs/01-core-type.md`](./docs/01-core-type.md): the three
  parameters, how the channels compose.
- [`docs/02-services.md`](./docs/02-services.md): service
  convention and idiomatic provision.
- [`docs/03-errors.md`](./docs/03-errors.md): typed failures,
  catching, and the defect channel.
- [`docs/04-layers.md`](./docs/04-layers.md): the `Layer` type,
  vertical (`>>>`) and horizontal (`<+>`) composition.
- [`docs/05-resources.md`](./docs/05-resources.md):
  `acquireRelease`, `ensuring`, `Scope` / `scoped`.
- [`docs/06-concurrency.md`](./docs/06-concurrency.md): fork,
  race, parallel traversal, cancellation caveats.
- [`docs/07-testing.md`](./docs/07-testing.md): the spec
  adapters and the virtual-time clock.
- [`docs/08-scheduling.md`](./docs/08-scheduling.md): retry and
  repeat policies, combinators, and how to drive them
  deterministically in tests.
- [`docs/09-stm.md`](./docs/09-stm.md): STM primitives,
  atomicity on the JS event loop, `retry` / `orElse`.
- [`docs/10-tracing.md`](./docs/10-tracing.md): tracing and
  metrics services.
- [`docs/11-fiber-local.md`](./docs/11-fiber-local.md): ambient
  state, scoped overrides, and the fork-inheritance trade-off
  relative to ZIO `FiberRef`.
- [`docs/12-logging.md`](./docs/12-logging.md): structured
  logging.
- [`docs/13-streams.md`](./docs/13-streams.md): pull-based
  streams.
- [`docs/14-causes.md`](./docs/14-causes.md): the `Cause`
  algebra and the cause-collecting combinators.
- [`docs/15-config.md`](./docs/15-config.md): the `Config`
  descriptor type, the `Source` adapter set, `Secret` redaction.
- [`docs/performance.md`](./docs/performance.md): benchmark
  baselines and dominant costs.

`rio-fiber`-specific tour:

- [`rio-fiber/README.md`](./rio-fiber/README.md): the full
  fiber-runtime surface, the bridge to `Aff`, and the
  side-by-side comparison table.
- [`rio-fiber/docs/ecosystem.md`](./rio-fiber/docs/ecosystem.md):
  guided tour of how `Stream` / `Pipe` / `Sink` and `STM`
  compose into pipelines.

Migration guides for users coming from other ecosystems:

- [`docs/migrating-from-zio.md`](./docs/migrating-from-zio.md)
- [`docs/migrating-from-effect.md`](./docs/migrating-from-effect.md)

Worked examples (each example targets one of the two runtimes;
the package's `spago.yaml` makes the choice explicit):

- [`examples/logger/`](./examples/logger/): the smallest
  end-to-end demo. A tiny `Logger` service run with
  `provideAll` + `runRIO`.
- [`examples/notify/`](./examples/notify/): exercises
  `RIO.Aff.Postgres.Notify` end-to-end against the workspace's
  docker-compose Postgres: subscribes via `withListen`, fires
  `NOTIFY` payloads on the pool, and lets the scope finalizer
  drain both clients on exit.
- [`examples/otel-demo/`](./examples/otel-demo/): wires
  `RIO.Aff.Tracer.OTel.Adapter.makeOTelTracer` into a real
  OpenTelemetry SDK with an in-memory exporter.
- [`examples/todo-api/`](./examples/todo-api/): a small
  HTTPurple service demonstrating layers, typed failures, real
  Postgres persistence, middleware that stamps `request.id` /
  `request.method` / `request.path` on every emission,
  bearer-token auth, and JSON codec bridging.
- [`examples/worker-pool/`](./examples/worker-pool/): a
  producer + bounded queue + fan-out of N workers + per-job
  retry schedule + spans + metrics.
- [`examples/stream-pipeline/`](./examples/stream-pipeline/):
  three partition sources merged via `mergeAll` and fanned out
  to two consumers via `broadcast`.
- [`examples/sink-analytics/`](./examples/sink-analytics/):
  one stream pass over an HTTP request log computes five
  summaries via `Sink.zipPar`.
- [`examples/config-loader/`](./examples/config-loader/): loads
  typed configuration from a sample `.env` file, showing the
  `Config` DSL end-to-end with `Secret` redaction.
- [`examples/batch-import/`](./examples/batch-import/): a
  row-typed pipeline exercising `memoize`, `validatePar`,
  `orElse`, `option`, and `foldRIO`.
- [`examples/typed-error-workflow/`](./examples/typed-error-workflow/):
  a flaky `userApi` survived via retries, a `CircuitBreaker`
  that fail-fasts after a streak, and `catchTag` routing.
- [`examples/showcase/`](./examples/showcase/): wires `Schema`,
  `Logger.withFields`, `Tracer`, a `mockSql` store, and
  `HttpStream.fromChunks` into an in-process HTTP application.
- [`examples/production-app/`](./examples/production-app/):
  the canonical long-running daemon wire-up: `Layer` for the
  service graph, `Runtime` for the resolved environment,
  `withSpan` per heartbeat, and `withShutdown` racing the
  worker against `SIGINT` / `SIGTERM`.

## Build

```sh
npm install
npx spago build
npx spago test
```

Build or test a single package:

```sh
npx spago build -p rio-fiber
npx spago test -p rio-fiber
npx spago test -p rio-aff
```

Run the example:

```sh
npx spago run -p rio-example-todo-api
```

Run the benchmark suite (compares `rio-fiber`, `rio-aff`, and
raw `Aff` head-to-head):

```sh
npx spago run -p rio-benchmarks
```

## Status

Pre-release. Nothing has been published to the PureScript
registry or Pursuit yet; the surface is being developed on
`main`. Treat any version string in any `spago.yaml` as a
placeholder.

`rio-fiber` is the active development target: its
interpreter is stable, its surface in `RIO.Fiber.*` is
unlikely to shift in shape, and the test suite covers each
module end-to-end (880+ tests at time of writing). `rio-aff`
is maintained as the ecosystem-friendly alternative and
receives bug fixes plus selective forward-ports of new
surface, but new design work happens in `rio-fiber` first.

Companion adapter packages (`rio-fiber-node`, `rio-fiber-http`,
`rio-fiber-otel`, `rio-fiber-config-file`, `rio-fiber-postgres`,
`rio-fiber-postgres-json`, `rio-fiber-postgres-migrate`) sit on
top of `rio-fiber`; the matching `rio-aff-*` packages do the
same for `rio-aff`. See [`FUTURE_WORK.md`](./FUTURE_WORK.md)
for the open work list.

## License

MIT. See [`LICENSE`](./LICENSE).
