# rio-fiber

A fiber runtime for `RIO` that does not sit on `Effect.Aff`.

The main `purescript-rio` package interprets `RIO r e a` on top of
`Aff`. That is a deliberate, sensible default, but it inherits
`Aff`'s ceiling: no fiber identity, no per-fiber state, no
structured supervision, no virtual time except by Clock discipline,
no `Cause` distinguishing failure from defect from interrupt, and
cancellation semantics shaped by `Effect.Aff`'s canceler protocol.

`rio-fiber` replaces the `Aff` interpreter with a hand-rolled step
loop and per-fiber state. The user-facing type is still
`RIO r e a`; what changes is the runtime underneath. In exchange
for that work you get proper fibers, FiberRef, Cause, a TestClock,
STM with event-loop atomicity, and the rest of the ZIO-shaped
surface, and the bind hot path is roughly 4x faster than `Aff` on
the workspace benchmarks; `fork x16 + join each` sits at parity
with `forkAff` once V8's JIT is warm (whichever variant the bench
runs second wins; the first pays the JIT compile cost).

This package is independent of the main `rio` package: it has its
own `RIO.Fiber.Core` module, its own combinators, its own
runtime. Programs are written against `RIO.Fiber.*` instead of
`RIO.*`. The two cannot share a program directly, but
[`RIO.Fiber.Aff`](./src/RIO/Fiber/Aff.purs) bridges either
direction at the boundary.

## A 30-second tour

```purescript
import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import RIO.Fiber.Aff (runAffThrow)
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Stream as Stream
import RIO.Fiber.Pipe as Pipe

doubled :: RIO () () (Array Int)
doubled = Stream.runCollect
  (Stream.via (Stream.fromArray [1, 2, 3, 4]) (Pipe.map (_ * 2)))

main :: Effect Unit
main = launchAff_ do
  xs <- runAffThrow doubled
  liftEffect (Console.logShow xs)
```

`RIO.Fiber.Core` re-exports the small set of primitives every
program needs: `pure`, `bind`, `liftEffect`, `ask`, `asks`,
`fail`, `catchAll`, `async`, `fork`, `forkInline`, `join`,
`interrupt`, `uninterruptible`, `bracket`, `ensuring`, `race`,
`raceAll`, `parTraverse`, `zipPar`, `validatePar`, `timeout`, and
the runners (`runRIO`, `runRIO'`, `runRIOCallback`).

`forkInline` is the opt-in alternative to `fork`: it drives the
child synchronously up to its first suspension (or completion)
before returning the handle. A sync-bodied child finishes inline
and a subsequent `join` resolves without touching the microtask
scheduler. The ordering trade-off is visible: with `fork` the
parent's next op observes the world before the child runs; with
`forkInline` the child runs first.

## What's here

- **`RIO.Fiber.Core`**: the entry point. `RIO r e a` plus the
  primitives above and three runners: `runRIO` (sync, returns
  `Either`), `runRIO'` (sync, fully discharged), and
  `runRIOCallback` (async, returns the full `Outcome`).
- **`RIO.Fiber.Aff`**: bridge to `purescript-aff`. `fromAff`
  lifts an `Aff` into `RIO` (Aff failures surface as defects;
  cancellation propagates either way). `runAff`, `runAffEither`,
  and `runAffThrow` run an `RIO` inside `Aff` at three projection
  levels: full `Outcome`, Aff-shaped `Either (Variant e) a`, or a
  bare value with defects raised on Aff's error channel.
- **`RIO.Fiber.Cause`**: the failure algebra. `Cause e` has five
  shapes (`Empty`, `Fail`, `Die`, `Interrupt`, `Then`, `Both`)
  and the introspection set (`firstFailure`, `firstDefect`,
  `interruptCount`, `stripInterrupts` / `stripFailures` /
  `stripDefects`, `mapFailures`, `flatten`, `squash`, `fold`,
  and `prettyPrint`).
- **`RIO.Fiber.Scope`**: resource scopes with LIFO finalizers.
  `scoped`, `acquireRelease`, `addFinalizer`, plus the structured-
  concurrency primitives `forkScoped` (child interrupted on scope
  close) and `forkSupervised` (auto-scoped to the nearest
  `supervised` block via an ambient `FiberRef`).
- **`RIO.Fiber.Ref`**: `FiberRef`, per-fiber state copied to
  children on fork. Backed by copy-on-write so an empty/unused ref
  table doesn't cost a Map allocation per fiber.
- **`RIO.Fiber.Deferred`**: one-shot write-once cell.
  `succeed` / `fail` / `await` / `poll`.
- **`RIO.Fiber.Semaphore`**, **`RIO.Fiber.Latch`**: counting
  semaphore (`withPermit`, `parTraverseN` for bounded-concurrency
  parallel traversal) and one-shot count-down latch.
- **`RIO.Fiber.Queue`**, **`RIO.Fiber.Hub`**: bounded /
  unbounded async queue, and a pub/sub hub.
- **`RIO.Fiber.STM`** plus `STM.TMVar`, `STM.TChan`, `STM.TQueue`,
  `STM.TArray`: software transactional memory. Single-event-loop
  atomicity (no version checks, no retry loops); `retry` /
  `orElse` / `check`.
- **`RIO.Fiber.Stream`**: pull-based stream. `fromArray`,
  `repeatRIO`, `fromQueue`, `fromTQueue`, `map` / `filter` /
  `take`, `flatMap`, `mapPar`, `chunked` / `unchunked` /
  `mapChunks`, `mapAccum`, `intersperse`, `scan`, `groupBy`,
  `merge`, `zipPar`, `buffer`, `throttle`, `debounce`,
  `timeoutPerPull`, `broadcast`, `share`, `catchAll`, `retry`,
  `acquireReleaseStream`, plus runners (`run`, `runCollect`,
  `fold`, `forEach`, `runSink`) and `via` for splicing pipes.
- **`RIO.Fiber.Sink`**: dual to Stream. Composable terminating
  consumers (`drain`, `head`, `last`, `count`, `sum`,
  `collectAll`, `foreach`, `fold`, `foldRIO`, `foldUntil`,
  `takeN`) with `map` and `contramap`. Runs via `Stream.runSink`.
- **`RIO.Fiber.Pipe`**: stream-to-stream transducers.
  `identity`, `map`, `filter`, `mapAccum`, `take`, `chunked`,
  plus `andThen` for end-to-end composition. Splice into a Stream
  with `Stream.via`.
- **`RIO.Fiber.Layer`**, **`RIO.Fiber.Pool`**: layered service
  composition (`>>>` and `<+>`) and a fixed-size object pool with
  `withPooled` bracketing.
- **`RIO.Fiber.Schedule`**: recursion policies (`recurs`,
  `spaced`, `exponential`, `jittered`, `intersect`, `andThen`,
  `whileInput`) with `repeat` / `retry` runners that sleep via
  the active `Clock`.
- **`RIO.Fiber.Clock`**, **`RIO.Fiber.TestClock`**: real wall
  clock and a virtual-time clock for deterministic tests.
  `TestClock.advance` wakes sleeping fibers when their deadline
  is crossed.
- **`RIO.Fiber.Tracer`**, **`RIO.Fiber.Metrics`**,
  **`RIO.Fiber.Logger`**: tracing / metrics / structured logging
  services with `withSpan`, counters / gauges / histograms, and
  scoped log annotations. Each ships a noop and at least one
  recording or live backend.
- **`RIO.Fiber.Supervisor`**: global supervisor registration so a
  monitor can observe every fiber start and end (used by the
  tracing test backend).
- **`RIO.Fiber.Random`**: a small randomness service with a
  seeded deterministic backend for tests.

## Choosing between `rio` and `rio-fiber`

| | `rio` (Aff-backed) | `rio-fiber` |
|---|---|---|
| Cancellation | `Aff` canceler protocol; interruption is cooperative | Structured: interrupt request observed at safe points; finalizers always run |
| Fiber identity | None | Numeric id, supervisor hooks, observe |
| Per-fiber state | Shared `RIO.Local` cells | `FiberRef` with copy-on-write to children |
| Failure model | Single `Variant e` or `Cause` reified at boundaries | First-class `Cause e` everywhere (Then/Both/Interrupt) |
| Virtual time | `RIO.Test.Clock` simulated via `Clock` discipline | `TestClock` wakes sleeping fibers directly |
| STM atomicity | One commit per event-loop turn (shared with `Aff` scheduling) | Same model, plus retry on `TVar` change without spinning |
| Bind hot path | About 33 ns per `bind` in the workspace bench | About 10 ns per `bind` (BIND fuses common leaf ops in the step loop) |
| Fork hot path | About the same as `forkAff` | At parity with `forkAff` on `fork x16 + join each` once V8 is warm; the bench variant that runs second wins by ~10% over the one that runs first because of JIT compile cost |

Use `rio` for code that already lives on `Aff` and only needs the
three-channel type. Use `rio-fiber` when you need any of the
columns the `Aff` runtime can't deliver: structured
cancellation, fiber identity, FiberRef, full `Cause` tree, or a
test clock that drives sleeping fibers without the harness having
to advance them by hand.

The two interop at the boundary via `RIO.Fiber.Aff`. A common
shape is a top-level `Aff` program that calls
`RIO.Fiber.Aff.runAffThrow` to run a fiber-backed subroutine.

## Going further

The per-module list above is a reference. For a guided tour of
how `Stream` / `Pipe` / `Sink` and `STM` actually compose into
pipelines, see [`docs/ecosystem.md`](./docs/ecosystem.md).

## Build

This package lives in the same workspace as `rio`. From the
repository root:

```sh
npx spago build -p rio-fiber
npx spago test -p rio-fiber
```

A head-to-head benchmark suite that exercises `rio-fiber`,
`Aff`-backed `RIO`, and raw `Aff` lives in `../benchmarks`:

```sh
npx spago run -p rio-benchmarks
```

## Status

Pre-release. The interpreter is stable, the surface in
`RIO.Fiber.*` is unlikely to shift in shape, and the test
suite covers each module end-to-end (269 tests at time of
writing). It has not been published to the PureScript registry.
