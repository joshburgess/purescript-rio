# rio-fiber

The default runtime for `purescript-rio`. A hand-rolled fiber
interpreter for `RIO r e a` that owns its scheduling, supervision,
and failure model end-to-end.

`RIO r e a` is the same three-channel type you'd see anywhere else
in the repository: typed environment row `r`, typed failure row
`e`, success value `a`. What `rio-fiber` adds is the runtime
underneath: numeric fiber identity, `FiberRef` copy-on-write
across fork, first-class `Cause e`, a `TestClock` that wakes
sleeping fibers, structured concurrency via `Scope` / `forkScoped`
/ `forkSupervised`, STM with `retry` parking on `TVar` change,
and an interruption model that distinguishes typed failure from
defect from interrupt at every node of the cause tree.

The bind hot path is roughly 4x faster than the `Aff`-backed
[`rio-aff`](../rio-aff/) on the workspace benchmarks (about 10 ns
per `bind` versus 33 ns); `fork x16 + join each` sits at parity
with `forkAff` once V8's JIT is warm; and the specialised
`forkAll x16 + joinAll` array fan-out runs at roughly 5x the
speed of `forkAff x16 + joinFiber` (it walks the array in a
single op dispatch instead of the per-element bind chain that
`traverse fork` builds).

Programs are written against `RIO.Fiber.*`.
[`RIO.Fiber.Aff`](./src/RIO/Fiber/Aff.purs) bridges to / from
`Effect.Aff` at the boundary, so a top-level `Aff` host program
can call into a fiber-backed subroutine, or vice versa, without
forcing the whole tree onto one runtime.

If you're starting a new project, this is the package to pick.
[`rio-aff`](../rio-aff/) is maintained alongside as the
ecosystem-friendly alternative for codebases that need to stay
on `Effect.Aff` end-to-end; the cost of that choice is documented
in [`docs/aff-constraints.md`](../docs/aff-constraints.md).

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
`fail`, `catchAll`, `async`, `fork`, `forkInline`, `forkAll`,
`join`, `joinAll`, `interrupt`, `uninterruptible`, `bracket`,
`ensuring`, `race`, `raceAll`, `parTraverse`, `zipPar`,
`validatePar`, `timeout`, and the runners (`runRIO`, `runRIO'`,
`runRIOCallback`).

`forkInline` is the opt-in alternative to `fork`: it drives the
child synchronously up to its first suspension (or completion)
before returning the handle. A sync-bodied child finishes inline
and a subsequent `join` resolves without touching the microtask
scheduler. The ordering trade-off is visible: with `fork` the
parent's next op observes the world before the child runs; with
`forkInline` the child runs first.

`forkAll` and `joinAll` are the array-shaped variants. They walk
the array in a single op dispatch (no per-element bind chain),
so a fan-out of N children pays a single specialized step
instead of the ~2N BIND nodes that `traverse fork xs` would
build. `joinAll` waits on every fiber and resumes with the
results in order; the first non-success outcome propagates and
sibling fibers are left alone. On the workspace benchmarks
`forkAll x16 + joinAll` runs at roughly 5x the speed of
`forkAff x16 + joinFiber`.

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
- **`RIO.Fiber.Cause`**: the failure algebra. `Cause e` has six
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
- **`RIO.Fiber.STM`** plus `STM.TArray`, `STM.TChan`,
  `STM.TDeferred`, `STM.TMap`, `STM.TMVar`, `STM.TPubSub`,
  `STM.TQueue`, `STM.TSemaphore`, `STM.TSet`: software
  transactional memory. Single-event-loop atomicity (no version
  checks, no retry loops); `retry` / `orElse` / `check`. `TPubSub`
  is the transactional pub/sub primitive (the aff package spells
  this `STM.THub`).
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
  `takeN`, `takeWhile`, `dropWhile`, `mkString`, `findRIO`) with
  `map` and `contramap`. Runs via `Stream.runSink`.
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

## Choosing between `rio-fiber` and `rio-aff`

| | `rio-fiber` (default) | `rio-aff` (Aff-backed alternative) |
|---|---|---|
| Cancellation | Structured: interrupt request observed at safe points; finalizers always run | `Aff` canceler protocol; interruption is cooperative |
| Fiber identity | Numeric id, supervisor hooks, observe | None |
| Per-fiber state | `FiberRef` with copy-on-write to children | Shared `RIO.Aff.Local` cells (process-global) |
| Failure model | First-class `Cause e` everywhere (Then / Both / Interrupt) | Single `Variant e` or `Cause` reified at boundaries |
| Virtual time | `TestClock` wakes sleeping fibers directly | `RIO.Aff.Test.Clock` simulated via `Clock` discipline |
| STM atomicity | One commit per event-loop turn, plus `retry` parks on `TVar` change without spinning | Same atomicity model, no `TVar`-park retry |
| Bind hot path | About 10 ns per `bind` (BIND fuses common leaf ops in the step loop) | About 33 ns per `bind` in the workspace bench |
| Fork hot path | At parity with `forkAff` on `fork x16 + join each` once V8 is warm | About the same as `forkAff` |
| Array fan-out | `forkAll` / `joinAll` are specialised ops; `forkAll x16 + joinAll` runs at roughly 5x the speed of `forkAff x16 + joinFiber` | `traverse forkAff` builds a per-element bind chain |

Pick `rio-fiber` unless you have a specific reason to stay on
`Aff`. Pick `rio-aff` when the host program is already on `Aff`,
when you want the smallest surface to learn first, or when you're
embedding into a framework whose handler shape is `Aff a` and you
want to discharge to it without a bridge call.

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
suite covers each module end-to-end (880+ tests at time of
writing). New design work in the repository targets
`rio-fiber` first; the matching surface is then ported to
`rio-aff` when it can be expressed on the `Aff` runtime.
Nothing has been published to the PureScript registry yet.
