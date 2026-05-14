# Future Work: Gaps vs ZIO / Effect-TS

A focused inventory of what's still missing from `purescript-rio`
compared to ZIO and Effect-TS, oriented around "what would convince
a skeptical reader that this idea is real and usable," not "ship the
entire ecosystem."

## What we already have

Confirmed in `src/RIO/` and `PROJECT_BUILD_PLAN.md` as of the last
review:

- Core: `RIO r e a` newtype, env rows, error rows (Variant)
- Catch combinators: `catchTag`, `catchAll`, `mapError`, `sandbox`,
  `unsandbox`, `die`, `rethrow`
- Resource safety: `acquireRelease`, `Scope`, scoped layers
- Layers: `Layer rIn e rOut`, composition, `provideLayer`
- Concurrency primitives: `Concurrency.Par`, `Deferred`, parallel
  combinators, `Fiber`-style fork via `Aff`
- STM: `THub`, `TMap`, `TQueue`, `TSemaphore`
- Async (non-STM) primitives: `RIO.Queue`, `RIO.Hub`, `RIO.Semaphore`
- Schedule (retry / repeat) with combinator-style policy values
- Streams: `RIO.Stream` (pull-based, with map, filter, take, drop,
  concat, flatMap, mapM, unfoldM, repeatM, fold runners)
- Cause tree: `RIO.Cause` (parallel + sequential failure trees,
  `bothPar`, `prettyCause` renderer)
- Logger, Metrics, Tracer (with `Test.*` doubles for each)
- Clock + `TestClock`, Random + `TestRandom`, Config
- Spec integration helpers
- Adapters: `rio-http`, `rio-otel`, `rio-postgres` (with Notify,
  prepared statements, pool stats, migrate, json)
- Examples: `logger`, `notify`, `otel-demo`, `todo-api`,
  `worker-pool` (fan-out + Semaphore + Schedule retry + Metrics +
  Tracer)

That covers most of what an "effect system" needs to be more than a
toy. The gaps below are about what's still visibly missing to
someone comparing surface area against ZIO / Effect-TS.

## Shipped since the first pass of this document

The Tier 1 + Tier 2 items from the original review have all landed.
See `src/RIO/Stream.purs`, `src/RIO/Queue.purs`, `src/RIO/Hub.purs`,
`src/RIO/Semaphore.purs`, `src/RIO/Random.purs`,
`src/RIO/Test/Random.purs`, `src/RIO/Config.purs`,
`src/RIO/Cause.purs`, and `examples/worker-pool/`.

## Remaining gaps

The items below would be the natural next layer once the core
demonstration has had time to settle.

### Stream extensions

- `Sink` / `Channel` style for composable, terminating consumers
- Parallel stream combinators (`mergeAll`, `flatMapPar`)
- Backpressured fan-out (`broadcast`, `partition`)
- Resource-safe streams that thread `Scope` through their pull

The current `RIO.Stream` is intentionally pull-based and
single-channel. The above are the obvious follow-ups when the
demo workload outgrows the minimal surface.

### Cause integration

`RIO.Cause` now ships `Fail` / `Die` / `Parallel` / `Sequential`
plus the combinator set:

- `attemptCause` reifies any outcome as `Either (Cause e) a`
- `bothPar` collects two parallel outcomes
- `parTraverseCause` / `parSequenceCause` collect every failure
  from N parallel branches into a left-leaning `Parallel` tree
- `raceCause` waits for the first success and combines both
  failures into a `Parallel` cause when both sides fail
- `acquireReleaseCause` records `Sequential (body, release)` when
  the body and the finalizer both fail
- `prettyCause` renders the tree

`worker-pool` demonstrates `parTraverseCause` + `prettyCause` end
to end.

What it doesn't yet do:

- Have `RIO.Resource.acquireRelease` itself produce a `Cause` when
  its release path fails on top of a primary failure - today only
  the explicit `acquireReleaseCause` opts into that
- Render Aff stacktraces inside `Die` (today we only show
  `message`)
- Surface suppressed failures from `RIO.Concurrency.race`
  (`raceCause` waits for first-success-or-both-fail, but the
  short-circuit `race` still drops the loser)

### Config sources

- JSON file source (today only `envSource` + `mapSource` exist)
- `.env` file parser
- Secrets-rotation hook for `Secret` values

## Out of scope for the core demonstration

These are real features in ZIO / Effect-TS but adding any of them
would weaken the message rather than strengthen it. They're
non-goals for the "is this real" milestone.

- Full `ZStream` parity (sinks, parallel streams, transducers)
- Kafka / Redis / MongoDB adapters
- STM `orElse` and deeper transactional features
- A custom runtime / fiber supervisor beyond what `Aff` provides
- A web framework on top of `rio-http` (HTTPurple is enough for the
  examples)
- Cron / scheduled-job adapter (Schedule covers backoff; cron is a
  separate concern)
- Auto-derived persistent storage / ORM features

## Recommended priority order

With the original Tier 1 + Tier 2 list shipped, the highest-leverage
next step is probably Cause integration: making `race`, `bracket`,
and `parTraverse` produce `Cause`-shaped failures automatically so
the renderer earns its keep without users assembling causes by hand.
Stream extensions and richer config sources come after.
