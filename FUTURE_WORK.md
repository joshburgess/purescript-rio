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

`RIO.Stream.Par` ships the parallel combinators:

- `mergeAll` / `merge` / `mergeMap` fan in N producers onto a
  shared bounded queue; first failure shuts everything down
- `broadcast` fans one upstream out to N consumer streams over
  per-consumer bounded queues (backpressure end-to-end)
- `partition` routes each upstream element to exactly one of N
  buckets via a key function

`RIO.Stream.Resource` adds `bracketStream`, a single-element
resource-acquiring stream whose release is registered with the
enclosing `scoped` block.

Still open:

- `Sink` / `Channel` style for composable, terminating consumers.
  `docs/sink-design.md` proposes a focused `Sink r e i a`
  layer (no full Channel algebra) with the primitives,
  combinators, `runSink` runner, and parallel-composition
  semantics laid out so implementation is mechanical. The
  recommended landing order is also documented there.

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
- `prettyCause` renders the tree; `prettyCauseWithStack` adds the
  JS stack underneath each `Die` leaf when one is available

`worker-pool` demonstrates `parTraverseCause` + `prettyCause` end
to end.

What's left here is mostly a design call rather than missing
plumbing: the existing `acquireRelease` / `race` keep their
non-Cause shapes on purpose so users only pay for cause-tree
construction when they explicitly ask for it. If that ever feels
wrong, the migration is mechanical: switch the implementations
over to the `*Cause` variants and surface the cause through a new
service row, the way ZIO and Effect-TS do.

### Config sources

The `rio-config-file` adapter ships:

- `dotenvFileSource` reads a `.env`-style file (with `export `
  prefixes, double- and single-quoted values, trailing
  comments, and 1-based parse-error line numbers)
- `jsonFileSource` reads a JSON file and flattens nested
  objects into `_`-joined keys, matching the way
  `RIO.Config.nested` qualifies keys; nulls drop, arrays are
  rejected with a path-aware shape error

Still open:

- Secrets-rotation hook for `Secret` values (refresh on a
  schedule or signal)

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

With the parallel + resource-safe stream extensions landed
(`mergeAll`, `merge`, `mergeMap`, `broadcast`, `partition`,
`bracketStream`) and the file-backed config sources shipped in
`rio-config-file` (`dotenvFileSource`, `jsonFileSource`), the
remaining single biggest demo gap against ZStream is a `Sink` /
`Channel` design. That work is large enough to warrant a focused
design pass: the wire-level shape, fusion story, and
parallel-sink combinators are all intertwined, and getting them
wrong is more costly than getting them late. Secrets rotation
for `Secret` values is the smaller remaining config gap.
