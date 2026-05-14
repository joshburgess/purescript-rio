# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
once it reaches `1.0.0`. While in the `0.x` series, minor releases may include
breaking changes (see `PROJECT_BUILD_PLAN.md`, "Versioning Policy").

## [Unreleased]

### Added

- Worked-example cross-links added to three more numbered
  reference docs: `docs/05-resources.md` now points at
  `examples/notify/` and `examples/todo-api/` for resource-safe
  service shutdown; `docs/06-concurrency.md` points at
  `examples/worker-pool/` for the fan-out / `parTraverseCause` /
  `forkScoped` / `Deferred` mix; and `docs/08-scheduling.md`
  points at `examples/worker-pool/` for the
  `intersect (recurs n) (exponential base 2.0)` retry shape.
- CI now exercises every adapter test suite and every example
  runtime. The build matrix runs `npx spago test` for
  `rio-config-file`, `rio-http`, and `rio-otel`; builds
  `rio-postgres-json` and `rio-postgres-migrate`; and runs
  `worker-pool`, `stream-pipeline`, `sink-analytics`, and
  `config-loader` as smoke checks. The postgres integration job
  also drives `rio-postgres-json` and `rio-postgres-migrate`
  against the service container, not just `rio-postgres`. The
  format-check step was updated to include `rio-config-file`,
  `rio-postgres-json`, and `rio-postgres-migrate`, matching the
  refreshed `npm run format:check` scripts.
- `rio-otel`: test suite for `RIO.Tracer.OTel`. Pins the
  adapter's own bookkeeping (sequential `SpanId` allocation
  starting at 1, `currentSpan` reporting the latest open span
  as parent, stack pop on `endSpan` restoring the parent,
  `endSpan` of an unknown or already-closed span as a no-op,
  `endSpan` of a non-current span filtering it out of the
  stack, `addAttribute` safety against unknown ids, and
  per-tracer counter isolation). Runs against the no-op tracer
  that `@opentelemetry/api` returns when no SDK is registered;
  the end-to-end SDK round-trip lives in `examples/otel-demo/`.
  `npx spago test -p rio-otel` now reports `12/12 tests
  passed`.
- `rio-http`: test suite for `RIO.HTTPurple.Auth`,
  `RIO.HTTPurple.Request`, and `RIO.HTTPurple.Middleware`.
  Covers `bearerAuthConfig` prefix shape, `requireAuth`
  success / missing-header / mismatched-header / case-
  insensitive-header / scheme-required paths,
  `defaultRequestIdHeader`, `mkRequestContext`'s header-
  honouring, monotonic-fallback, custom-header-name, and
  counter-isolation behaviours, and `withRequestContext`'s
  log-line pair, `request.id` / `request.method` /
  `request.path` annotation stamping, `requestId` `Local`
  set/restore, and `duration_ms` annotation on completion
  (driven by `RIO.Test.Logger` and `RIO.Test.Clock`). Fills
  the previously empty test stanza on the package; `npx spago
  test -p rio-http` now reports `20/20 tests passed`.
- Numbered reference docs filled in for every substantive module:
  `docs/04-layers.md` (`Layer` construction, composition,
  `provideLayer`, resource-safe layers), `docs/05-resources.md`
  (`acquireRelease`, `ensuring`, `Scope` / `scoped`,
  `RIO.Resource.Do`), `docs/14-causes.md` (the `Cause` algebra
  and cause-collecting combinators), and `docs/15-config.md`
  (the `Config` DSL, sources, `Secret`, `Rotating`). The
  numbered doc set now runs 01-15 contiguously and covers every
  module under `src/RIO/`. The `forthcoming` pointers in
  `docs/07-testing.md` and both migration guides are resolved.
- `docs/sink-design.md` rewritten as design notes (why the
  shape, why single-fiber `zipPar`, why no Channel) now that
  `RIO.Sink` has shipped.
- `examples/config-loader/`: loads typed configuration from a
  sample `.env` file via `rio-config-file`. Exercises
  `dotenvFileSource`, the `Config` DSL (with `withDefault` and
  `secret`), `Secret` redaction at print time, and the
  failure path through `prettyConfigError`. First example
  that uses `rio-config-file` end-to-end.
- `examples/sink-analytics/`: a single-pass analytics demo over
  a synthetic HTTP request log. Composes five small sinks
  (`count`, `filterIn isError count`, `mapInput _.latencyMs`
  over a max-fold, a path-set fold, and `find` for the first
  slow request) with `zipPar` and runs the result against
  `fromArray`. One stream pass produces the full summary.
- `RIO.Sink`: first-class composable terminating consumers for
  `RIO.Stream`. `Sink r e i a` consumes some prefix of `i`s and
  produces an `a`. The shape is `Need k finish | Halt a` so
  short-circuiting sinks (`take`, `find`, `any`, `all`) finalise
  cleanly against infinite streams. Ships `drain`, `head`,
  `last`, `count`, `collect`, `foldL`, `foldM`, `take`, `find`,
  `any`, `all`, `mapResult`, `mapInput`, `filterIn`, `andThen`,
  `zipPar`, `zipParWith`, and `runSink`. `zipPar` runs two
  sinks in lockstep against the same stream on one fiber;
  early-halt of one side is remembered while the other
  continues. See `docs/13-streams.md` and `docs/sink-design.md`.
- `RIO.Config.Rotating`: a refreshable cell for values that
  can change at runtime (typically a rotating `Secret`).
  `newRotating` allocates a cell with an initial value;
  `readRotating` / `writeRotating` are atomic read / write
  primitives. `withRotation` bundles a loader: it runs the
  loader once to populate the cell and returns a `refresh`
  action that re-runs the loader and overwrites the cell. The
  module imposes no rotation policy; polling, signal handling,
  or other triggers are left to the caller.
- `rio-config-file`: new adapter package providing
  `dotenvFileSource` and `jsonFileSource`. Both read a file
  from disk and return a `RIO.Config.Source` ready to feed
  into `load`. `parseDotenv` and `flattenJson` are exposed as
  pure helpers for tests and in-memory callers. The JSON
  flattener joins nested object keys with `_`, matching the
  way `RIO.Config.nested` qualifies keys, so the same
  `Config` descriptor works against env, dotenv, and JSON
  sources without modification.
- `RIO.Stream.Par`: parallel stream combinators. `mergeAll`
  fans in N producer streams onto a shared bounded queue (one
  fiber per producer; first observed failure shuts the queue
  down and is propagated on the consumer's next pull). `merge`
  is the two-stream convenience. `mergeMap` materialises the
  outer stream then merges every inner stream concurrently.
  `broadcast` fans one upstream out to N consumer streams over
  per-consumer bounded queues with end-to-end backpressure.
  `partition` routes each upstream element to one of N buckets
  via a key function (mod N, normalised for negative keys).
  All four share the same failure model: first failure wins;
  sibling producers exit naturally when they find the queue
  closed.
- `RIO.Stream.Resource`: `bracketStream` is a single-element
  resource-acquiring stream whose release is registered with
  the enclosing `scoped` block. Compose with `flatMap` to
  thread the acquired resource through a multi-element
  downstream. Release runs on every termination path; if
  acquire fails, no finalizer is registered.
- `RIO.Stream` now exports `Stream(..)` and `unStream` so
  companion modules (e.g. `RIO.Stream.Par`,
  `RIO.Stream.Resource`) can step the underlying `RIO` when
  building new combinators. End-user code should still reach
  for the combinator surface; the constructor is exposed for
  library extension only.

- `RIO.Resource.Do`: qualified-do sugar over
  `RIO.Resource.acquireRelease`. Each `<-` inside a
  `Resource.do` block desugars to an `acquireRelease`, with the
  block's continuation becoming the `use` callback. Flattens
  what would otherwise be nested brackets when one computation
  opens several resources. Release ordering matches
  `acquireRelease`: LIFO on every termination path (success,
  typed failure, defect, kill). Plain `RIO` statements
  interleaved between acquisitions wrap with
  `Resource.liftRIO`. Verified against hand-nested
  `acquireRelease` (identical event ordering) and against
  typed-failure / failed-acquire paths in `Test.RIO.Resource.Do`.
- `RIO.Concurrency.Par`: qualified-`ado` sugar for running
  independent `RIO` actions concurrently and combining their
  results under `Control.Parallel`'s `ParAff`. Use with
  `Par.ado`, not `Par.do` (qualified-do for parallel
  composition would still sequence). Failure bias: leftmost
  typed failure wins, but every branch is allowed to run to
  completion. For short-circuiting fan-out (cancel the loser on
  first failure), keep using `RIO.Concurrency.parPair` /
  `parTuple`. Verified at 3x speedup vs sequential `do` in
  `Test.RIO.Concurrency.Par`.

- `rio-http` workspace package: extracts the reusable HTTP
  pieces of the todo-api example into a standalone adapter so
  apps that pair `rio` with [HTTPurple](https://pursuit.purescript.org/packages/purescript-httpurple)
  can pick them up without copying.
  - `RIO.HTTPurple.Request` exposes a `RequestContext` record
    (method, path, requestId, headers) plus `mkRequestContext`,
    `newRequestCounter`, and `defaultRequestIdHeader` for
    snapshotting an HTTPurple `Request` into a flat shape free of
    the route type variable. Honours an inbound
    `X-Request-Id` header when present; falls back to a
    monotonic `req-N` allocated from a per-process counter.
  - `RIO.HTTPurple.Middleware.withRequestContext` wraps any
    `RIO` action so every emitted log line carries
    `request.id` / `request.method` / `request.path`, writes
    the request id into a `Local String` for downstream
    correlation, and emits a `request received` /
    `request completed` (or `request failed`) pair around the
    body with elapsed milliseconds and a success / failure
    verdict. The wrapped action's error row is preserved.
  - `RIO.HTTPurple.Auth.requireAuth` is a polymorphic typed
    failure: takes a `Proxy sym` and a payload supplied by the
    caller so each consuming app can choose its own tag
    (`unauthorized`, `forbidden`, ...) and payload on its
    own error row. `bearerAuthConfig` builds a config whose
    `expected` field is `"Bearer " <> token`.
- CI builds `rio-http` on every PR and the
  `purs-tidy` format check now covers the `rio-http/` source tree.

- `rio-postgres` workspace package: an adapter wrapping
  [`purescript-postgresql`](https://pursuit.purescript.org/packages/purescript-postgresql)
  (the `node-postgres` / `pg` driver) so apps can talk to
  Postgres through a row-typed `Postgres` service. Each
  combinator surfaces driver failures on a caller-chosen typed
  tag carrying `PgError` (a thin newtype around the library's
  `NonEmptyArray Error`), keeping the row layout up to the
  consuming app.
  - `RIO.Postgres` exposes the `Postgres` service token,
    `PgError` plus `pgErrorMessage`, and a small set of smart
    constructors: `withClient` brackets a client from the pool
    for a callback (release on every termination path),
    `query` / `exec` run a one-shot statement on a fresh
    client, and their `*Using` variants thread an existing
    client for in-transaction chaining. `withTransaction`
    wraps a block in `BEGIN` / `COMMIT`, rolling back on any
    typed failure on the chosen tag and re-raising it.
    Re-exports `Pool`, `Client`, `AsQuery`, `FromRows`, and
    the underlying `Error` shape so consumers don't need to
    depend on the driver package directly.
  - `RIO.Postgres.Layer.postgresLayer` builds a fresh pool
    from a `node-postgres` config record and registers the
    pool's `Pool.end` shutdown as a finalizer on the
    surrounding scope, so pool drain is guaranteed on every
    exit path of the scope the layer is built into.
- CI builds `rio-postgres` on every PR and the `purs-tidy`
  format check covers the `rio-postgres/` source tree.

### Changed

- `examples/todo-api/Middleware.purs` is now a thin app-shim
  over `rio-http`. It re-exports `RequestContext` /
  `AuthConfig` / `withRequestContext` verbatim and pre-applies
  `requireAuth` against the example's `unauthorized` typed
  failure so call sites stay unchanged.

### Research

- `spikes/qualified-do/`: explores PureScript's qualified-do as
  ergonomic sugar over `RIO` patterns. Two candidates earn their
  keep: `Resource.do` flattens nested `acquireRelease` blocks
  (verified to produce identical LIFO release ordering against
  the hand-nested form), and `Par.ado` runs each `<-` line
  concurrently under `Control.Parallel` (clocked at 102ms vs
  302ms for three 100ms branches). The findings call out one
  ergonomic gotcha (plain `RIO` lines inside `Resource.do` need
  an explicit `liftRIO`) and the things qualified-do
  fundamentally cannot do (type-directed implicit `Proxy` at the
  `<-` site, implicit `atomically` lifts that mix `STM` and
  `RIO`, generator-style `yield` / `await`). See
  `spikes/qualified-do/FINDINGS.md`. CI builds and runs the
  spike on every PR. The two winning candidates are scoped for
  inclusion in the main package under
  `RIO.Resource.Do` / `RIO.Concurrency.Par` once the surface is
  reviewed.

### Changed

- `examples/todo-api/`: migrated to `RIO.Logger` for structured
  logging and demonstrates two `RIO`-native middleware
  combinators built on the new logging / local services. A new
  `Example.TodoApi.Middleware` module ships
  `withRequestContext`, which opens a per-request
  `withFields` block stamping `request.id` /
  `request.method` / `request.path` on every emitted line
  (including domain log lines), writes the request id into a
  `Local String` so downstream code can correlate without
  threading an argument, and emits a `request received` /
  `request completed` (or `request failed`) pair around the
  body with elapsed milliseconds and a success / failure
  verdict; `requireAuth` is a bearer-token check that raises
  the `unauthorized` typed failure on the existing error row.
  The handlers stay domain-focused: logging correlation,
  per-request id propagation, and auth are layered on by the
  middleware in `Main.purs`, not threaded through every call.
  Inbound `X-Request-Id` headers are honoured for trace
  correlation; otherwise the server assigns a monotonic
  `req-N`. The example's `ApiError` row picks up an
  `unauthorized :: Unit` tag handled in `renderApiError`
  alongside the existing `notFound` case. The walkthrough in
  `examples/todo-api/README.md` is updated with the new
  smoke-test curls (auth path, `X-Request-Id`, JSON 400,
  method 405) and a sample of the resulting structured log
  output.

### Added

- `spikes/phase-9-review/`: randomised stress harness covering
  the recently-added modules. Eight scenarios, one invariant
  each: `RIO.Logger` nests `withFields` up to eight levels
  deep under random typed failures and fork/join, then asserts
  the annotation set is empty after the program returns;
  `RIO.Local` does the same shape on a `Local Int` with an
  added kill path that interrupts a forked child mid-flight;
  `RIO.STM.TQueue` runs up to four producers in parallel
  against up to four forked consumers and asserts count and
  sum match across the queue; `RIO.STM.THub` covers all four
  back-pressure strategies (Unbounded fan-out with multiple
  subscribers; Bounded back-pressure with a forked publisher
  forced to retry while a single consumer drains; Sliding
  drop-oldest with a non-draining subscriber checking the last
  `buffer` values survive in order; Dropping drop-new with a
  non-draining subscriber checking the first `buffer` values
  survive and overflowing publishes return `false`);
  `RIO.STM.TSemaphore` exercises `withTSemaphore` with random
  typed failures and mid-hold fiber kills and asserts every
  permit is returned. 250 iterations per scenario per
  invocation (2000 total). Across four consecutive local runs
  (8000 total iterations) the harness reports zero invariant
  violations. See `spikes/phase-9-review/FINDINGS.md`. CI
  builds and runs the spike on every PR.
- `RIO.STM.THub` module: transactional publish/subscribe hub.
  Each published value fans out to every active subscriber's
  private buffer; subscribers consume independently. Four
  back-pressure strategies chosen at construction time:
  `newBoundedTHub n` (producer retries while any subscriber is
  full), `newSlidingTHub n` (drops oldest on full, never
  blocks), `newDroppingTHub n` (drops new on full, never
  blocks, returns `false` when any subscriber dropped),
  `newUnboundedTHub` (never blocks, never drops, susceptible to
  memory growth on slow consumers). Subscribers register with
  `subscribeTHub` and consume with `takeSubscription` /
  `tryTakeSubscription`; `unsubscribeTHub` removes a
  subscription and drops its buffered values. Prefer
  `withSubscription` for the common case: it brackets
  subscribe/unsubscribe against an `RIO` action so the
  subscription is released on every termination path. A new
  subscriber sees only values published after it registers.
  See `docs/09-stm.md`.
- `RIO.Logger` module: structured logging service. `Logger` is a
  record of `log` / `getAnnotations` / `setAnnotations`
  operations carried in the environment row at the `logger`
  field. Five levels: `LogTrace`, `LogDebug`, `LogInfo`,
  `LogWarn`, `LogError` (no `LogFatal`; unrecoverable failures
  belong on the defect channel). Smart constructors `logTrace`
  / `logDebug` / `logInfo` / `logWarn` / `logError` emit at
  each level. `withField key value action` and `withFields
  fields action` scope a batch of `(key, value)` annotations to
  a block; the previous annotation set is restored by
  `Aff.finally` on every termination path. Annotation merging
  shadows existing keys with their inner replacements and
  preserves attach order so backends can render fields in input
  order. Backends shipped: `noopLogger` (discards emissions,
  retains annotation scoping), `consoleLogger` (writes
  `[LEVEL] message  k1=v1, k2=v2` lines to
  `Effect.Console.log`), and `RIO.Test.Logger.newRecordingLogger`
  (in-memory recorder for tests). Annotations are stored in a
  shared `Ref`; see `docs/12-logging.md` for the documented
  fork-inheritance trade-off (the same one `RIO.Tracer` and
  `RIO.Local` make).
- `RIO.Local` module: ambient state with scoped overrides.
  `Local a` is a typed cell created by `newLocal` (or
  `newLocalEffect` for callers building their environment
  outside an `RIO` action) with `get` / `set` / `update`
  operations and a `locally fl value action` combinator that
  scopes a value to a block. The restore is guaranteed by
  `Aff.finally` on every termination path (success, typed
  failure, defect, interrupt). Backed by `Effect.Ref`, so a
  forked child fiber observes the parent's current value and a
  child's writes are visible to the parent; this is the same
  implicit-context model `RIO.Tracer` uses. See
  `docs/11-fiber-local.md` for use cases, the comparison to
  ZIO `FiberRef`, and the documented fork-inheritance
  trade-off.

### Added

- `RIO.Concurrency.timeout :: Milliseconds -> RIO r e a -> RIO r e
  (Maybe a)`. Race an action against a deadline; on timeout the
  action is interrupted and `Nothing` is returned. Typed failures
  from the action propagate unchanged.
- `RIO.Concurrency.parTraverseN :: Int -> (a -> RIO r e b) -> Array
  a -> RIO r e (Array b)`. Bounded-concurrency traversal that
  chunks the input array into `n`-sized groups and `parTraverse`s
  each chunk in turn.
- `RIO.Concurrency.uninterruptible :: RIO r e a -> RIO r e a`. Wrap
  a critical section so kills are queued until the inner action
  completes. Sits on top of `Aff.invincible`.
- `RIO.Concurrency.forkScoped :: Scope -> RIO r e a -> RIO r e'
  (Fiber e a)`. Fork into a scope: when the scope exits the fiber
  is interrupted as part of its LIFO finalizer pass. The
  structured-concurrency counterpart of `fork`.
- `RIO.Resource.ensuring :: RIO r e a -> RIO r () Unit -> RIO r e
  a`. `finally`-style finalizer guarantor without the
  acquire/release split of `acquireRelease`.
- `RIO.Deferred` module: one-shot write-once cell over
  `Effect.Aff.AVar` for fiber handshakes. `makeDeferred`,
  `succeedDeferred`, `failDeferred`, `awaitDeferred`,
  `pollDeferred`.
- `RIO.Layer.passthrough :: Union rOut rIn rPassed => Layer rIn e
  rOut -> Layer rIn e rPassed`. Extend a layer's output row with
  the labels it required as input, so downstream consumers see
  both. Closes DX-1.
- `RIO.Schedule` module: pure scheduling policies for retry and
  repeat. `Schedule r i o` with `recurs`, `spaced`, `exponential`,
  `forever`, `once`; combinators `andThen`, `intersect`,
  `whileInput`, `jittered`, `mapSchedule`; runners `repeat`,
  `retry`, `retryOrElse` that sleep via the `Clock` service so a
  virtual-time test clock can drive scheduled programs
  deterministically. `step` exposes one decision for tests that
  sample a schedule's delay distribution. The error row is fixed
  to `()`; schedules cannot themselves fail with a typed error.
  See `docs/08-scheduling.md`.
- `Benchmarks.Gate`: developer-runnable performance regression
  gate. Runs the same scenarios as `Benchmarks.Main`, compares
  each one's mean wall-clock per iteration against a baseline
  picked by profile (`RIO_GATE_PROFILE` env var, default
  `local-m1-pro`; `ci-ubuntu-latest` is the in-repo CI profile),
  prints a one-row table per scenario plus a single-line
  `BASELINE_JSON` blob of observed means for capture, and exits
  non-zero if any scenario's mean is more than 3x its baseline.
  Threshold is deliberately generous to tolerate
  machine-to-machine variance. Scenarios with no baseline in the
  active profile are reported as `n/a` and do not contribute to
  the regression count. The CI workflow now runs the gate on the
  `node 20` matrix leg in informational mode
  (`continue-on-error: true`) so the `BASELINE_JSON` line can be
  mined to populate the `ci-ubuntu-latest` profile; the gate
  becomes required by flipping that one flag once the baseline
  has been captured. See `docs/performance.md` for the full
  capture procedure.
- `random` to the main `rio` package's dependency manifest
  (used by `RIO.Schedule.jittered`; was previously available
  transitively through the test stack only).
- `RIO.STM` module: software-transactional memory. `TRef a`,
  `STM e a` (with `Functor` / `Apply` / `Bind` / `Monad`
  instances), `newTRef`, `readTRef`, `writeTRef`, `modifyTRef`,
  `retry`, `check`, `orElse`, `failSTM`, and `atomically`. The
  implementation uses the JS event loop's lack of preemption
  directly: an `STM` body is a synchronous `Effect` whose
  intermediate writes are unobservable to other fibers, so commit
  needs neither version checks nor pessimistic locks. `retry`
  suspends until any read `TRef` is written, via waiter callbacks
  fired from the writer's commit phase. Typed failures abort the
  transaction (no writes apply) and surface on the parent's row.
  See `docs/09-stm.md`.
- `RIO.STM.TQueue`: unbounded FIFO queue built on a single
  `TRef (Array a)`. Surface: `newTQueue`, `writeTQueue`,
  `readTQueue` (retries when empty), `tryReadTQueue`,
  `peekTQueue`, `isEmptyTQueue`, `lengthTQueue`. Underlying
  enqueue/dequeue are `Array.snoc` / `Array.uncons` (O(n) on the
  JS backend); the API leaves room for a deque-based replacement.
- `RIO.STM.TMap`: transactional map keyed by an `Ord` type,
  backed by a single `TRef (Map k v)`. Surface: `newTMap`,
  `insertTMap`, `lookupTMap`, `deleteTMap`, `memberTMap`,
  `sizeTMap`, and `awaitKey` (retries until a key is present).
  Wakeups are not key-indexed; any write to the underlying TRef
  re-checks the predicate, which suits "wait on handler
  registration" patterns and is fine for low-churn maps.
- `RIO.STM.TSemaphore`: counting semaphore on a single
  `TRef Int`. Surface: `newTSemaphore`, `acquireTSemaphore`,
  `acquireN`, `releaseTSemaphore`, `releaseN`,
  `availableTSemaphore`, and `withTSemaphore`, which brackets
  an acquire/release pair around an `RIO` action via
  `acquireRelease` so the permit is returned on every
  termination path.
- `ordered-collections` to the main `rio` package's dependency
  manifest (used by `RIO.STM.TMap`).
- `RIO.Tracer` module: tracing service with named spans, status
  recording (`SpanOk` / `SpanFailed` / `SpanInterrupted`), and
  string attributes. `withSpan` brackets an action: opens a span
  as a child of the currently-active span, runs the action,
  closes the span with the appropriate status on every
  termination path (success, typed failure, fiber kill).
  `addAttribute` attaches a key/value pair to the currently
  active span; `currentSpan` reports it. `noopTracer` discards
  every operation. Parent context is implicit and survives the
  common "fork inside a span" case in the JS single-event-loop
  model; see `docs/10-tracing.md` for the explicit caveats.
- `RIO.Test.Tracer` module: `newRecordingTracer` returns a
  `Tracer` plus a `snapshot` action that returns the recorded
  spans in start order. Virtual time advances by 1 per
  `startSpan` / `endSpan`, making span ordering deterministic in
  tests.
- `RIO.Metrics` module: counter / gauge / histogram service
  shape with `recordCounter`, `recordGauge`, `recordHistogram`
  and call-site-readable aliases (`incrementCounter`,
  `setGauge`, `observeHistogram`). `noopMetrics` discards every
  emission.
- `RIO.Test.Metrics` module: `newRecordingMetrics` returns the
  service plus a `snapshot` action returning every emission
  with its kind (`Counter` / `Gauge` / `Histogram`), name, and
  value.
- `rio-otel` package (`rio-otel/`): OpenTelemetry adapter for
  `RIO.Tracer`. `RIO.Tracer.OTel.makeOTelTracer name` returns a
  `Tracer` record that forwards every span lifecycle, attribute
  write, and parent / child relationship to an
  `@opentelemetry/api` tracer; call sites that use `withSpan`,
  `addAttribute`, or `currentSpan` keep working verbatim. Status
  maps `SpanOk -> OK`, `SpanFailed -> ERROR`,
  `SpanInterrupted -> ERROR` with message `"interrupted"`. With
  no OTel SDK registered the adapter is silent (the global API
  returns a no-op tracer). An end-to-end demo wiring the adapter
  to `BasicTracerProvider` + `InMemorySpanExporter` lives at
  `examples/otel-demo/`.

### Changed

- `parTraverse` and `parSequence` now short-circuit on the first
  typed failure, cancelling sibling fibers. The earlier
  implementation ran every branch to completion before surfacing
  the first `Left`. The new
  behaviour matches ZIO `foreachPar` / Effect-TS `forEach` with
  `concurrency: "unbounded"` and is implemented by throwing a
  sentinel defect from the failing branch (caught by `Aff.attempt`
  at the boundary) plus a shared `Ref` for the first-failure value.
  Successful programs are unaffected; programs that depended on
  the old "run to completion" semantics will see siblings
  interrupted instead of completing.
- `zipPar` short-circuits similarly: the first `Left` from either
  branch cancels the other.
- `raceAll` is now implemented in terms of
  `Control.Parallel.parOneOfMap` rather than a left-fold of
  pairwise `race`. The behaviour is the same (first to complete
  wins; losers are interrupted), but every branch is started in
  parallel rather than racing pairwise.

## Earlier work (build-plan phases 0–8)

Nothing below has been released. These entries describe the
phase-by-phase work that landed on `main` against the original
build plan, covering the production core: typed environment row,
typed error row, resource-safe bracket and scope primitives,
layer composition, structural concurrency with cancellation,
virtual-time testing, and adapters for `purescript-spec`. See
`docs/` for the walkthroughs, and `docs/migrating-from-zio.md` /
`docs/migrating-from-effect-ts.md` for idiom-by-idiom mappings.

### Release-prep work (not actually released)

- Version string set to `0.1.0` in `spago.yaml` as a placeholder.
- README rewritten as a landing page: 30-second tour, install
  note, module-by-module surface, links to walkthrough docs and
  the worked example, build and run instructions.

### Added

- Initial project scaffold (Phase 0.1).
- `RIO.Internal` module defining the `RIO r e a` newtype, with the
  data constructor available for in-library use only (Phase 1.1).
- `RIO.Core` module exposing `RIO` as an opaque type plus `runRIO`,
  `runRIO'`, and `unsafeRunRIO` (Phase 1.1).
- `Functor`, `Apply`, `Applicative`, `Bind`, and `Monad` instances
  for `RIO r e`, with sampled law checks in the test suite (Phase 1.2).
- `MonadEffect` and `MonadAff` instances for `RIO r e` (Phase 1.3).
- `RIO.Error` module with `fail` for raising typed failures, re-exported
  from `RIO.Core` (Phase 1.3).
- `docs/01-core-type.md`: walkthrough of the three type parameters and a
  comparison with ZIO and Effect-TS (Phase 1.4).
- `RIO.Env` module with `ask` and `asks` for reading services out of the
  environment row (Phase 2.1).
- `provide` in `RIO.Env`: single-service injection that shrinks the
  required row by one field. The `Lacks` constraint from the original
  draft is dropped, per the Phase 0.4 spike's LE-1 finding; the internal
  insertion uses `Record.Unsafe.unsafeSet`, which is safe under the
  `Cons` relation (Phase 2.2).
- `provideAll` in `RIO.Env`: full-environment injection that produces a
  `RIO () e a` runnable directly via `runRIO` or `runRIO'` (Phase 2.3).
- `examples/logger/`: a complete `Logger` service plus a runnable
  example demonstrating the idiomatic service shape (record of
  `Aff`-valued operations + smart constructors lifting into `RIO`)
  (Phase 2.4).
- `docs/02-services.md`: the service convention, including two traps to
  avoid (polymorphic operation fields, and using `asks` to project an
  operation function) (Phase 2.4).
- Row-inference regression test asserting that a do-block with two
  disjoint `ask`s infers a row covering both services with the
  environment-row variable kept open (Phase 2.5).
- `RIO.Test` module with `mockService` (a more readable alias for
  `provide`) and `recording` (a small helper for capturing service-call
  histories into a `Ref` for test assertions) (Phase 2.6).
- `compile-fail/` test driver and the first negative case: providing a
  service whose value type doesn't match the required service. CI now
  runs the driver alongside the regular test suite.
- `spikes/phase-2-review/`: Phase 2 review cycle. Ten realistic service
  compositions written against the production `RIO.Core` API with no
  user-supplied type signatures; `FINDINGS.md` reproduces each inferred
  type verbatim. Confirms LE-1 (the `Lacks` leak from the Phase 0.4
  spike) is gone in the production API and surfaces no new regressions.
  CI builds the spike on every PR.
- `catchTag` in `RIO.Error`: catch one named failure tag and remove it
  from the error row, with the handler free to introduce new tags
  (Phase 3.1).
- `catchAll` and `mapError` in `RIO.Error`: replace the error row in
  bulk via an effectful handler or a pure translation respectively;
  `rethrow` as the identity handler for selective passthrough inside
  `catchAll` (Phase 3.2).
- `die`, `sandbox`, `unsandbox` in `RIO.Error`: distinguish typed
  failures (in the row) from defects (`Aff` exceptions); `sandbox`
  reifies defects into the success channel as `Either Error a`
  without absorbing typed failures (Phase 3.3).
- `docs/03-errors.md`: walked-through example narrowing a three-tag
  error row down to `()`, with the compiler's actual inferred type
  quoted at each step from
  `spikes/phase-2-review/src/Spike/ErrorsDocFixture.purs` (Phase 3.4).
- Phase 3 review cycle: two new compile-fail cases (`runRIO'` with a
  leftover error tag; `catchTag` with a wrong payload type) plus
  `compile-fail/FINDINGS.md` rating the readability of each compiler
  message and listing candidates for custom `Fail` instances.
- `RIO.Resource` module with `acquireRelease`: bracket-style primitive
  that guarantees the release action runs on every termination path of
  the use phase (success, typed failure, defect, or external fiber
  kill). The release path has an empty error row by construction; if
  acquisition itself fails, release is not invoked. Builds directly on
  `Effect.Aff.bracket`, whose release phase is uninterruptible by
  default (Phase 0.5 spike, scenario S6) (Phase 4.1).
- `Scope`, `addFinalizer`, and `scoped` in `RIO.Resource`: introduce a
  scope under the `scope` service label, push `Aff` finalizers onto its
  stack, and run them LIFO on exit on every termination path. A
  finalizer that throws does not stop subsequent finalizers from
  running; exceptions are swallowed for now so a single leak cannot
  cascade. Aggregating finalizer errors is deferred to a later phase
  (Phase 4.2).
- Phase 4 review cycle: `spikes/phase-4-review/` opens 1000 nested
  scopes per iteration, picks a random depth and termination mode
  (success, typed failure, defect), and asserts the resulting event
  log shows every `register-k` matched by a `finalize-k` in LIFO
  order. A second scenario forks the program and injects a random
  `killFiber` during an innermost `Aff.delay` and applies the same
  check. 100 iterations per invocation, replayed in CI. Across four
  consecutive local runs (400 total iterations) the harness reports
  zero leaks and zero LIFO violations. Findings live in
  `spikes/phase-4-review/FINDINGS.md`.
- `RIO.Layer` module with the `Layer rIn e rOut` newtype, `fromRecord`
  (lift a fixed record), `fromRIO` (build a record from an `RIO` that
  can `ask` for inputs, lift `Aff`, and register finalizers via the
  `scope` service), and `buildLayer` (a closing runner intended for
  test layers that do not own resources) (Phase 5.1).
- `andThen` and `combine` in `RIO.Layer`, with operator aliases
  `(>>>)` (`infixr 1`) for sequential composition and `(<+>)`
  (`infixr 7`) for horizontal composition. `(>>>)` shadows
  `Control.Semigroupoid.(>>>)`; `RIO.Core` re-exports only the named
  forms so `import Prelude` keeps the standard `(>>>)` accessible.
  `combine` requires `Prim.Row.Union` on the output rows; output rows
  with overlapping labels are rejected by the compiler (Phase 5.2).
- `provideLayer` in `RIO.Layer`: build a layer and run an inner
  program in the layer's services, unioning layer and program error
  rows via `Prim.Row.Union e e' eOut`. A single scope spans both the
  layer build and the program run, so finalizers registered by the
  layer release after the program completes on every termination
  path: success, typed failure, and defect (Phases 5.3 and 5.4). The
  forward error-row expansion uses `Data.Variant.expand` against the
  supplied `Union`; the program-side expansion uses `unsafeCoerce`
  because PureScript's row solver can't recover the symmetric
  `Union e' e eOut` instance from the user-supplied one. The cast is
  safe at runtime: `expand` itself is `unsafeCoerce`, and the
  constraint already proves every label of `e'` is in `eOut`.
- `Scope` constructor exported from `RIO.Resource` for in-library
  use by `RIO.Layer.provideLayer`. `RIO.Core` continues to re-export
  only the opaque type, so the public surface is unchanged.
- `spikes/phase-5-review/`: Phase 5 review cycle. A six-service
  layered application (`Config`, `Logger`, `Clock`, `Cache`,
  `Database`, `UserService`) split across three layers, including
  a failing layer (`dbConnect` when `databaseUrl` is empty) and a
  resourceful layer (registers `cache-flush` and `db-close`
  finalizers). Three scenarios assert exact event sequences:
  happy path, layer-level failure, and program-level typed failure
  after service use. All three pass. `FINDINGS.md` records one DX
  issue worth tracking: the lack of a passthrough operator for
  sequential composition (DX-1, candidate for a later phase). CI
  builds and runs the spike on every PR.
- `RIO.Concurrency` module with `Fiber e a`, `fork`, `join`, and
  `interrupt` (Phase 6.1). `fork` and `interrupt` are infallible
  from the caller's perspective and leave the caller's error row
  free (instead of pinning it to `()`) so they compose cleanly
  inside a do-block whose surrounding row is non-empty: this is the
  one departure from the build plan's literal signature, made
  because the `()` form forces the entire surrounding do-block to
  have `()` for its error row. `Fiber e a` wraps an
  `Effect.Aff.Fiber (Either (Variant e) a)`; typed failures from
  inside a fiber surface on `join` as `Left v` on the joiner's
  row, defects (including the kill exception from `interrupt`)
  propagate as `Aff` exceptions and are observable via
  `RIO.Error.sandbox`. The cancellation guarantees come from the
  Phase 0.5 spike scenarios S1 / S3 / S4.
- `parTraverse`, `parSequence`, and `zipPar` in `RIO.Concurrency`
  (Phase 6.2). Layered on `Effect.Aff`'s `ParAff` applicative via
  `Control.Parallel.parTraverse` and `Effect.Aff.parallel /
  sequential`. Failure semantics: all branches run to completion
  and the first `Left` (in array order, or favouring the left side
  for `zipPar`) is surfaced; first-failure racing semantics are
  reserved for `race` in Phase 6.3. Timing tests confirm two 100ms
  actions complete in ~100ms rather than ~200ms. `parallel`,
  `arrays`, `datetime`, `integers`, `newtype`, and `now` added to
  the package's dependency manifest.
- `RIO.Clock` module with the `Clock` service (`now :: Aff
  Milliseconds`, `sleep :: Milliseconds -> Aff Unit`), smart
  constructors `now` and `sleep` that read the service from the
  environment row, and `liveClock` backed by `Effect.Now` and
  `Effect.Aff.delay`. The service operations are `Aff`-valued
  following the `docs/02-services.md` convention; the
  cancellation guarantees of `liveClock.sleep` come from
  `Aff.delay` and match the Phase 0.5 spike's S1 scenario
  (Phase 7.1). `datetime` and `now` added to the package's
  dependency manifest.
- `RIO.Test.Clock` module with `newTestClock`: allocates a
  virtual `Clock` whose `now` and `sleep` are driven by an
  explicit `advance :: Milliseconds -> Aff Unit` controller.
  Pending sleepers wake in deadline order within a single
  `advance` call; an interrupted fiber's sleeper is removed
  from the pending list by its canceler and does not fire on
  later advances (Phase 7.1).
- `RIO.Spec` module with `itRIO`, `itRIO_`, and `runSpecRIO`:
  `purescript-spec` integration helpers so an `RIO` program
  slots directly into a `Spec` suite without per-test
  boilerplate. `itRIO` runs a fully-handled `RIO () () Unit`
  via `runRIO'`; `itRIO_` accepts a service record and
  `provideAll`s it before running; `runSpecRIO` pre-installs
  the console reporter and exits the process with the suite's
  result (Phase 7.2). `spec` and `spec-node` added to the
  package's dependency manifest. The trade-off (transitive
  spec dep for every consumer) is intentional for now; we may
  factor `RIO.Spec` out into a sibling workspace package later.
- `docs/07-testing.md`: end-to-end walkthrough of the testing
  story, covering `mockService` and `recording` (from Phase
  2.6), the `Clock` service plus `newTestClock` (Phase 7.1),
  `purescript-spec` integration via `itRIO` / `itRIO_` /
  `runSpecRIO` (Phase 7.2), how to structure tests for layered
  programs (referencing `spikes/phase-5-review/`), and what is
  intentionally absent from Phase 7 (generator-based property
  tests, snapshot testing, per-fiber isolation) (Phase 7.3).
- `spikes/phase-7-review/`: Phase 7 review cycle. Ports the
  Phase 5 review's six-service layered application to a
  `Test.Spec` suite that uses only `RIO.Spec`, `RIO.Test`, and
  `RIO.Test.Clock`. Four scenarios: A. happy path with
  `recording` + `newTestClock`; B. failing layer (`dataLayer`
  raises `dbConnect`); C. program failure after service use
  (typed `progBoom`); D. time-sensitive forks parked on
  `clock.sleep` resumed by `advance` in deadline order.
  Replaces Phase 5's hand-rolled `ScenarioResult` harness with
  ordinary `it` / `itRIO_` bodies and `shouldEqual` assertions.
  All four scenarios pass; CI builds and runs the spike on
  every PR. `FINDINGS.md` records three DX observations:
  `recording` is `Aff Unit`-only (no `recordingWith`); `itRIO_`
  requires `e ~ ()`, so typed-failure inspection falls back to
  plain `it` + `runRIO`; forking a service-using program inside
  a spec body costs an inner `runRIO` because `Aff.forkAff`
  works in `Aff`, not in `RIO`.
- `examples/todo-api/`: Phase 8.1 tutorial example. A small
  HTTP service built on HTTPurple 4.0 plus `rio`. Six modules:
  service interfaces (`Services.purs`), production layer
  wiring (`Layers.purs`) with an in-memory `Ref`-backed store,
  domain handlers (`Handlers.purs`) expressed as `RIO`
  programs over a three-service environment (`logger`,
  `todoStore`, `clock`) with a single typed failure
  (`notFound`), JSON codecs (`Codecs.purs`) bridging
  `argonaut-codecs` to HTTPurple's `JsonEncoder` /
  `JsonDecoder`, route definitions (`Routes.purs`) via
  `Routing.Duplex`, and a bridging `Main.purs` that builds the
  layer once at startup and runs each request via `runRIO`.
  Four endpoints (GET `/todos`, GET `/todos/:id`, POST
  `/todos`, DELETE `/todos/:id`) with HTTP semantics
  (200/204/400/404/405) verified against `curl` end-to-end.
  Persistence is in-memory only; the SQLite-backed variant
  called for in the original build plan is deferred since the
  layer-swap story is already demonstrated by the Phase 7
  review and the example does not need a second driver to
  show off RIO. CI builds the example on every PR.
  `argonaut-codecs`, `argonaut-core`, `httpurple`, and
  `integers` join the example package's dependency manifest;
  none are added to the main `rio` package's dependencies.
- `docs/migrating-from-zio.md` and
  `docs/migrating-from-effect-ts.md`: Phase 8.2 migration
  guides. Each maps idioms 1:1 with code snippets in both
  languages, covering the core type, lifting values,
  composition, services, providing services, typed errors,
  resource safety, concurrency, layers, and testing. Each
  guide closes with a "what RIO does not have yet" backlog
  (STM, `Schedule`, bounded-concurrency `forEach`, tracing /
  metrics, supervisor model, plus `@effect/schema` for the
  Effect-TS guide) and a short "what RIO has that the source
  language does not" section calling out structural error
  rows and `Layer`'s exact-shrink typing.
- Phase 8.3 docstring audit. Every public function across
  `RIO.Core`, `RIO.Env`, `RIO.Error`, `RIO.Concurrency`,
  `RIO.Layer`, `RIO.Resource`, `RIO.Clock`, `RIO.Spec`,
  `RIO.Test`, and `RIO.Test.Clock` now carries at least one
  inline usage example alongside its existing semantics
  description. Specifically added examples to: `unsafeRunRIO`,
  `catchAll`, `mapError`, `die`, `sandbox`, `unsandbox`,
  `fork`, `join`, `interrupt`, `parTraverse`, `parSequence`,
  `zipPar`, `race`, `raceAll`, `fromRecord`, `fromRIO`,
  `unLayer`, `andThen`, `combine`, `buildLayer`,
  `provideLayer`, `acquireRelease`, `addFinalizer`, `scoped`,
  `now`, `sleep`, `liveClock`, `itRIO`, `newTestClock`.
  Publication of the generated docs to Pursuit waits on the
  first registry release.
- `benchmarks/` (workspace package `rio-benchmarks`) plus
  `docs/performance.md`: Phase 8.4 benchmark suite. Four
  scenarios (bind chain at 100 and 10 000 depths, `ask` +
  `Record.get` loop, sequential vs parallel traversal over a
  32-element array of pure work, typed-failure round-trip via
  `fail` + `catchTag`) plus three baselines (`runRIO' pure
  unit`, raw `Aff pure unit`, service-free pure loop). The
  harness is a small `Aff`-aware analogue of `minibench` that
  samples `process.hrtime()` for nanosecond resolution. The
  perf doc records headline numbers on Apple M1 Pro / node 20
  (per-bind cost ~90 ns amortised, service lookup is
  effectively free, `parTraverse` over pure work is ~3x
  sequential, typed-failure round-trip ~930 ns) and the
  reasoning behind the dominant costs. CI builds the suite
  on every PR; it does not run it, since benchmark numbers in
  CI are too noisy to gate on. Setting up a regression gate
  is tracked as a future backlog item.
- `spikes/phase-6-review/`: Phase 6 review cycle. Four randomised
  stress scenarios driven by `Effect.Random` parameters:
  `parTraverse` over up to eight actions with up to 60 percent
  typed-failure rate, `zipPar` with independent failures on each
  side, `raceAll` over up to six branches, and `fork` plus
  `interrupt` against a chain of up to fifty nested `scoped`
  blocks killed mid-sleep. Each iteration asserts the resource
  counter returns to zero. 250 iterations per scenario per
  invocation (1000 total). Across four consecutive local runs
  (4000 total iterations) the harness reports zero leaks across
  every combinator. CI builds and runs the spike on every PR.
  `FINDINGS.md` records three observations: `raceAll` is
  unbiased over its branches in practice, the fork plus
  interrupt path is stable through depth-50 nested scopes, and
  no flaky iterations were observed at the harness's
  millisecond granularity.
- `docs/06-concurrency.md`: walkthrough of the interruption model
  (citing `spikes/aff-interruption/FINDINGS.md` scenarios S1, S2,
  S2b, S3, S4, S5), uninterruptible regions, the cooperative
  cancellation caveat and its `liftAff (delay (Milliseconds 0.0))`
  mitigation, how `race` interacts with `acquireRelease` and
  `Scope`, `parTraverse` failure semantics, and a "what RIO does
  not give you" section calling out the deliberate omissions of
  structured concurrency, interrupt-with-cause, and fiber-local
  state (Phase 6.4).
- `race` and `raceAll` in `RIO.Concurrency` (Phase 6.3). `race`
  uses `Aff`'s `ParAff` `Alt` instance to run two actions
  concurrently, returns whichever completes first (success or
  typed failure), and interrupts the loser. Finalizers registered
  by the loser via `acquireRelease` or `Scope` run on
  interruption, leveraging the same `Aff.bracket` guarantees from
  Phase 0.5 scenario S3. `raceAll` takes a `NonEmptyArray` and is
  the left fold of `race` over the array (no `parOneOf` because
  the `Parallel f m` constraint solver couldn't infer the
  instance from a polymorphic-`f` callsite; the fold is
  equivalent and uses concrete types throughout). `control` and
  `foldable-traversable` added to the dependency manifest.
