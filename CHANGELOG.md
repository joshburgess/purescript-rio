# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
once it reaches `1.0.0`. While in the `0.x` series, minor releases may include
breaking changes (see `PROJECT_BUILD_PLAN.md`, "Versioning Policy").

## [Unreleased]

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
  message and listing candidates for v0.2 custom `Fail` instances.
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
