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
