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
