# RIO: A ZIO/Effect-Style Library for PureScript, Build Plan

## Status (May 2026)

This document is the original phase-by-phase build plan. It is kept for
historical context. **Nothing has been published yet.** References to
`v0.1.0`, `v0.2`, `v0.3` below are phase labels from the original plan,
not actual releases on the PureScript registry or Pursuit. The library
is being developed in `main` with no released tags.

What this means for items below:

- "Status: Complete" entries describe code that exists in `main`, not a
  released artifact. The Phase 8.5 "Repository ready for release" note
  is accurate at the code level only; the tag/publish step has not been
  taken.
- The `v0.2 candidate backlog` and `v0.3 and beyond` sections were
  written before the surface grew. Several items listed there have
  already landed in `main` and are marked inline.
- The `Iteration Cycles` section sketches a two-week sprint cadence
  that was never adopted in practice. It is left here as a future
  template if the project ever grows beyond a single maintainer.

For the canonical map of what exists today, look at `docs/` and the
module list in `src/`. This plan is a snapshot of intent, not a status
dashboard.

## Project Overview

**RIO** (Reader + IO + Either) is a PureScript library that brings the ergonomics of ZIO (Scala) and Effect (TypeScript) to PureScript. The core type tracks three orthogonal concerns in one monad: a **R**eader-style environment of required services, an extensible error channel, and **I**O via `Aff`.

```purescript
newtype RIO r e a = RIO (Record r -> Aff (Either (Variant e) a))
```

Both `r` (services) and `e` (errors) are row types, so requirements and possible failures aggregate automatically on composition and shrink as they're handled or provided.

### Goals

- A core `RIO` type with full monad/applicative/functor instances and row-based service + error tracking
- Service pattern with `provide`, `provideAll`, and a `Layer`-equivalent for composing service constructors
- Error handling primitives that narrow the error row as failures are caught
- Resource safety primitives (`acquireRelease`, `scoped`)
- Concurrency primitives built atop `Aff` (fork, join, race, parallel combinators, interruption)
- Test utilities (mock services, deterministic clock, in-memory implementations) usable from Phase 2 onward
- Documentation, examples, and migration guides for users coming from ZIO/Effect

### Non-Goals (original v1 framing)

- Generator-style direct syntax (not feasible without compiler support; do-notation is the supported style). Qualified-do has since been explored and `RIO.Resource.Do` / `RIO.Concurrency.Par` shipped to `main`.
- A full STM implementation. **Now shipped:** `RIO.STM` covers `TVar`, `atomically`, `TQueue`, `TMap`, `TSemaphore`, `THub`.
- Streaming (still deferred; would live in a separate `rio-streams` package).
- Metrics and tracing integration. **Now shipped:** `RIO.Tracer` and `RIO.Metrics` live in `main`.

### Tech Stack

- **Language:** PureScript 0.15.x family, pinned to a specific patch in CI
- **Build:** Spago (next-gen, `spago.yaml`-based)
- **Runtime base:** `purescript-aff` for the IO layer
- **Row machinery:** `purescript-variant` for the error channel, `purescript-record` for environment manipulation, `Prim.Row` for inference constraints
- **Test:** `purescript-spec` with `purescript-spec-quickcheck` for property tests
- **Benchmark:** `purescript-minibench` plus a small custom harness (avoiding the largely unmaintained `purescript-benchotron`)
- **CI:** GitHub Actions matrix across Node LTS versions (20, 22), pinned `purs` version
- **Docs:** `purs docs` to Pursuit for API reference; a small hand-written guide site (Docusaurus) for tutorials and migration guides

---

## How to Use This Plan with AI Agents

Each phase is broken into **work items**. A work item is sized to be a single PR by a single agent. Each item has:

- **Goal:** what success looks like
- **Inputs:** prerequisites and reference material
- **Deliverables:** files, functions, tests, docs that must exist
- **Acceptance criteria:** checks an agent (or human reviewer) must verify before marking done
- **Out of scope:** explicit non-goals so the agent doesn't sprawl

Agents should work one item at a time, open a PR per item, and not move on until the item's acceptance criteria are green in CI. At the end of each phase there is an **integration & review cycle** where a separate agent (or the team) reviews the phase as a whole.

**Spikes** are special work items that exist to de-risk a design choice. A spike's only deliverable is a written finding (kept or thrown away, but informing the next item). Spikes appear in Phase 0 and gate later phases.

---

## Phase 0, Foundations, Repo Setup, and De-Risking Spikes

**Goal:** A working repository skeleton that compiles, tests, and publishes locally, plus two technical spikes that prove the design's load-bearing assumptions before any API is finalized.

### 0.1 Repository bootstrap

- **Goal:** Empty but valid Spago project named `rio`.
- **Deliverables:**
  - `spago.yaml` with package name, version `0.0.0`, license MIT
  - `src/`, `test/`, `docs/`, `examples/`, `bench/`, `spikes/` directories
  - `.gitignore`, `LICENSE`, `README.md` (stub), `CHANGELOG.md` (stub)
- **Acceptance:** `spago build` succeeds; `spago test` runs a placeholder test that passes.

### 0.2 CI pipeline

- **Goal:** GitHub Actions config running on every PR.
- **Deliverables:** `.github/workflows/ci.yml` running `spago build`, `spago test`, `purs-tidy check`, with `purs` pinned to a specific patch (e.g. `0.15.15`) and Node 20 + 22.
- **Acceptance:** CI green on a trivial PR.

### 0.3 Code style and contributor docs

- **Deliverables:** `purs-tidy` config, `CONTRIBUTING.md` describing branch naming, commit-message style, PR template referencing the work-item structure of this plan, and a note that work items are sized for one PR.
- **Acceptance:** Lint runs clean on the empty project.

### 0.4 Spike: row inference for service and error composition

- **Goal:** Prove that PureScript's row machinery can express the API shapes we want without forcing the user to write explicit type signatures everywhere. This is the highest technical risk in the project.
- **Inputs:** Read the `Prim.Row` docs, study how `purescript-variant` and `purescript-record` use `Union`, `Nub`, `Cons`, and `Lacks`.
- **Deliverables:** `spikes/row-inference/` containing:
  - A prototype `RIO r e a` newtype with `pure`, `bind`, `ask`, `fail`, `provide`, `catchTag`.
  - Ten realistic example programs (service composition, error composition, mixed) written without any user-supplied type signatures.
  - `spikes/row-inference/FINDINGS.md` recording: which examples compile cleanly, which need annotations, the shape of the error messages when things go wrong, and a recommendation on whether to proceed with the current `RIO` shape or adjust it.
- **Acceptance:** Findings document is reviewed and a go/no-go decision is recorded. If "adjust," the recommended adjustments feed into Phase 1's API design.
- **Out of scope:** Production-quality implementation. This is throwaway code.

### 0.5 Spike: `Aff` cancellation and interruption semantics

- **Goal:** Prove that `Aff`'s cancellation model is strong enough to support ZIO-style `fork`/`interrupt`/`acquireRelease`, or identify the gap.
- **Deliverables:** `spikes/aff-interruption/` containing:
  - A minimal `Fiber`-like wrapper around `Aff.launchAff` and `Aff.killFiber`.
  - Tests covering: interrupt during a long sleep, interrupt during a synchronous block, interrupt during a finalizer, interrupt of an interrupted fiber, interrupt before start, resource release after interrupt.
  - `spikes/aff-interruption/FINDINGS.md` recording what works, what doesn't, and whether RIO needs its own runtime layer above `Aff` or can rely on `Aff` directly.
- **Acceptance:** Findings document is reviewed; the conclusions feed into Phase 6's design and into the resource-safety primitives in Phase 4.
- **Out of scope:** Designing the public concurrency API. That's Phase 6.

### Phase 0 review cycle

A second agent verifies: clone the repo fresh, follow `CONTRIBUTING.md`, run all listed commands. Read both spike findings and confirm they answer the questions posed. File issues for any friction or missing analysis. Resolve before proceeding to Phase 1.

---

## Phase 1, Core Type and Monadic Interface

**Goal:** `RIO r e a` exists, has the standard typeclass instances, supports do-notation, and exposes a `fail` primitive whose final shape was settled by the Phase 0.4 spike.

### 1.1 The `RIO` newtype and runners

- **Goal:** Define `RIO` and provide two runners that cover the common cases.
- **Deliverables:**
  - `src/RIO/Core.purs` exporting `RIO` (newtype, constructor not exposed to users).
  - `runRIO :: forall e a. RIO () e a -> Aff (Either (Variant e) a)` for the general case.
  - `runRIO' :: forall a. RIO () () a -> Aff a` for the fully-handled case, where the error row is empty.
  - A lower-level `unsafeRunRIO` for internals.
- **Acceptance:** Type compiles; `runRIO (pure 42)` returns `Right 42`; `runRIO' (pure 42)` returns `42`.

### 1.2 Functor, Apply, Applicative, Bind, Monad

- **Deliverables:** Instances in `RIO.Core`, each with a docstring noting how it threads the environment and short-circuits on `Left`.
- **Acceptance:**
  - Property tests: functor identity, functor composition, applicative identity/composition/homomorphism/interchange, monad left/right identity, associativity.
  - All instances compile under the same `r` and `e` parameters; no leakage.

### 1.3 MonadEffect, MonadAff lifts, and `fail`

- **Deliverables:**
  - `MonadEffect (RIO r e)` and `MonadAff (RIO r e)` instances.
  - `fail :: forall sym a r e' e b. IsSymbol sym => Cons sym a e' e => Proxy sym -> a -> RIO r e b`, in its final shape (informed by the Phase 0.4 spike). It is not "revisited" later.
- **Acceptance:**
  - Test: `liftEffect (Console.log "hi")` runs without error.
  - Test: `fail (Proxy :: _ "notFound") { id: 1 }` produces `Left` with the right variant tag.

### 1.4 Documentation: "The RIO type, explained"

- **Deliverables:** `docs/01-core-type.md` walking through the type, the three parameters, and a comparison table with ZIO and Effect.
- **Acceptance:** A reader unfamiliar with the codebase can write a `pure`/`bind`/`liftEffect` program from the doc alone, verified by a human reader.

### Phase 1 review cycle

A reviewer agent attempts to implement five small programs from the docs without reading source. Each program either compiles first try or doesn't; tally the score and file issues for any that didn't. Resolve before Phase 2.

---

## Phase 2, Service Pattern (Environment Row) and Basic Test Helpers

**Goal:** Services can be required by row, provided one at a time or in bulk, and the row shrinks accordingly. A minimal set of test helpers ships alongside so tests in this and later phases can use mock services without rework.

### 2.1 `ask` and `asks` primitives

- **Deliverables:**
  - `ask :: forall sym a r' r e. IsSymbol sym => Cons sym a r' r => Proxy sym -> RIO r e a`
  - `asks :: forall sym a r' r e b. IsSymbol sym => Cons sym a r' r => Proxy sym -> (a -> b) -> RIO r e b`
- **Acceptance:** Test demonstrating that an effect requiring `(logger :: Logger | r)` can read the logger from the environment and the row is inferred correctly.

### 2.2 `provide` for single-service injection

- **Deliverables:**
  - `provide :: forall sym a r' r e b. IsSymbol sym => Cons sym a r' r => Proxy sym -> a -> RIO r e b -> RIO r' e b`. (The `Lacks` constraint from the original draft is redundant given `Cons`; confirmed by the Phase 0.4 spike.)
- **Acceptance:**
  - Test: providing `logger` removes `logger` from the required row.
  - Negative test (compile-fail snapshot via a `test/compile-fail/` script that runs `spago build` against a fixture and asserts non-zero exit): providing a service whose type doesn't match the requirement fails with a recognisable error.

### 2.3 `provideAll` for full-environment injection

- **Deliverables:** `provideAll :: forall r e a. Record r -> RIO r e a -> RIO () e a`
- **Acceptance:** After `provideAll`, the resulting `RIO () e a` is runnable directly with `runRIO` or `runRIO'`.

### 2.4 Service definition convention

- **Goal:** Establish the idiomatic way to define a service.
- **Deliverables:**
  - `docs/02-services.md` showing a `Logger` service: type alias for the record, smart constructors for the operations that already lift into `RIO`, sample `consoleLogger` implementation.
  - An `examples/logger/` example.
- **Acceptance:** Example compiles, runs, and is referenced from the docs.

### 2.5 Composing services (row union behaviour)

- **Deliverables:** A doc + test showing that composing two effects requiring disjoint services produces an effect whose row is the union, inferred automatically in the patterns that worked in the Phase 0.4 spike. Any pattern that needed an explicit annotation is documented as such, not hidden.
- **Acceptance:** No manual annotation needed for the documented "ergonomic" patterns; documented patterns match the spike's findings.

### 2.6 Basic test helpers (pulled forward from old Phase 7)

- **Goal:** Make services testable from Phase 2 onward, so tests in later phases don't get rewritten.
- **Deliverables:**
  - `src/RIO/Test.purs` exporting `mockService` (an alias for `provide` that reads better in tests) and a small `recording` helper for capturing calls into a `Ref (Array Call)`.
  - One example test using `mockService` in `test/`.
- **Acceptance:** Example test passes; the helper is used in at least one of the Phase 2 tests above.
- **Out of scope:** `TestClock`, `itRIO`, `purescript-spec` integration. Those stay in the dedicated testing phase (now Phase 7).

### Phase 2 review cycle

Reviewer audits inference quality: write 10 realistic service compositions, note any case where the user has to write an explicit type signature for the compiler to accept it. Cross-reference against the Phase 0.4 findings; file issues for regressions or undocumented sharp edges.

---

## Phase 3, Error Channel (Error Row)

**Goal:** Typed errors compose by row, narrow on catch, and integrate cleanly with the service pattern. `fail` already exists from Phase 1; this phase adds the handling side.

### 3.1 `catchTag`

- **Deliverables:**
  - `catchTag :: forall sym a e' e r b. IsSymbol sym => Cons sym a e' e => Proxy sym -> (a -> RIO r e' b) -> RIO r e b -> RIO r e' b`, catching one tagged error, removing it from the row, and possibly introducing new errors via the handler.
- **Acceptance:**
  - Test: an effect that can fail with `(notFound :: NotFound, parse :: ParseError)` becomes `(parse :: ParseError)` after `catchTag _notFound`.
  - Test: handler can itself fail with a different tag, and that tag appears in the resulting row.

### 3.2 `catchAll` and `mapError`

- **Deliverables:**
  - `catchAll :: forall r e e' a. (Variant e -> RIO r e' a) -> RIO r e a -> RIO r e' a`
  - `mapError :: forall r e e' a. (Variant e -> Variant e') -> RIO r e a -> RIO r e' a`
- **Acceptance:** Tests confirm row replacement semantics; property test that `catchAll (rethrow)` is identity.

### 3.3 Defects vs. failures

- **Goal:** Distinguish typed failures (in the row) from defects (unexpected `Aff` exceptions, async errors).
- **Deliverables:**
  - `die :: forall r e a. Error -> RIO r e a` for explicit defects.
  - `sandbox :: forall r e a. RIO r e a -> RIO r e (Either Error a)`, exposing defects as values.
  - `unsandbox` for the reverse.
- **Acceptance:** Tests show that `Aff` exceptions raised inside an RIO program surface as defects, not as typed errors, and that `sandbox` makes them observable.

### 3.4 Error narrowing in practice

- **Deliverables:** `docs/03-errors.md` with a walked-through example that starts with three possible errors and handles them one at a time, showing the row shrinking at each step in the inferred type.
- **Acceptance:** All inferred types in the doc match what the compiler actually produces, verified via a REPL session captured in the doc.

### Phase 3 review cycle

Reviewer focuses on error-message quality: deliberately introduce wrong tags, missing handlers, double-handles, and verify the compiler errors are intelligible. File issues for the worst offenders; consider `Fail` instances for the most common mistakes.

---

## Phase 4, Resource Safety (formerly Phase 5)

**Goal:** Bracket-style resource management integrated with the error channel. This phase ships before Layers, because Layers depend on it.

### 4.1 `acquireRelease`

- **Deliverables:** `acquireRelease :: forall r e a b. RIO r e a -> (a -> RIO r Void Unit) -> (a -> RIO r e b) -> RIO r e b`. Release cannot fail with a typed error.
- **Acceptance:** Tests covering normal exit, typed-failure exit, defect exit; release always runs.

### 4.2 `Scope` and `scoped`

- **Deliverables:** A `Scope` service that collects finalizers; `scoped` runs a computation in a fresh scope and runs all finalizers on exit in LIFO order.
- **Acceptance:** Property test: finalizers run in reverse order of acquisition, always, regardless of how the scope ends (success, typed failure, defect).

### 4.3 Interaction with `Aff` cancellation

- **Goal:** Use the findings from the Phase 0.5 spike to make resource release robust under cancellation.
- **Deliverables:**
  - `uninterruptibleMask`-equivalent primitive if the spike concluded RIO needs one (the name and shape come out of the spike).
  - Tests that resources allocated inside `acquireRelease` are released even if the surrounding `Aff` fiber is killed.
- **Acceptance:** Tests pass; behaviour matches the contract documented in the spike findings.

### Phase 4 review cycle

Stress-test: write a program that opens 1000 nested scopes, fails at random depths, verify zero leaks. Repeat with random `Aff.killFiber` injection.

---

## Phase 5, Layers (formerly Phase 4)

**Goal:** A `Layer` type that builds services from other services, can itself fail, composes horizontally and vertically, and is resource-safe by construction (using Phase 4's `Scope`).

### 5.1 `Layer rIn e rOut` type and runner

- **Deliverables:**
  - `Layer rIn e rOut` representing "given services in `rIn`, produce services `rOut` (possibly failing with `e`)".
  - `buildLayer :: forall e rOut. Layer () e rOut -> Aff (Either (Variant e) (Record rOut))`.
- **Acceptance:** Trivial layer producing `{ logger :: Logger }` from `()` builds successfully.

### 5.2 Layer composition

- **Deliverables:**
  - `(>>>)` for sequential composition (output of one feeds input of next).
  - `(<+>)` for horizontal composition (independent layers combined into a richer output row).
- **Acceptance:** A `databaseLayer >>> userServiceLayer` example works and the resulting layer's `rIn` is correctly inferred. The horizontal composition's row-union behaviour matches the Phase 0.4 spike's findings.

### 5.3 `provideLayer`

- **Deliverables:** `provideLayer :: forall rIn rOut e e' eOut a. Union e e' eOut => Layer rIn e rOut -> RIO rOut e' a -> RIO rIn eOut a`. The shape of the error-row union (via `Union` constraint with an output row) is the one validated in the Phase 0.4 spike. If the spike rejected that shape, this signature changes accordingly and is documented.
- **Acceptance:** End-to-end example: build a `Logger + Database` layer, provide it to a program, run.

### 5.4 Resource-safe layers via `Scope`

- **Deliverables:** Layers acquire resources in a `Scope` and release them when the providing scope exits, on both success and failure paths.
- **Acceptance:** Test with a fake resource that records acquire/release events; verify release happens on success, typed failure, and defect.

### Phase 5 review cycle

Reviewer builds a non-trivial layered application (5+ services, at least one failing layer, at least one resource-managing layer) and reports DX issues.

---

## Phase 6, Concurrency

**Goal:** Fork-based concurrency, parallel combinators, racing, and interruption, all building on Phase 0.5's findings about what `Aff` can and cannot do.

### 6.1 `Fiber` and `fork`

- **Deliverables:**
  - `Fiber e a` representing an in-flight computation.
  - `fork :: forall r e a. RIO r e a -> RIO r () (Fiber e a)`. Forking is infallible from the parent's perspective; the child's errors are inside the `Fiber`.
  - `join :: forall r e a. Fiber e a -> RIO r e a`.
  - `interrupt :: forall r e a. Fiber e a -> RIO r () Unit`.
- **Acceptance:** Tests cover fork+join round-trip, joining a failed fiber surfaces its error in the joining context, interrupt cancels in-flight `Aff` work (with the caveats documented in the Phase 0.5 findings).

### 6.2 Parallel combinators

- **Deliverables:**
  - `parTraverse`, `parSequence` over lists/arrays, layered on `Aff`'s `parallel` applicative.
  - `zipPar :: RIO r e a -> RIO r e b -> RIO r e (Tuple a b)`.
- **Acceptance:** Property tests for associativity up to tupling; timing test that two 100ms effects run in ~100ms not ~200ms.

### 6.3 `race` and `raceAll`

- **Deliverables:** First-to-complete wins; loser is interrupted; resource safety preserved (via Phase 4's primitives).
- **Acceptance:** Test: racer that allocates a resource then sleeps is interrupted, and the resource is released.

### 6.4 Interruption semantics doc

- **Deliverables:** `docs/06-concurrency.md` covering the interruption model, uninterruptible regions, and how it interacts with `acquireRelease`. The doc cites the Phase 0.5 findings as the authoritative source on what's guaranteed.
- **Acceptance:** Document is reviewed by someone familiar with ZIO's interruption model and judged accurate in spirit.

### Phase 6 review cycle

Concurrency bugs hide. Reviewer writes a property-based test suite that runs each combinator under random scheduling delays and asserts no leaks, no deadlocks, no lost errors over 10,000 runs.

---

## Phase 7, Test Suite Polish

**Goal:** Round out the testing story that started in Phase 2.6. Most service testing already works at this point; this phase adds clock control, spec integration, and patterns.

### 7.1 `TestClock` service

- **Deliverables:** A `Clock` service interface plus a `TestClock` implementation where time can be advanced manually.
- **Acceptance:** A `sleep` primitive built on `Clock` is testable in milliseconds of wall clock for arbitrary virtual durations.

### 7.2 `purescript-spec` integration helpers

- **Deliverables:** `RIO.Spec` module providing `itRIO`, `runSpecRIO` so `RIO` programs slot directly into a spec suite.
- **Acceptance:** Example spec file in `test/` using these helpers.

### 7.3 Test patterns doc

- **Deliverables:** `docs/07-testing.md` covering: mocking services (from 2.6), `TestClock`, spec integration, recording wrappers, and how to structure tests for layered programs.
- **Acceptance:** Reader can write a tested layered program from the doc alone.

### Phase 7 review cycle

Reviewer ports one of the Phase 2/3/5 examples to use only test services and asserts the experience is at parity with running against real implementations.

**Status:** Complete. See `spikes/phase-7-review/`. Ports the Phase 5 review's six-service layered application to a `Test.Spec` suite that uses only `RIO.Spec`, `RIO.Test`, and `RIO.Test.Clock`. Four scenarios (happy path, failing layer, program failure after use, time-sensitive forks) all pass on every run. DX is at parity: the test-helpers shape replaces a hand-rolled `ScenarioResult` harness with ordinary `it` / `itRIO_` bodies and `shouldEqual` assertions. Three small DX observations recorded in `FINDINGS.md` as candidates for a future helper kit; none block the production core.

---

## Phase 8, Documentation, Examples, Release Prep

**Goal:** Library is usable by an external developer following only public docs.

### 8.1 Tutorial: building a small web service

- **Deliverables:** `examples/todo-api/`, a small HTTP service using HTTPure, with services for logging, persistence (in-memory + a SQLite-backed variant via `purescript-node-sqlite3` or similar), and request handling. Demonstrates layers, errors, and resource safety.
- **Acceptance:** Reader can clone, `spago run`, hit endpoints, see logs.

**Status:** Complete. See `examples/todo-api/`. Built on HTTPurple 4.0 (rather than HTTPure, which is unmaintained in registry 77.0.0). In-memory persistence only; the SQLite-backed variant is deferred since the layer-swap story is already demonstrated by the Phase 7 review and adding a second driver here would dilute rather than reinforce the tutorial. Four endpoints (GET / POST / DELETE / GET-by-id) verified end-to-end with `curl`; HTTP semantics (200, 204, 400, 404, 405) all behave correctly. README walks readers through the module layout and the bridging pattern (`runRIO` inside an HTTPurple router).

### 8.2 Migration guides

- **Deliverables:**
  - `docs/migrating-from-zio.md`
  - `docs/migrating-from-effect-ts.md`
  - Each maps idioms 1:1 with code snippets in both languages.
- **Acceptance:** A ZIO user and an Effect-TS user each review their respective doc and confirm the mappings are accurate.

**Status:** Complete. See `docs/migrating-from-zio.md` and `docs/migrating-from-effect-ts.md`. Each guide opens with a core-type comparison table, walks through lifting / composing / services / providing / typed errors / resource safety / concurrency / layers / testing with paired code snippets, and closes with two backlog sections ("what RIO does not have yet" and "what RIO has that the source language does not"). External review (ZIO user, Effect-TS user) is a release gate item.

### 8.3 API reference

- **Deliverables:** `purs docs` output published to Pursuit.
- **Acceptance:** Every public function has a docstring with at least one example.

**Status:** Complete (docstring audit). Every public function across `RIO.Core`, `RIO.Env`, `RIO.Error`, `RIO.Concurrency`, `RIO.Layer`, `RIO.Resource`, `RIO.Clock`, `RIO.Spec`, `RIO.Test`, and `RIO.Test.Clock` now carries a docstring with at least one inline code example. Worked examples were added to: `unsafeRunRIO`, `catchAll`, `mapError`, `die`, `sandbox`, `unsandbox`, `fork`, `join`, `interrupt`, `parTraverse`, `parSequence`, `zipPar`, `race`, `raceAll`, `fromRecord`, `fromRIO`, `unLayer`, `andThen`, `combine`, `buildLayer`, `provideLayer`, `acquireRelease`, `addFinalizer`, `scoped`, `now`, `sleep`, `liveClock`, `itRIO`, `newTestClock`. Pursuit publication waits on the first registry release, since uploading docs requires the package to be tagged in the registry.

### 8.4 Performance baseline

- **Deliverables:** A benchmark suite using `purescript-minibench` plus a small custom harness covering: monadic bind in a tight loop, service lookup overhead, parallel vs sequential traversal. Baselines committed.
- **Acceptance:** Numbers documented in `docs/performance.md` with notes on the dominant costs.

**Status:** Complete. See `benchmarks/` (workspace package `rio-benchmarks`) and `docs/performance.md`. The suite covers four scenarios (bind chain at 100 / 10 000 depths, `ask` + `Record.get` loop, sequential vs parallel traversal over 32 pure elements, and `fail` + `catchTag` round-trip) plus three baselines (`runRIO' pure unit`, raw `Aff pure unit`, service-free pure loop). The harness samples `process.hrtime()` before and after each invocation for nanosecond resolution (`Effect.Now` was too coarse). Headline numbers on Apple M1 Pro / node 20: per-bind cost ~90 ns amortised, service lookup is essentially free, `parTraverse` over pure work costs ~3x sequential traverse (break-even ~10 μs of latency per element), typed failure round-trip ~930 ns. CI builds the suite on every PR (it does not run it; benchmark numbers in CI are too noisy to gate on). Setting up the regression gate is captured as a future backlog item in `docs/performance.md`.

### 8.5 Release prep

- **Deliverables:**
  - Tagged release on GitHub.
  - Published to the PureScript registry (distribution) and to Pursuit (docs).
  - Announcement post draft for the PureScript Discourse.
- **Acceptance:** A fresh project can add `rio` as a dependency and use it.

**Status:** Code-complete; **not released**. The `spago.yaml` version, CHANGELOG header, and README landing page are all in `main`, but no GitHub tag has been cut and nothing has been pushed to the PureScript registry or Pursuit. Treat the "0.1.0" string in `spago.yaml` as a placeholder, not a published version. The tag-and-publish step has been intentionally deferred while the surface continues to grow (STM, tracing, metrics, schedule, qualified-do, the `rio-httpurple` companion package, etc. all landed after this entry was originally written).

### Phase 8 review cycle

Final acceptance: an external PureScript developer (not involved in development) is asked to build a small program using only public docs and the registry release. Their feedback drives the first patch.

---

## Phase Dependency Summary

```
0.1, 0.2, 0.3 (setup)
        |
0.4 (row-inference spike)        0.5 (Aff cancellation spike)
        |                                 |
        v                                 v
        1 (core type) <------------------+
        |
        2 (services + basic test helpers)
        |
        3 (errors)
        |
        4 (resource safety) <----- Phase 0.5 findings
        |
        5 (layers)         <----- Phase 4 primitives
        |
        6 (concurrency)    <----- Phase 0.5 findings, Phase 4 primitives
        |
        7 (test polish)
        |
        8 (docs + release)
```

The two spikes in Phase 0 gate everything after them. If 0.4 rejects the current `RIO` shape, Phase 1's API changes before any production code is written. If 0.5 reveals an `Aff` gap, Phases 4 and 6 either work around it or RIO ships its own thin runtime layer.

---

## Versioning Policy

While in the `0.x` series, breaking changes may land in any minor release (`0.1 -> 0.2`) without a deprecation cycle. Patch releases (`0.1.1`, `0.1.2`) are always backwards-compatible bug fixes and additive changes. The `1.0.0` release commits to semver proper, with a documented deprecation policy and a stable public API surface.

---

## Iteration Cycles (aspirational, never adopted)

The two-week sprint cadence below was never actually run; the project
has been solo-maintained on an as-and-when basis. The template is left
here as a sketch of how a multi-contributor cadence could look.

### Iteration template

- **Week 1:** Pick 3 to 5 work items from the backlog. Contributors work in parallel, one item each. Daily merge of completed items.
- **Week 2:** Integration testing across merged items. Documentation updates. One contributor runs a "user journey" exercise: pretend to be a new user, attempt a non-trivial task, file issues.
- **End of iteration:** Tag a minor release. Update CHANGELOG. Update roadmap.

### Original "v0.2 candidate backlog" (now mostly landed)

- ~~STM-style transactional refs (`TRef`, `atomically`).~~ **Done.** Shipped as `RIO.STM` with `TVar`, `TQueue`, `TMap`, `TSemaphore`, `THub`.
- Streaming (`RStream r e a`) as a sibling package. **Still open.**
- ~~Tracing hooks for observability tools.~~ **Done.** `RIO.Tracer` in `main`.
- ~~Metrics (counters, gauges, histograms) as a built-in service.~~ **Done.** `RIO.Metrics` in `main`.
- ~~Schedule combinators (`Schedule` from ZIO).~~ **Done.** `RIO.Schedule` in `main`.
- Improved compiler error messages via custom `Fail` instances. **Still open.**

### Original "v0.3 and beyond" (partially landed)

- ~~DSL exploration for direct-style syntax via PureScript's `qualified-do`.~~ **Done.** Spike in `spikes/qualified-do/`, with the two winners promoted into `main` as `RIO.Resource.Do` and `RIO.Concurrency.Par`.
- ~~`rio-httpure` integration package.~~ **Done** (as `rio-httpurple`, since HTTPure is unmaintained). The companion package lives at `http/` and `examples/todo-api/` consumes it.
- `rio-node`, `rio-aws`, `rio-postgres` integration packages. **Still open.**
- Property-based testing integration with `purescript-quickcheck` specifically tuned for RIO programs. **Still open** (basic QuickCheck is available; no RIO-tuned harness yet).

### Other items not in the original backlog that have shipped

- `RIO.Local` (fiber-local implicit context with `locally`-scoped overrides).
- OpenTelemetry / OTLP demo example (`examples/otel-demo/`).
- Test helpers (`RIO.TestHelpers`, `RIO.SpecHelpers`) for richer assertions and spec wiring.

---

## Risk Register

| Risk | Likelihood | Impact | Mitigation |
|---|---|---|---|
| Row inference produces incomprehensible errors at scale | High | High | Phase 0.4 spike; Phase 2/3 review cycles focused on error quality; custom `Fail` instances for common mistakes are a future backlog item. |
| `Aff`'s cancellation model is too weak for ZIO-style interruption | Medium | High | Phase 0.5 spike. If `Aff` can't carry it, the spike's findings define the workaround (uninterruptible regions, custom runtime layer, or scoped guarantees) before Phase 4 starts. |
| Layer composition becomes verbose without intersection types | Medium | Medium | Lean on row-union helpers validated in the Phase 0.4 spike; document escape hatches. |
| Performance overhead from `Record`/`Variant` indirection | Medium | Medium | Phase 8.4 benchmarks; optimize hot paths with `unsafeCoerce` where safe. |
| External adoption blocked by PureScript ecosystem size | High | Low | Out of scope. Ship a good library and let it find its users. |

---

## Definition of Done (Project-Wide)

A work item is **done** when:

1. Code compiles with no warnings under the pinned `purs` version.
2. All new public functions have docstrings with at least one example.
3. Tests cover happy path, at least one failure path, and at least one edge case.
4. CI is green on the PR branch.
5. CHANGELOG entry added under "Unreleased".
6. If user-facing: relevant doc file in `docs/` is updated.
7. PR description references the work-item ID from this plan.

A spike is **done** when its findings document is written, reviewed, and the recommended decision is recorded (kept or rejected) before the dependent phase begins.

A phase is **done** when all its work items (and any gating spikes) are done **and** the phase review cycle has completed with no critical issues open.
