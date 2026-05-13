# Phase 7 Review: Layered Application via Test Helpers

**Status:** Complete.

**Recommendation:** **GO.** The full Phase 5-style layered
application (logger, database, clock, user service) ports
cleanly to a `Test.Spec` suite that uses only `RIO.Spec`,
`RIO.Test`, and `RIO.Test.Clock`. All four scenarios pass on
every run, the assertions read like ordinary spec bodies, and
the boilerplate cost over the Phase 5 review's hand-rolled
harness is small: each scenario gains roughly a `recording` and
a `newTestClock` line and loses the harness's manual
event-comparison plumbing.

## Method

A workspace sub-package, `spike-phase-7-review`, depends on the
real `rio` package and ports the Phase 5 review's layered
application to a `Test.Spec` suite. The same three layers
(`loggerLayer`, `dataLayer`, `userServiceLayer`) compose into
the same `appLayer`; the differences are:

- The clock is `RIO.Clock.Clock`, supplied at the test boundary
  from `RIO.Test.Clock.newTestClock` rather than built inside a
  layer.
- The `Logger` is supplied a recorder produced by
  `RIO.Test.recording`.
- The harness is `runSpecRIO` from `RIO.Spec`, not a hand-rolled
  `launchAff_` script.

No internal modules are reached into; everything goes through
`RIO.Core`, `RIO.Layer`, `RIO.Spec`, `RIO.Test`, and
`RIO.Test.Clock`.

### Services

| Service       | Operations                                | Layer            |
| ------------- | ----------------------------------------- | ---------------- |
| `Logger`      | `log :: String -> Aff Unit`               | `loggerLayer`    |
| `Database`    | `fetchUser :: Int -> Aff (Maybe String)`  | `dataLayer`      |
| `Clock`       | `now`, `sleep` (re-exported `RIO.Clock`)  | injected at root |
| `UserService` | `greet`, `greetAfter`                     | `userServiceLayer` |

### Layers

```
loggerLayer record
  :: Layer (clock :: Clock) e (logger :: Logger, clock :: Clock)

dataLayer dbUrl
  :: Layer (logger :: Logger, clock :: Clock)
           (dbConnect :: String)
           ( database :: Database
           , logger :: Logger
           , clock :: Clock
           )

userServiceLayer
  :: forall e
   . Layer ( database :: Database
           , logger :: Logger
           , clock :: Clock
           )
           e
           (userService :: UserService)

appLayer dbUrl record
  :: Layer (clock :: Clock) (dbConnect :: String) (userService :: UserService)
appLayer dbUrl record =
  loggerLayer record >>> dataLayer dbUrl >>> userServiceLayer
```

The `(clock :: Clock)` passthrough on `loggerLayer` and the
`(logger, clock)` passthrough on `dataLayer` are the Phase 5
DX-1 pattern: there is no implicit passthrough operator for
sequential composition, so a layer that wants to expose upstream
services to its downstream must list them in its output record.

### Scenarios

`spec` runs four `it` bodies via `runSpecRIO`:

* **A (happy path).** `recording` produces the recorder for the
  logger; `newTestClock` produces the clock. The program greets
  three users twice, then the test asserts the recording
  byte-for-byte (`db-open` followed by six `greet uid=N` lines).
* **B (failing layer).** `dataLayer ""` raises `dbConnect`
  before the program body runs. The test asserts `Left
  (dbConnect: empty database url)` and an empty recording.
* **C (program failure after service use).** `greet 7` runs
  once, then `fail (Proxy :: Proxy "progBoom") unit`. The test
  asserts `Left progBoom` and a two-event recording.
* **D (time-sensitive behaviour).** Two forked `greetAfter`
  calls park inside `clock.sleep`. The test interleaves `advance
  50ms` / `advance 50ms` / `advance 100ms` with assertions on a
  completion `Ref`; each fiber wakes exactly when its deadline
  is crossed.

### Results

```
$ npx spago run -p spike-phase-7-review
Phase 7 review: layered app via test helpers
  A: happy path - greet runs, logger records every line
  B: failing layer - dbConnect surfaces, body skipped
  C: program failure after service use - greet runs, progBoom surfaces
  D: greetAfter sleeps until virtual time advances
Summary
4/4 tests passed
```

All four scenarios pass on every run. Wired into CI alongside
the Phase 5 and Phase 6 review spikes.

## What This Validates

- **`RIO.Test.recording`.** A single allocation per test, the
  `record` field slots into any service record as an `Aff Unit`
  field, and `calls` reads back a deterministic array. The
  failing-layer scenario (B) confirms it stays empty when the
  layer fails before the program runs, and the program-failure
  scenario (C) confirms it captures pre-failure events only.
- **`RIO.Test.Clock.newTestClock`.** Built once at the top of a
  test, the returned `clock` injects through `provideAll` like
  any other service, and the `advance` controller drives forked
  fibers deterministically. Scenario D shows two pending
  sleepers waking in deadline order across multiple `advance`
  calls, with `Aff.delay (Milliseconds 0.0)` yielding the event
  loop between an `advance` and the assertion that reads its
  effect.
- **`RIO.Spec.itRIO_`.** Provides services via `provideAll` and
  runs the program with `runRIO'`, defects surface as `Aff`
  exceptions that `Spec` reports as test failures. Scenario D
  uses it directly. Scenarios B and C, which need to inspect a
  typed-failure `Left`, fall back to plain `it` + `runRIO`, which
  is exactly the pattern recommended in `docs/07-testing.md`.
- **The overall test-helpers DX.** A scenario's setup reads as
  three lines (`rec <- recording`, `tc <- newTestClock`, build
  the program), the run is one `runRIO`, the assertion is one
  `shouldEqual`. No bespoke harness, no manual exit-code
  bookkeeping, no `formatResult` helpers.

## DX Comparison vs Phase 5 Review

The Phase 5 review ran the same layered application under a
hand-rolled `launchAff_` harness that built a custom
`ScenarioResult` type, compared event sequences with `==`, and
exited the process with a thrown error on mismatch
(`spikes/phase-5-review/src/Spike/Phase5Review/Main.purs`). The
Phase 7 port replaces all of that with `Test.Spec` machinery.

| Concern                       | Phase 5 review                     | Phase 7 review                    |
| ----------------------------- | ---------------------------------- | --------------------------------- |
| Logger capture                | hand-built `Events = Ref (Array String)` plus a `push` helper threaded through every layer | `recording :: Aff (Recording String)` allocated per scenario, `rec.record` slotted directly into the logger record |
| Clock                         | counter-based `Ref Int` constructed inside `clockLayer` | `newTestClock` injected at the boundary, virtual time advanced explicitly when relevant |
| Per-scenario boilerplate      | `ScenarioResult` record, `formatResult`, `checkScenario*` functions, manual `==` and reason strings | one `it` or `itRIO_` body with `shouldEqual` |
| Failure surface               | `liftEffect (throwException (error "phase-5 review failed"))` at the end of `main` | `runSpecAndExitProcess` does it; per-test failures highlighted in the reporter |
| Typed failure inspection      | hand-written `showDbConnect` + arm of a case in the harness | identical helper inside the spec body, but called from a plain `it` with `runRIO` and matched against `shouldEqual` |
| Lines of harness code         | `Main.purs`: 255 lines             | `Spec.purs` + `Main.purs`: 224 lines total, of which Main is 17 lines |

The line-count difference is not the headline; the difference
that matters is shape. The Phase 5 harness writes a tiny
testing framework inline (result type, formatter, exit handler,
event check functions). The Phase 7 harness writes test bodies
and lets `Test.Spec` be the framework. Adding a fifth scenario
in Phase 5 means three new functions (`runScenario`,
`checkScenario`, and an entry in `main`); in Phase 7 it means
one new `it` block.

## DX Observations

### DX-A: `recording` allocates `Aff Unit`-valued operations only.

`Recording a` records values of type `a`. Service operations
that return non-`Unit` values (`fetchUser :: Int -> Aff (Maybe
String)`) can't share a recording with `log :: String -> Aff
Unit` directly. In this spike that does not matter because only
the logger feeds the recording, but a future test that wants to
record database calls *and* return mock results would have to
build its own combinator. A `Recording (Aff a)` variant or a
small `recordingWith :: (a -> b) -> Aff (Recording a)` could
close that gap. Not blocking for v0.1.

### DX-B: `itRIO_` needs `e ~ ()`.

Scenarios B and C surface a typed-failure `Left` and inspect
it. `itRIO_` requires the program's error row to be `()`, so
those scenarios fall back to plain `it` + `runRIO`. This is
documented in `docs/07-testing.md` and is the right default
(otherwise `itRIO_` would need a `Show (Variant e)` constraint
the caller cannot always discharge), but it does mean a single
spec module mixes two patterns. A wrapper like

```purescript
itRIOEither
  :: forall r e
   . Show (Variant e)
  => String
  -> Record r
  -> RIO r e Unit
  -> Spec Unit
```

would let suites that *can* satisfy `Show (Variant e)` (the
common case for closed apps with small error rows) drop the
plain-`it` fallback. Worth considering for a v0.2 helper kit.

### DX-C: Forking a service-using program inside a spec body costs an inner `runRIO`.

Scenario D forks fibers from inside an `itRIO_` body, and each
forked fiber has to provide the layer locally because
`Aff.forkAff` works in `Aff`, not in `RIO`. The pattern works,
but the inner

```purescript
runRIO
  ( provideAll { clock: tc.clock }
      ( provideLayer (appLayer "postgres://demo/db" record)
          ( do ... )
      )
  )
```

block reads heavy. An `RIO`-native `fork` that closes over the
ambient environment (Phase 6 ships `fork`, but it's an `RIO`
combinator, not an `Aff` one) would let the same scenario read
as

```purescript
do
  _ <- fork (us.greetAfter (Milliseconds 100.0) 1)
  _ <- fork (us.greetAfter (Milliseconds 200.0) 2)
  ...
```

inside the same environment. The current shape is the
consequence of mixing `Aff.forkAff` with a program that owns
its own environment; if the test author lifts everything into
`RIO`, the forks are tidy. Not a `RIO` problem so much as a
"forking out of `RIO` into ambient `Aff`" problem.

## What This Does **Not** Validate

- `attempt`-style typed-failure capture inside an `itRIO_` body.
  PureScript-`spec` does not need it because `runRIO` handles
  the discrimination at the boundary, but a future `RIO`-native
  spec helper (DX-B) would.
- Cross-test isolation under concurrent runs. The Phase 6 review
  validated `fork`/`race`/`zipPar` under stress; this review's
  concurrency surface is the four forked fibers in scenario D.
- `purescript-quickcheck` generators driving `RIO` programs.
  Listed as out-of-scope in `docs/07-testing.md` and a v0.2
  candidate.
