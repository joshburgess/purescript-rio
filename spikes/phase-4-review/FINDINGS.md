# Phase 4 Review: Resource-Safety Stress Test

**Status:** Complete.

**Recommendation:** **GO.** Bracket-style resource safety in
`RIO.Aff.Resource` survives 1000-deep nested scopes under every
termination path the library promises: success, typed failure,
defect, and external `Aff` fiber kill. Over 100 randomized runs per
invocation, repeated four times locally, the finalizer log was
byte-identical to the expected LIFO sequence in every case.

## Method

A workspace sub-package, `spike-phase-4-review`, depends on the real
`rio-aff` package and exposes a single `main` that exercises two
scenarios:

1. **Deep nested scopes.** Recursively open `target = 1000` scopes;
   at each depth `k` push `register-k` into a shared event log and
   `addFinalizer` an action that pushes `finalize-k`. Pick a random
   depth `failAt` and a random termination mode (`Succeed`,
   `TypedFail`, `Defect`) and stop there. The expected event log is

   ```
   register-0, register-1, ..., register-failAt,
   finalize-failAt, finalize-(failAt-1), ..., finalize-0
   ```

   The harness asserts the two collected index sequences are exactly
   `[0..failAt]` and its reverse.

2. **Kill mid-flight.** Same nested structure as above, but instead
   of failing the innermost body sleeps for a random duration. The
   outer harness forks the program, waits a random interval, then
   calls `killFiber`. The expected log is `register-0 .. register-(target-1)`
   followed by `finalize-(target-1) .. finalize-0`. (The "expected
   register length matches actual" check would in principle also
   catch a kill that lands before all 1000 registers complete, but
   in practice the entire register path runs in a tight synchronous
   tail before the `Aff.delay`, so the kill always arrives at the
   sleep.)

Counts per invocation:

- 50 deep iterations with `target = 1000`, randomized termination
  and `failAt` uniform in `[0, 999]`.
- 50 kill iterations with `target = 1000`, random sleep in `[5, 39] ms`
  and random kill delay in `[1, 36] ms`.

The harness aggregates failures and exits non-zero on the first
violation. It is run from CI on every PR.

## Results

Four consecutive local invocations on the developer laptop:

```
$ npx spago run -p spike-phase-4-review
Phase 4 review: 50 deep-nested runs at depth 1000, then 50 kill runs.
OK: 100 stress runs, zero leaks.
```

400 stress runs total, zero leaks, zero LIFO violations, no flakiness.
Wall time per invocation: ~2.4 s including npm and spago startup;
the actual stress work is well under a second.

## What This Validates

- `acquireRelease` and `scoped` honour the release-runs-on-every-path
  contract documented in `rio-aff/src/RIO/Aff/Resource.purs`. The contract holds
  at depth 1000, not just the depths used by the unit tests in
  `test/Test/RIO/ResourceSpec.purs`.
- The `Effect.Aff.bracket` foundation's uninterruptible release phase
  (Phase 0.5 spike, scenario S6) propagates correctly through
  recursive `scoped` calls. A `killFiber` at any depth still drains
  every registered finalizer in LIFO order.
- The polymorphic `forall r. RIO r e a` shape used by the recursive
  `loop` and `sleepLoop` helpers in the harness is the natural way to
  recurse into a `scoped` block. Each recursive call instantiates
  `r` to `(scope :: Scope | r_outer)`, which matches the row required
  by the `scoped` argument; no per-level annotation is needed.

## What This Does **Not** Validate

- Finalizer aggregation behaviour. The current Phase 4.2 implementation
  swallows exceptions thrown from individual finalizers (so one bad
  finalizer cannot prevent the rest from running). Reifying or
  aggregating those exceptions is deferred to a later phase; the
  stress harness deliberately does not register any throwing
  finalizers.
- Heap growth across iterations. Each iteration uses a fresh `Ref`
  and the test process exits at the end, so we are not measuring
  steady-state memory.

## DX Observations

- The recursive helpers needed to be polymorphic in `r` so the inner
  `scoped` block could see the `scope` service while the outer call
  site sees a plain `r`. This was a one-line change and the compiler
  message was clear (`Could not match () with (scope :: Scope | t0)`).
  Worth noting in the eventual concurrency / layers documentation as
  the canonical shape for recursive scope helpers.
- `unsafeRunRIO` is the right tool for the harness's empty
  environment, but the type ascription on the `RIO` term inside the
  call was needed for the variant-tagged stress error to elaborate.
  `runRIO` would have worked just as well; `unsafeRunRIO` was chosen
  only to skirt the `Either`-wrapping for cleanliness.

## Pointers

- Harness: `src/Spike/Phase4Review/Main.purs`
- Recursive program: `src/Spike/Phase4Review/Stress.purs`
- Underlying primitives: `rio-aff/src/RIO/Aff/Resource.purs`
- Cancellation contract source of truth: `spikes/aff-interruption/FINDINGS.md`
  scenario S6.
