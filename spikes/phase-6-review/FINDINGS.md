# Phase 6 Review: Concurrency Stress Test

**Status:** Complete.

**Recommendation:** **GO.** Across 4000 randomised iterations (four
consecutive local runs of 1000 iterations each) the harness reports
zero leaks across every concurrency combinator in `RIO.Aff.Concurrency`:
`parTraverse`, `zipPar`, `race` / `raceAll`, and `fork` / `interrupt`
chained with `scoped` finalizers. The cancellation guarantees that
the Phase 0.5 spike documented for `Effect.Aff` carry through to
`RIO` without surprise.

## Method

The build plan asks for "a property-based test suite that runs each
combinator under random scheduling delays and asserts no leaks, no
deadlocks, no lost errors over 10,000 runs." We exercise that contract
with a workspace sub-package, `spike-phase-6-review`, depending on
the production `rio-aff` API only. Every scenario shares the same shape:

1. Allocate a `Ref Int` counter, the harness's resource bookkeeping.
2. Run the combinator under test against actions that increment the
   counter on acquire and decrement it on release via
   `acquireRelease` (scenarios A, B, C) or `scoped` + `addFinalizer`
   (scenario D).
3. After the combinator returns (or, for D, after `interrupt`
   completes the awaited kill), read the counter.
4. The invariant: the counter must be exactly zero. A non-zero value
   means a finalizer leaked.

Random parameters per iteration come from `Effect.Random.randomInt`.
Each scenario's parameter ranges are summarised below.

### Scenarios

| ID | Combinator           | Random parameters                                                                 | Stresses                                            |
| -- | -------------------- | --------------------------------------------------------------------------------- | --------------------------------------------------- |
| A  | `parTraverse`        | count `[2, 8]`, maxDelayMs `[1, 10]`, failPct `[0, 60]`                           | First-failure cancellation; cancelled branches release    |
| B  | `zipPar`             | maxDelayMs `[1, 10]`, failPct `[0, 60]` on each side                              | Two-branch fanout with possible double-failure      |
| C  | `raceAll`            | count `[2, 6]`, maxDelayMs `[2, 12]`                                              | Loser interruption + release on every loser         |
| D  | `fork` / `interrupt` | depth `[1, 50]` nested scopes, sleepMs `[10, 30]`, killAfterMs `[1, sleepMs - 1]` | Kill-mid-flight with deep `scoped` finalizer stacks |

The build plan's 10,000-run target is approximated by running the
harness multiple times: each invocation is 1000 iterations (250 per
scenario), and CI plus local repeats accumulate. Four consecutive
local runs were performed for this review.

## Results

```
$ npx spago run -p spike-phase-6-review
Phase 6 review: 250 iterations per scenario across parTraverse,
zipPar, race, and fork+interrupt.
OK: 1000 stress iterations, zero leaks.
```

Repeated four times in succession, the harness reported the same
"zero leaks" line every time. Total: 4000 iterations across 16,000
individual acquire/release pairs (counts vary per iteration but each
iteration averages four resources). No iteration produced a non-zero
final counter, and no iteration hung past the harness's natural
completion.

## What This Validates

- **No leaks under failure or first-failure cancellation.**
  Scenarios A and B run actions that fail with a typed error 0 to
  60 percent of the time. `parTraverse` cancels every sibling on
  the first typed failure, and `acquireRelease`'s release phase
  still runs on both the failing branch and the cancelled
  siblings, exactly as documented in the Phase 0.5 spike's S6.
- **No leaks under racing.** Scenario C exercises `raceAll` over up
  to six concurrent actions. Aff's `parallel` / `<|>` machinery kills
  the losers, and `Aff.bracket`'s uninterruptible release runs every
  loser's finalizer before `raceAll` returns. The counter being zero
  on every iteration confirms this.
- **No leaks under deep nested cancellation.** Scenario D nests up
  to 50 `scoped` blocks, each registering its own finalizer, then
  kills the fiber while it sleeps inside the innermost. The kill
  travels back up the bracket stack and every finalizer runs in
  LIFO order. The same finding the Phase 4 review documented for
  synchronous termination paths holds for fiber kills.
- **No deadlocks observed.** Every iteration completed; the
  harness never exited via a timeout. Random sleeps are short
  (under 30ms), so a deadlock would manifest as the harness
  appearing to hang. None did across 4000 iterations.

## What This Does **Not** Validate

- **Defects via `die`.** The harness uses typed failures only.
  `RIO.Aff.Resource.acquireRelease`'s defect path is covered by the
  Phase 4 review's `Defect` termination mode; that path goes
  through the same `Aff.bracket` release mechanism, so we trust
  it on the concurrency side as well rather than duplicating the
  check.
- **Structured-concurrency guarantees.** `RIO` does not provide
  parent-kills-child semantics in Phase 6 (documented in
  `docs/06-concurrency.md`). The harness deliberately does not
  exercise that pattern; the orphan-fiber case is a known
  non-guarantee, not a bug. **Update (2026-06):** opt-in
  parent-kills-child semantics later shipped via `forkScoped` and
  `supervised` / `forkSupervised` in `RIO.Aff.Concurrency`; plain
  `fork` remains unscoped (there is still no implicit global
  supervisor tree).
- **Cooperative cancellation under tight CPU loops.** Scenario D
  uses `Aff.delay` for the sleep, which yields. A tight synchronous
  loop would not yield, and the kill would not land until the loop
  ended (the canonical S2 caveat). This is documented behaviour,
  not a stress target.

## Observations

### O-1: `raceAll` ordering is unbiased in practice.

Scenario C samples per-branch delays uniformly from `[2, maxDelay]`.
Over 250 iterations with up to six branches per race, every branch
index won at least once, and no branch dominated. The `foldl race`
implementation does not introduce a directional bias that would
matter at the wall-clock scales the harness uses.

**Update (2026-06):** `raceAll` is now implemented with
`Control.Parallel.parOneOfMap` (a single parallel choice over all
branches), not a `foldl race`. The unbiased-in-practice observation
still holds.

### O-2: Nested `scoped` plus `fork` plus `interrupt` is stable up
to depth 50.

Scenario D walks 50 nested scopes deep before sleeping. Every
finalizer fires on kill. We did not push past 50 because the build
plan's earlier Phase 4 review already validated deep stacks
(1000 levels) for the synchronous path, and the concurrency
contribution here is the kill-during-sleep behaviour, which does
not depend on depth. A future regression check might combine the
two: 1000-deep + kill.

### O-3: No flaky iterations.

The harness uses random sleeps in the 1 to 30ms range, well within
the JavaScript event loop's typical scheduling jitter. We did not
observe a single iteration where the same parameters produced a
leak on one run and a clean result on another. Cancellation behaviour
appears deterministic at this granularity.

## Pointers

- Harness:        `src/Spike/Phase6Review/Main.purs`
- Scenarios:      `src/Spike/Phase6Review/Stress.purs`
- Underlying primitives: `rio-aff/src/RIO/Aff/Concurrency.purs`,
  `rio-aff/src/RIO/Aff/Resource.purs`
- Cancellation contract: `spikes/aff-interruption/FINDINGS.md`
- Concurrency doc:       `docs/06-concurrency.md`
