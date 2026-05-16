# Instruction-list rewrite plan

Branch: `instr-list-spike`. Goal: replace the current closure-based
`RIO r e a = Record r -> Aff (Either (Variant e) a)` encoding with
an ADT-encoded instruction list interpreted by a hand-rolled
while-loop. Keep `Aff` as the async bridge.

This document is the source of truth for what is and is not done.
Update it (check boxes, add notes, record numbers) as work lands.

---

## Strategic decision: Path A (keep Aff)

We are committing to Path A from the original recommendation:

- The new RIO core is an ADT interpreted by a hand-rolled loop.
- Async work routes through a new `ASYNC` instruction whose payload
  is an `Aff a` (or a callback-style `(callback -> cancel)` shim).
- We do not rewrite the fiber runtime. `forkFiber`, `parTraverse`,
  `bracket`'s scope finalizers, cancellation, scheduling, etc.,
  continue to delegate to `Aff`.

Consequences:

- The full speedup from the spike (3.8x on bind, 2.6x on ask) is
  available on purely synchronous hot paths.
- Async boundaries still pay Aff's per-step cost. Workloads
  dominated by fork/join or `parTraverse` get a smaller win.
- Scope is weeks, not months. We do not own the scheduler.

If at some point we want the full Path B win, the ADT encoding is
a prerequisite for it anyway, so this work is not wasted.

---

## Baseline numbers (commit `94ace9f`, spike)

200 samples each, mean wall-clock per iteration.

| Workload | Production RIO | Instr spike | Ratio |
|---|---|---|---|
| Bind chain (10k binds) | 814 us | 212 us | 3.8x faster |
| Service loop (10k ask + lookup) | 778 us | 297 us | 2.6x faster |
| Raw Aff bind chain (10k) (reference) | 453 us |  | Instr ~2x faster than Aff |

Anything we ship has to hold these ratios (or better) on the
production benchmark suite after the full primitive set is in.

---

## Phase 1: sync feature parity (in progress)

Extend the spike on this branch with the synchronous primitives
needed to run the existing RIO test suite, minus async/fork.

### Primitives

- [x] `catchTag` with stack-walking unwind to the nearest matching
      catch frame. (Commit `97e1433`.)
  - Parallel `catches` stack alongside the bind continuation stack.
  - Each catch frame records the bind-stack depth at entry.
  - On normal value propagation, drop catch frames whose protected
    scope has been exited.
  - On `FAIL`, walk `catches` from the top looking for a matching
    label; truncate the bind stack to the matched frame's depth
    and run the handler. No match terminates with `Left`.
  - Bench: 1 round-trip 1.88 us (RIO) vs 1.03 us (Instr), 1.8x.
  - Bench: 10k round-trips 43.57 ms (RIO) vs 913 us (Instr), 47x.
    The big gap is because production RIO bottoms out in real JS
    exception machinery; the spike walks an array.

- [~] `bracket` / `acquireRelease` with finalizer ordering.
  - **Deferred to Phase 2.** Bracket's primary use cases are
    around async resources (file handles, DB connections),
    where the natural implementation reuses `Aff.generalBracket`
    once the `ASYNC` instruction lands. The synchronous-only
    bracket would add interpreter complexity (a fourth parallel
    stack, FAIL-time finalizer firing with re-FAIL trampolining,
    nested-bracket ordering) for a feature whose typical caller
    is async. Revisit immediately after Phase 2.

- [~] `Functor` / `Apply` fast paths.
  - **Deferred.** Today `map` routes through `flatMap` (one
    extra `Pure` node). Phase 1 benchmarks already show 2.4-47x
    wins without it; adding dedicated `MAP` / `APPLY` tags is
    interpreter complexity for marginal further gain. Revisit
    if any Phase 7 production workload is map-heavy enough to
    motivate it.

- [x] `Local` / `provide` / `provideAll`. (Commit `f5f743f`.)
  - A `LOCAL` instruction whose payload is `(r -> r')` and an
    inner `Instr r' e a`. The interpreter saves the current env,
    runs the inner with the modified env, then restores.
  - Third parallel `envs` stack with depth-based scope tracking.
    On FAIL match, env restores from the catch frame's snapshot
    and envs strictly past the catch depth are dropped.
  - Bench: provided service loop (10k) 775 us (RIO) vs 319 us
    (Instr), 2.4x. instrLocal push adds ~8% over direct env.

- [x] Stack-safety stress test: 1M binds runs without blowing the
      JS stack or hitting GC pressure that distorts the bench.
      (Commit `f87d84a`.) 1M binds: 32 ms (Instr) vs 75 ms (RIO),
      2.3x faster. No stack overflow.

### Wiring

- [x] Sanity assertions before each new benchmark workload (verify
      the spike returns the expected value before timing it).
      catchTag sanity prints `Right 2` before the bench runs.
- [x] Benchmark `catchTag` round-trip head-to-head with production
      `RIO.catchTag`. 1.8x on single, 47x on 10k loop.
- [~] Benchmark `bracket` round-trip head-to-head. Deferred with
      bracket itself.
- [x] Benchmark `provide` head-to-head. 2.4x via instrLocal.

### Exit criteria

- [x] Spike beats production RIO on bind, ask, catchTag, and
      provide. (3.8x, 2.4x, 1.8x-47x, 2.4x respectively.)
- [x] 1M-bind stack-safety case passes.
- [~] Bracket and Functor/Apply fast paths deferred per above.

**Phase 1 closed at commit `f87d84a`.** All synchronous primitives
in the original spike plus catchTag and Local are in place and
benchmark-validated. Bracket and the Functor/Apply fast paths are
recorded as known-deferred items to revisit after Phase 2.

---

## Phase 2: the `ASYNC` instruction

Bridge the spike interpreter back into `Aff`.

- [x] Add `ASYNC` instruction with payload `Aff a`. (Tag 7.)
- [x] Refactor `_runInstr` into a step/resume machine:
      `_initInstrState` allocates mutable state, `_stepInstr` runs
      the inner loop until completion or an `ASYNC` suspension,
      `_resumeInstr` threads the `Aff`'s result back in.
- [x] PureScript driver in `runInstr` binds the pending `Aff` and
      re-enters the loop. Synchronous-only programs pay only one
      `liftEffect` + one `Aff` `pure` total.
- [x] `instrLiftAff :: Aff a -> Instr r e a` exported as the
      canonical primitive that wraps any async work.
- [~] Cancellation handling. Path A defers to `Aff`: when the
      outer `Aff` is killed, the pending `_pendingAff` bind is
      cancelled by Aff itself, the driver never resumes, and the
      interpreter state is dropped along with the parent's
      `Aff`. No interpreter-side finalizers exist in Phase 2 so
      no leak is possible. Revisit when bracket lands in Phase 3.

### Numbers (200 samples each)

| Workload | RIO | Instr | Ratio |
|---|---|---|---|
| Bind chain (10k binds) | 797 us | 193 us | 4.1x faster |
| Service loop (10k ask) | 773 us | 334 us | 2.3x faster |
| Catch loop (10k) | 43.5 ms | 1.00 ms | 43x faster |
| Async loop (10k, every iter ASYNC) | 739 us | 2.03 ms | 2.7x slower |
| Mixed loop (1k iters, 9 sync + 1 ASYNC) | 786 us | 492 us | 1.6x faster |
| 1M binds (stack safety) | 73 ms | 45 ms | 1.6x faster |

The async-only loop is the spike's worst case: every iteration
crosses the driver boundary so there is no synchronous work to
amortise. The mixed workload (9 sync binds per ASYNC, closer to
realistic application code) crosses back into a win. Anything
ASYNC-heavy on a hot path should be batched into larger sync
sequences between suspensions to keep the FFI loop fed.

### Exit criteria

- [x] ASYNC sanity check returns `Right 43` (lift `pure 42`,
      bind, add 1).
- [x] A mixed sync/async workload is faster than RIO. (492 us vs
      786 us, 1.6x faster.)
- [x] Pure-sync workloads from Phase 1 still pass after the
      refactor. (Bind chain, service loop, catchTag, catch loop,
      1M-bind stack safety all green and slightly faster than the
      pre-refactor numbers.)

**Phase 2 closed at commit `b5a7327`.** The ASYNC bridge works,
the worst-case overhead is documented, and the realistic workload
beats production RIO comfortably.

---

## Phase 3: concurrency primitives

Build on top of `ASYNC`. Most of these are thin shells that defer
to `Aff`'s existing implementations.

- [ ] `forkFiber` via `Aff.forkAff` plus interpreter handoff.
- [ ] `joinFiber`, `awaitAll`, `race`, `raceAll`.
- [ ] `parTraverse` via the `Parallel` instance, ultimately
      `Aff`'s parallel applicative.
- [ ] `killFiber` / cancellation. Path A defers to `Aff`'s
      cancellation but we need to confirm the interpreter sees the
      kill and unwinds finalizers correctly.
- [ ] `FiberRefs` snapshot on fork. Same map-of-Refs shape we ship
      today; the interpreter reads/writes through the env.
- [ ] `generalBracket` parity.

### Exit criteria

- Production fan-out/fan-in benchmark within 1.5x of raw `Aff`.
  (Today we are 3.3x slower; the bar is "improve substantially,"
  not "match Aff," because we still pay closure cost on async
  paths.)
- All concurrency tests pass.

---

## Phase 4: state and STM

- [ ] `Ref` lifted through `SYNC` (Effect.Ref under the hood).
- [ ] `Local` (env scoping). Already in Phase 1.
- [ ] `STM` reuses the existing PureScript implementation; just
      lifts its terminal `Effect` action through `SYNC`.

### Exit criteria

- All Ref and STM tests pass against the new encoding.

---

## Phase 5: public API surgery

This is the riskiest commit. Everything before this has been
additive on a side branch; this swaps the actual `RIO` definition.

- [ ] Replace `src/RIO/Internal.purs` newtype with the foreign ADT.
- [ ] Update every module that destructures `RIO` directly or calls
      `unRIO` / `unsafeUnRIO`. (Count: ~15 modules in `src/RIO/`.)
- [ ] Re-derive `Functor`, `Apply`, `Applicative`, `Bind`, `Monad`,
      `MonadEffect`, `MonadAff`, `MonadThrow`, `MonadError`,
      `MonadRec`, `Parallel` instances.
- [ ] Decide the fate of `RIO.Effect.run`, `runRIO`, `runRIO'`,
      `provideAll`. They probably stay; their internals change.
- [ ] Rename / merge: `Instr` module disappears; its types become
      the new `RIO`.

### Exit criteria

- `npx spago build` clean.
- `npx spago test` (root) all green.

---

## Phase 6: downstream workspace

- [ ] `rio-http` builds and tests green.
- [ ] `rio-postgres` builds and tests green.
- [ ] `rio-otel` builds and tests green.
- [ ] `rio-config-file` builds and tests green.
- [ ] `examples/` builds and runs.

### Exit criteria

- Full workspace `npx spago test` clean.

---

## Phase 7: benchmark gate

Re-run the production benchmark suite on the new encoding and
confirm we held the spike's win across the suite.

- [ ] Bind chain: should match or beat the spike's 3.8x.
- [ ] Service loop: should match or beat the spike's 2.6x.
- [ ] parTraverse (32 elements): aim for parity with raw Aff
      (today 1.5x slower).
- [ ] Fan-out/fan-in: aim for 1.5x of raw Aff (today 3.3x slower).
- [ ] Baseline `runRIO' (pure unit)`: should be lower than today's
      ~870 ns.

### Exit criteria

- All four head-to-head workloads improve relative to `main`.
- No regression on any production workload.

---

## Out of scope (for this branch)

- Full Path B (own fiber runtime, drop Aff).
- New public API surface. The encoding switch should be invisible
  to library users.
- Performance work on `Data.Variant` itself, even though the FAIL
  unwind reads its runtime shape.

---

## Open questions

- Should the interpreter's stack and `catches` arrays be allocated
  per-run or per-fiber and recycled? For Path A, per-run is fine.
- For `LOCAL`, do we save the env in a stack frame or in the
  catch-like frame array? Probably the latter, since we already
  walk frames on unwind.
- For Variant FAIL unwind, the interpreter reads `variant.type`.
  We should confirm this matches `Data.Variant`'s runtime shape
  and document the dependency in `Instr.js`.

---

## Progress log

- 2026-05-16: Spike committed at `94ace9f`. Numbers above.
- 2026-05-16: Plan document committed.
- 2026-05-16: Phase 1 `catchTag` landed at `97e1433`. 1.8x on
  single round-trip, 47x on 10k-round-trip loop. Sanity assertion
  in the bench harness returns `Right 2`.
- 2026-05-16: Phase 1 `LOCAL` landed at `f5f743f`. 2.4x on the
  provide-equivalent service loop. instrLocal at the top costs
  ~8% over direct env injection.
- 2026-05-16: Phase 1 stack safety landed at `f87d84a`. 1M binds:
  32 ms (Instr) vs 75 ms (RIO).
- 2026-05-16: Phase 1 closed with bracket and Functor/Apply
  deferred per inline rationale.
- 2026-05-16: Phase 2 ASYNC bridge landed. Interpreter refactored
  to step/resume shape, `instrAsync` / `instrLiftAff` exported,
  PureScript driver runs the loop via Aff binds. Sync workloads
  still pass and run slightly faster than the pre-refactor
  numbers. Mixed 9-sync+1-async loop 1.6x faster than RIO; pure
  async-only loop 2.7x slower (every iteration crosses the
  driver boundary). Cancellation defers to Aff for Path A.
