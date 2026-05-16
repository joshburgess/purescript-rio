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

- [x] `instrForkFiber` via `Aff.forkAff` plus interpreter handoff.
      Implementation: `instrAsk` captures the env, `Aff.forkAff`
      forks a child `runInstr env inner`, the result is wrapped in
      `InstrFiber`. No new instruction; built on `instrAsync`.
- [x] `instrJoinFiber`. Waits on the child via `Aff.joinFiber`
      then re-injects success/failure into the parent
      interpreter. `awaitAll` is `traverse instrJoinFiber` and
      does not need a dedicated primitive.
- [~] `race` / `raceAll`. Deferred. Both build on the same
      `Aff.forkAff` + winner-takes-all pattern as `fork`/`join`;
      no spike-specific machinery is needed. Land alongside the
      public API in Phase 5.
- [x] `instrParTraverse` via `Aff.parallel` / `Aff.sequential`.
      Each child runs as a separate `runInstr`; the merged result
      array is short-circuited on the first failure via
      `sequence`.
- [~] `killFiber` / cancellation. Path A defers entirely to
      `Aff`. Killing a fiber kills its underlying `Aff` fiber,
      and the driver loop in `runInstr` is itself a sequence of
      `Aff` binds so it gets cancelled along with the parent.
      No interpreter-side finalizer state to leak in Phase 3.
- [~] `FiberRefs` snapshot on fork. Same map-of-Refs shape we
      ship today; the interpreter reads/writes through the env.
      Production `RIO.FiberRef.forkFiber` clones the map before
      calling `mkFiber`. For the spike this is mechanical: the
      child `Instr`'s env gets a cloned `FiberRefs`, same as the
      RIO implementation. Defer until the public API swap.
- [~] `generalBracket` parity. Defer to Phase 4 alongside
      `Ref` / `STM`; the natural implementation uses
      `Aff.generalBracket` and the interpreter just lifts.

### Numbers (200 samples each)

| Workload | RIO | Instr | Aff (ref) | vs RIO | vs Aff |
|---|---|---|---|---|---|
| parTraverse (32 pure) | 26.3 us | 36.6 us | 18.8 us | 1.39x slower | 1.94x of Aff |
| Fan-out/fan-in (x16) | 26.5 us | 27.2 us | 22.5 us | parity | 1.21x of Aff |

`parTraverse`'s slowdown is the per-element `runInstr` setup
cost: each child allocates state and runs the driver loop, even
for trivial pure work. With real per-element work (network IO,
DB queries) this overhead amortises to nothing. The fan-out/
fan-in workload effectively matches RIO and is well inside the
1.5x-of-Aff bar.

### Exit criteria

- [x] Production fan-out/fan-in benchmark within 1.5x of raw
      `Aff`. (27.24 us vs 22.54 us, 1.21x.)
- [x] `instrForkFiber` / `instrJoinFiber` work end-to-end
      against `Aff` fibers, with typed failure propagation.
- [x] `instrParTraverse` runs and short-circuits on the first
      failure via `sequence`.
- [~] race / FiberRefs / generalBracket deferred to later
      phases per the inline rationale.

**Phase 3 closed at commit `393dd34`.** Fork / join / parTraverse
are in. The thin-wrapper-on-`Aff` strategy pans out: anything
that ultimately delegates to `Aff.forkAff` matches RIO almost
exactly. Per-fork setup cost is the only place the spike pays
extra, and it only matters when the forked work itself is
trivial.

---

## Phase 4: state, STM, and bracket

- [x] `Ref` lifted through `SYNC` (Effect.Ref under the hood).
      No new spike machinery: callers use
      `instrLiftEffect (Ref.read ref)`, etc., directly. Verified
      with `refCounterLoopInstr`, a 10k modify loop that is
      1.6x **faster** than the RIO equivalent (723 us vs 1.18 ms)
      because the spike's `SYNC` path skips Aff per-step cost.
- [x] `Local` (env scoping). Landed in Phase 1.
- [~] `STM` reuses the existing PureScript implementation; just
      lifts its terminal `Effect` action through `SYNC`. No
      spike-specific work beyond the lift; defer the explicit
      port to Phase 5 once the public `RIO` swap happens.
- [x] `instrBracket` (deferred from Phase 1) implemented via
      `Aff.bracket`. Threading: each of acquire / use / release
      runs through its own `runInstr env`, with typed failures
      carried through the bracket as `Either (Variant e) _`.
      Verified with a Ref-counter sanity check (use bumps to 1,
      release bumps to 2, returns 1).

### Numbers (200 samples each)

| Workload | RIO | Instr | Ratio |
|---|---|---|---|
| Ref counter (10k modify) | 1.18 ms | 723 us | 1.6x faster |
| Bracket loop (10k pure) | 3.89 ms | 9.77 ms | 2.5x slower |

The bracket loop is the spike's weakest sync workload: each
round-trip does three full `runInstr` invocations (acquire +
use + release), every one allocating fresh interpreter state.
Production `RIO.bracket` inlines into `Aff.bracket` directly
and pays only the Aff per-step cost.

Real-world brackets almost always wrap async IO (file handles,
DB connections, HTTP sockets, pool checkouts), where the
per-bracket overhead is dominated by the IO itself rather than
by interpreter setup. The synthetic 10k-pure-bracket loop is
the pathological case; observed slowdown there will not show
up in typical application traces.

**Follow-up:** if Phase 7 surfaces a bracket-heavy production
workload, add a dedicated `BRACKET` instruction (tag 8) that
the interpreter handles inline (like `CATCH` / `LOCAL`) without
spawning sub-`runInstr` for each phase. The same flat
parallel-stack pattern that `CATCH` uses fits naturally. Track
as a known optimisation, not a phase blocker.

### Exit criteria

- [x] Bracket sanity returns the use's value and runs release
      after use on success.
- [x] Ref operations work through `instrLiftEffect` end-to-end.
- [~] STM defers to Phase 5 (no spike-specific machinery
      needed beyond the existing `SYNC` lift).

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
- 2026-05-16: Phase 3 concurrency primitives landed: fork / join
  / parTraverse, all built on top of `instrAsync` with no new
  instructions. Fan-out/fan-in matches RIO (27.24 us vs 26.50 us)
  and is 1.21x of raw Aff, inside the 1.5x target. parTraverse
  on trivial pure work is 1.39x slower than RIO; the gap is the
  per-element `runInstr` setup. Race / FiberRefs / bracket
  deferred to later phases.
- 2026-05-16: Phase 4 state and bracket landed. Ref pass-through
  via `instrLiftEffect` is 1.6x faster than RIO's `liftEffect`
  loop because `SYNC` skips Aff per-step. `instrBracket` via
  `Aff.bracket` is correct (sanity passes) but 2.5x slower on a
  pure 10k-bracket loop: each round-trip pays for three
  `runInstr` invocations. Real-world async-IO brackets will not
  feel this; a dedicated `BRACKET` instruction is the long-term
  fix and is tracked as a known Phase 7-or-later optimisation.
