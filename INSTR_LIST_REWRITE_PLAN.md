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

- [ ] `catchTag` with stack-walking unwind to the nearest matching
      catch frame.
  - Parallel `catches` stack alongside the bind continuation stack.
  - Each catch frame records the bind-stack depth at entry.
  - On normal value propagation, drop catch frames whose protected
    scope has been exited.
  - On `FAIL`, walk `catches` from the top looking for a matching
    label; truncate the bind stack to the matched frame's depth
    and run the handler. No match terminates with `Left`.

- [ ] `bracket` / `acquireRelease` with finalizer ordering.
  - Finalizers run in reverse acquisition order, both on success
    and on failure.
  - Needs a `FINALIZER` frame shape on the unified stack, or a
    third parallel stack.
  - Defer the `Scope`-typed variant until we wire `Aff` back in.

- [ ] `Functor` / `Apply` fast paths.
  - Today `map` routes through `flatMap` (one extra `Pure` node).
    A dedicated `MAP` instruction skips that allocation.
  - Same for `Apply`. Likely small wins. Worth measuring before
    committing to the extra interpreter complexity.

- [ ] `Local` / `provide` / `provideAll`.
  - A `LOCAL` instruction whose payload is `(r -> r')` and an
    inner `Instr r' e a`. The interpreter saves the current env,
    runs the inner with the modified env, then restores.

- [ ] Stack-safety stress test: 1M binds runs without blowing the
      JS stack or hitting GC pressure that distorts the bench.

### Wiring

- [ ] Sanity assertions before each new benchmark workload (verify
      the spike returns the expected value before timing it).
- [ ] Benchmark `catchTag` round-trip head-to-head with production
      `RIO.catchTag`.
- [ ] Benchmark `bracket` round-trip head-to-head with production
      `RIO.bracket` (sync acquire and release).
- [ ] Benchmark `provide` head-to-head.

### Exit criteria

- All Phase 1 primitives implemented and benchmarked.
- Spike beats production RIO on at least bind, ask, catchTag, and
  provide. Bracket can be neutral.
- 1M-bind stack-safety case passes.

---

## Phase 2: the `ASYNC` instruction

Bridge the spike interpreter back into `Aff`.

- [ ] Add `ASYNC` instruction with payload `Aff a`.
- [ ] Interpreter on `ASYNC`: exit the loop, run the `Aff` via
      `Aff.runAff`, on completion re-enter the loop with the
      result threaded into the next bind continuation.
- [ ] `liftAff :: Aff a -> Instr r e a` becomes the canonical
      primitive that wraps any async work.
- [ ] Decide cancellation handling: if the outer `Aff` is killed
      while inside an `ASYNC`, what does the interpreter do? For
      Path A the answer is "Aff handles it, interpreter never
      resumes," but we have to make sure no finalizer state leaks.

### Exit criteria

- A workload that interleaves sync binds and `liftAff` calls runs
  correctly and is no slower than the production RIO equivalent
  on the same workload.

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
