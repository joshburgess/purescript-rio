# Spike 0.5: Aff Cancellation and Interruption Semantics

**Status:** Complete.

**Recommendation:** **GO.** PureScript's `Aff` provides every cancellation
guarantee RIO needs for ZIO-style `fork`/`interrupt`/`acquireRelease` in
Phase 6 and resource safety in Phase 4. One well-defined caveat
(cooperative cancellation does not preempt CPU-bound work without an async
boundary) is identified and has a known mitigation. **No custom runtime
layer above `Aff` is required.**

## Method

A small harness in `src/Spike/AffInterruption/Main.purs` exercises seven
scenarios against `Effect.Aff`'s `forkAff`, `killFiber`, `joinFiber`,
`bracket`, and `attempt`. Run with:

```sh
npx spago run -p spike-aff-interruption
```

The harness prints what happened at each step; outcomes are interpreted
below. Repeated runs produce identical outcomes.

## Scenarios and Results

### S1: interrupt during a long sleep

Fork an Aff that sleeps 5 seconds, kill it after 50ms, then attempt to join.

```
elapsed=51.0ms
join after kill: threw, message=S1-interrupt
```

**Result: PASS.** Kill takes effect promptly (within ~1ms of the wait
ending). Joining a killed fiber raises the kill exception via `attempt`,
which is exactly the semantics RIO wants for `Fiber.join`.

### S2: tight CPU loop, no yield points

Fork 1,000,000 monadic `Ref.modify_` increments with no async boundary,
kill it after `delay 0`, observe the counter.

```
counter after kill: 1000000 (of 1000000)
ran to completion -> no interruption
```

**Result: GAP IDENTIFIED.** Aff's interpreter does not preempt
synchronous bind chains. The kill is queued on the event loop but the
fiber's tight loop runs to completion in a single JS turn. **This is
the classic cooperative-cancellation limitation that also exists in
ZIO and Effect-TS.**

### S2b: same loop, with `delay 0` every 100 iterations

```
counter after kill: 5301 (of 1000000)
stopped early -> yields make loops interruptible
```

**Result: PASS.** When the loop yields periodically (even with a
zero-duration delay), the kill lands. Yielding every 100 iterations
caused the kill to take effect after ~5300 iterations (about 53
yield points), well within reasonable bounds.

**Implication for RIO:** We should expose a `yield :: RIO r e Unit`
primitive (implemented as `liftAff (delay (Milliseconds 0.0))`) and
document its use in CPU-bound code. This is the same pattern ZIO
recommends with `ZIO.yieldNow`.

### S3: bracket release runs when fiber is killed mid-use

Acquire a resource, write a "used" flag, sleep 5 seconds, then release.
Kill the fiber 50ms in.

```
acquired=true used=true released=true
PASS: release ran
```

**Result: PASS.** `Aff.bracket` honours the release path on
cancellation. This is the load-bearing guarantee for Phase 4's
`acquireRelease` and `Scope` primitives.

### S4: kill of a fiber that has already completed

Fork `pure 42`, join (so it completes), kill, join again.

```
first join=42
second join after late-kill: returned 42
```

**Result: PASS.** Killing a completed fiber is a no-op. The fiber's
result is still observable via subsequent joins. This means RIO's
`interrupt` does not need to guard against double-completion.

### S5: kill of a fiber that has already been killed

Fork a 5-second sleep, kill twice in succession, join.

```
join: threw S5-kill-1
(double kill survived without crashing the runtime)
```

**Result: PASS.** The first kill's exception wins; the second is
silently absorbed. RIO's `interrupt` is idempotent by virtue of
`Aff.killFiber`'s semantics.

### S6: kill during a release phase

Bracket whose release itself sleeps 200ms. Kill the fiber mid-use,
then issue a second kill 50ms into the release.

```
after kill+50ms: releaseStarted=true
releaseFinished=true
PASS: release was uninterruptible
```

**Result: PASS.** Aff treats the bracket's release as uninterruptible
by default. The second kill, issued while the release was running,
did not stop it. This is precisely the semantics ZIO calls
"uninterruptible finalizers" and is what makes `acquireRelease`
safe under concurrent cancellation.

## Summary

| Property                                                       | Status  | Notes |
|----------------------------------------------------------------|---------|-------|
| Interrupt fires at async boundaries                            | PASS    | S1    |
| Tight CPU bind chains are not interruptible                    | GAP*    | S2    |
| Explicit `delay 0` makes loops interruptible                   | PASS    | S2b   |
| `bracket` release runs on kill                                 | PASS    | S3    |
| Kill of completed fiber is a no-op                             | PASS    | S4    |
| Double kill is idempotent                                      | PASS    | S5    |
| Release/finalizer phase is uninterruptible by default          | PASS    | S6    |

\* The "gap" is the standard cooperative-cancellation property shared with
ZIO and Effect-TS. It is not a defect; it is a property to document and to
provide a `yield` escape hatch for.

## Decisions Feeding Phase 4 and Phase 6

1. **RIO's concurrency layer builds directly on `Aff`'s primitives.** No
   custom runtime wrapper is needed. `fork`, `join`, `interrupt`,
   `acquireRelease`, and `scoped` can be defined in terms of `forkAff`,
   `joinFiber`, `killFiber`, and `bracket`.

2. **Add `yield :: forall r e. RIO r e Unit`** as a public primitive,
   implemented as `liftAff (delay (Milliseconds 0.0))`. Document its use
   in CPU-bound sections. This goes into Phase 6.

3. **`acquireRelease` (Phase 4.1)** maps to `Aff.bracket` directly.
   Release runs on success, on typed failure (propagated through the
   `Either` channel), and on kill. The release path is uninterruptible
   by default, matching ZIO.

4. **`interrupt` (Phase 6.1)** does not need to guard against
   double-completion or double-kill; `Aff.killFiber` already handles
   both. The signature `interrupt :: Fiber e a -> RIO r () Unit`
   (infallible from the caller's perspective, per the revised plan) is
   correct.

5. **Joining a killed fiber surfaces the kill exception** rather than
   the fiber's typed errors. The Phase 6.1 design should expose this
   via a sandbox-style channel (similar to `sandbox`/`unsandbox` in
   Phase 3.3) so users can distinguish "the fiber's typed failure" from
   "the fiber was cancelled". A possible shape:

   ```purescript
   join :: forall r e a. Fiber e a -> RIO r e a              -- kills propagate as defects
   joinExit :: forall r e a. Fiber e a -> RIO r e0 (Exit e a) -- expose cancel/fail/ok
   ```

   where `Exit e a = Cancelled | Failed (Variant e) | Succeeded a`.
   Detail to lock in during Phase 6.1; not yet committed to the API.

6. **`uninterruptibleMask`-equivalent** (referenced in revised Phase 4.3)
   is not needed as a separate primitive: `Aff.bracket`'s release phase
   is already uninterruptible. If user-defined uninterruptible regions
   are desired later, they can be added; not in the v0.1 critical path.

7. **Document the no-yield-no-interrupt rule** in Phase 6.4's
   "Interruption semantics doc". Cite this spike for the underlying
   evidence.

## Reproducing the Findings

```sh
npx spago run -p spike-aff-interruption
```

Edit `src/Spike/AffInterruption/Main.purs` to adjust scenario parameters.
Each scenario is self-contained; comment out items in `main` to focus on
one at a time.
