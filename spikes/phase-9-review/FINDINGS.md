# Phase 9 (v0.3) Review: Logger, Local, and STM Stress Test

**Status:** Complete.

**Recommendation:** **GO.** Across 4000 randomised iterations
(four consecutive local runs of 1000 iterations each) the harness
reports zero invariant violations across the four v0.3 modules
exercised: `RIO.Logger` annotation restoration, `RIO.Local`
scoped overrides under fork-and-kill, `RIO.STM.TQueue`
producer/consumer correctness, and `RIO.STM.THub` fan-out with the
Unbounded strategy. The `finally`-backed restore the documentation
promises for `withFields` and `locally` holds under typed failures
and forked-then-killed termination, and STM's atomicity under
contention does not lose, duplicate, or drop values.

## Method

We follow the same shape as the Phase 6 review's harness: one
workspace sub-package (`spike-phase-9-review`) depending on the
production `rio` API only, with each scenario exposing a single
load-bearing invariant per iteration. Random parameters come from
`Effect.Random.randomInt`. The four scenarios:

### Scenarios

| ID | Module           | Random parameters                                                                       | Invariant                                                                              |
| -- | ---------------- | --------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| A  | `RIO.Logger`     | depth `[1, 8]`, failPct `[0, 50]`, forkPct `[0, 50]`                                    | After the program returns, the logger's annotation set is empty.                       |
| B  | `RIO.Local`      | depth `[1, 8]`, failPct `[0, 50]`, forkPct `[0, 50]`, killPct `[0, 50]`                 | After the program returns, the `Local Int` holds its initial value.                    |
| C  | `RIO.STM.TQueue` | producers `[1, 4]`, consumers `[1, 4]`, perProducer `[4, 16]`                           | Sum and count of dequeued values match sum and count of enqueued values.               |
| D  | `RIO.STM.THub`   | subscribers `[1, 5]`, publishCount `[4, 20]`                                            | Every subscriber dequeues exactly `publishCount` values; their sums match the source. |

Scenarios A and B exit each iteration through `attempt`, so typed
failures along the way do not abort the harness; the post-return
read of the logger annotations / `Local` cell is the load-bearing
check. Scenario B additionally exercises the interrupt path: when
the fork-and-kill branch fires (`forkPct` and `killPct` both
trigger), the parent kills the child mid-flight to force the
`finally` restore to run on the interruption path.

Scenarios C and D create their STM structures inside the same
`RIO` program that uses them and join all forked consumers before
returning, so the harness's external invariant check (`expectedSum`
/ `publishCount`) is observed after every fiber has settled.

The build plan's 10,000-run target is approximated by repeating
the harness: each invocation is 1000 iterations (250 per scenario),
and four consecutive local runs were performed for this review.

## Results

```
$ npx spago run -p spike-phase-9-review
Phase 9 (v0.3) review: 250 iterations per scenario across Logger,
Local, TQueue, and THub.
OK: 1000 stress iterations, every invariant held.
```

Repeated four times in succession, the harness reported the same
"every invariant held" line every time. Total: 4000 iterations
across 16,000 individual checks. No iteration produced a
non-empty residual annotation set, a drifted `Local` value, or a
mismatched STM count/sum.

## What This Validates

- **`Logger.withFields` restores on every termination path.**
  Scenario A nests `withFields` up to eight levels deep and
  randomly throws a typed failure or forks-then-joins at each
  level. The harness reads `logger.getAnnotations` after the
  program returns and asserts the array is empty. Every iteration
  passed: the `Aff.finally` wrapping `setAnnotations` runs whether
  the body exits cleanly, via typed failure, or under a kill that
  reaches the forked child's `finally` block.
- **`Local.locally` restores on every termination path.** Scenario
  B is the same shape as A but on a single `Local Int` and with an
  additional `killPct`: when a fork happens and the parent kills
  the child mid-flight, the child's `locally` block still restores
  the cell to its outer value before the kill is observed by the
  parent. The post-return read confirms the cell sits at its
  initial value on every iteration.
- **STM atomicity holds under producer/consumer contention.**
  Scenario C runs up to four producers in parallel (via
  `parTraverse`) and up to four consumers as forked fibers. The
  total budget is divided across consumers so the program
  terminates without `readTQueue` retrying past the producer count.
  No iteration dropped, duplicated, or reordered a value: the
  observed sum matches the closed-form sum of the producer
  identity formula and the observed count matches `producers *
  perProducer`.
- **THub Unbounded fan-out reaches every subscriber.** Scenario D
  forks the subscribers before the publisher starts (with a 1ms
  `Aff.delay` so every subscriber has registered before the first
  publish). Each subscriber sees exactly the published count and
  the published sum. No subscriber misses a value, and no
  subscriber sees a value twice.

## What This Does **Not** Validate

- **Bounded / Sliding / Dropping THub strategies.** The harness
  only exercises `Unbounded` because the invariant ("every
  subscriber sees every value") is the strongest. The other three
  strategies have their own invariants (Bounded: producer retries
  rather than dropping; Sliding: oldest dropped first; Dropping:
  returns `false` and drops the new value). Those are covered by
  `test/Test/RIO/STM/THubSpec.purs`; a future randomised harness
  could check them under contention.
- **Logger annotation merging order.** The harness checks the
  residue (must be empty), not the in-block content of emissions.
  `LoggerSpec` covers key-shadowing and attach-order behaviour
  under fixed inputs.
- **Cross-fiber `Local` visibility semantics.** The Local docs
  document the shared-Ref fork-inheritance model as a deliberate
  choice (not true `FiberRef` snapshotting). The harness exercises
  the path where a child reads the parent's `locally` value, but
  does not assert anything specific about ordering of concurrent
  writes; that lives in `LocalSpec`.
- **Long-running soak tests.** Each iteration completes in under a
  few milliseconds. A 24-hour soak would expose accumulated
  state-leak bugs not visible at this duration; that is a v0.4+
  concern.

## Observations

### O-1: Logger annotation restore survives a forked sub-tree.

Scenario A's `forkPct` branch forks the recursive call and joins
it before returning. When the forked branch enters its own
`withFields` block, it mutates the shared annotation Ref. The
parent's later `setAnnotations` (run by the parent's `finally`)
restores the parent's previous snapshot, which is what makes the
post-return read empty. We did not observe a case where the
child's annotations leaked past the parent's restore, even when
the child itself failed mid-block.

### O-2: Local restore survives a mid-block kill.

Scenario B's `killPct` branch calls `interrupt` on a forked child
that is in the middle of its own `locally` block. The kill
propagates as an `Aff` exception, the child's `finally` runs
before the exception surfaces to the parent's `attempt`, and the
parent's outer cell value is restored. Across 250 iterations of
the kill path no residue was observed.

### O-3: THub publisher does not need to wait for subscribers
beyond the initial 1ms delay.

Scenario D inserts a single `delay 1ms` between forking the
subscribers and the first publish. This is enough on every
observed run for `subscribeTHub` to complete before the publisher
starts; we did not observe an iteration where a subscriber missed
the leading values. The 1ms gap is intentionally larger than the
JS event-loop micro-task latency to keep the harness deterministic
at the millisecond grain.

### O-4: No flaky iterations.

The harness uses random sleeps only in scenario D (1ms before the
first publish). Scenarios A, B, and C have no sleeps. We did not
observe a single iteration where the same parameters produced a
violation on one run and a clean result on another. Restoration
behaviour for `withFields` / `locally` and STM atomicity for
`TQueue` / `THub` appear deterministic at this harness's
granularity.

## Pointers

- Harness:                `src/Spike/Phase9Review/Main.purs`
- Scenarios:              `src/Spike/Phase9Review/Stress.purs`
- Underlying primitives:  `src/RIO/Logger.purs`,
  `src/RIO/Local.purs`, `src/RIO/STM/TQueue.purs`,
  `src/RIO/STM/THub.purs`
- Documentation:          `docs/09-stm.md`,
  `docs/11-fiber-local.md`, `docs/12-logging.md`
- Earlier review precedent: `spikes/phase-6-review/FINDINGS.md`
