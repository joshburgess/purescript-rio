# Performance baseline

This page captures the v0.1 performance numbers for `rio`. The
goal is not absolute benchmark heroics; it is to make the cost
model legible so users can reason about hot paths and so future
versions can spot regressions.

The benchmark suite lives in `benchmarks/`. Run it with:

```
npx spago run -p rio-benchmarks
```

Numbers below were captured on an Apple M1 Pro running macOS,
node v20.15.1, 1000 samples per scenario. They are wall-clock
per iteration, measured by sampling `process.hrtime()` before
and after each invocation. The benchmark process does not force
GC; expect 10 to 30 percent run-to-run variance on the higher-
variance scenarios (the long tails are GC pauses).

## Headline numbers

| Scenario                                  | Mean        | Min        | Notes                                |
| ----------------------------------------- | ----------- | ---------- | ------------------------------------ |
| `runRIO' (pure unit)` baseline            | ~265 ns     | ~210 ns    | fixed cost of `unRIO {} >>= ...`     |
| `Aff (pure unit)` baseline                | ~150 ns     | ~83 ns     | underlying `Aff` cost                |
| Bind chain, 100 binds                     | ~17.6 μs    | ~7.3 μs    | ~90 to 175 ns per bind               |
| Bind chain, 10 000 binds                  | ~936 μs     | ~784 μs    | ~90 ns per bind, amortised           |
| `ask` + record read, 100 iterations       | ~11.8 μs    | ~8.0 μs    | ~80 to 120 ns per service lookup     |
| `traverse`, 32 pure elements              | ~9.9 μs     | ~5.6 μs    | sequential baseline                  |
| `parTraverse`, 32 pure elements           | ~28.4 μs    | ~18.9 μs   | ParAff overhead with no overlap      |
| `fail` + `catchTag`, 1 round-trip         | ~930 ns     | ~458 ns    | `Variant.inj` + `Variant.on`         |

## Dominant costs

### `bind` over `Aff`

The hottest path in any RIO program is the bind chain. Each
`bind` peels the newtype, calls the underlying function with the
environment record, runs the resulting `Aff`, and threads the
`Either (Variant e) a` through. The amortised cost per bind is
about 90 ns at 10 000 binds (the longer chain hides per-run
overhead in the constant factor); at 100 binds the per-bind
share rises into the 130 to 175 ns range because the fixed
runner cost is more visible.

In practice the dominant share of bind time is `Aff`'s own
trampoline, not RIO's wrapper. A direct `Aff` pure-unit run
costs about 150 ns; RIO adds about 100 ns on top to wrap and
unwrap the `Either` and the environment record.

### Service lookup

`ask` is `Record.get`, which compiles to a single property
access on the environment record. The 100-iteration loop runs
in about 12 μs, which is essentially indistinguishable from a
pure 100-step loop with no services (~10 μs). Service-row depth
does not affect lookup cost because every label resolves to a
direct property access, not a dictionary traversal.

In short: prefer many small services in the row over one large
service with a deep nested record. The row dispatch is free;
walking nested fields is not.

### Parallel vs sequential traversal

`parTraverse` over 32 elements of pure work costs about 3x as
much as `traverse` (~28 μs vs ~10 μs). This is the cost of
running through `Aff`'s `ParAff` applicative when there is no
real latency to overlap. The break-even point is around 10 μs
of latency per element; below that, sequential traversal is
cheaper.

For real I/O (HTTP, disk, database) the latency dwarfs the
ParAff bookkeeping and `parTraverse` is a clear win. Save it
for those cases; do not reach for it on tight CPU-bound loops.

### Typed failure round-trip

Constructing a typed failure with `fail` and catching it with
`catchTag` round-trips in about 930 ns. This is dominated by
`Variant.inj` plus the `Variant.on` dispatch in the catch path.
Adding more tags to the error row does not measurably affect
the round-trip; the dispatch is a single symbol comparison
regardless of row width.

This is fast enough that typed failures are a reasonable choice
for any failure mode the caller might actually want to inspect
and recover from. For truly hot loops where you measure failure
construction as a bottleneck, raise a defect via `die` instead
(constructing an `Error` is somewhat cheaper than constructing a
`Variant`, and the defect can be reified with `sandbox` once
outside the loop).

## What the benchmarks do not measure

- **Layer build cost.** A `Layer` is built once at startup, not
  per request; we did not measure it because in any realistic
  application its cost is amortised across the process lifetime.
- **Resource bracket cost.** `acquireRelease` and `scoped` add
  one `Aff.bracket` apiece, whose overhead is in the same
  range as a single bind. Measuring this in isolation produced
  numbers indistinguishable from the bind baseline.
- **Real I/O.** Every interesting service operation is `Aff`-
  valued; once `liftAff` is on the hot path, the I/O latency
  dominates the bind overhead by orders of magnitude.
- **Cold-start GC.** The first 50 to 100 iterations of any
  scenario are noisy; the headline numbers use 1000 samples to
  let the JIT warm up and average over GC pauses. For the most
  stable numbers, run node with `--expose-gc`.

## Reproducing

The benchmark binary is a workspace package, so a single Spago
command does the run end-to-end:

```
npx spago run -p rio-benchmarks
```

Each scenario prints `mean / stddev / min / max` in the most
appropriate unit (ns / μs / ms / s). The full source is in
`benchmarks/src/Benchmarks/`; the `Harness` module is a small
`Aff`-aware port of `purescript-minibench` that uses
`process.hrtime()` for nanosecond resolution.

## Regression gate

`Benchmarks.Gate` is a developer-runnable check that exercises
the same scenarios as `Benchmarks.Main` and compares each one's
mean wall-clock per iteration against a hard-coded baseline.
Run it before opening a PR that touches a hot path:

```
npx spago run -p rio-benchmarks --main Benchmarks.Gate
```

Output is a single table plus a summary line. The gate exits
with status `0` if every scenario is within tolerance and `1`
if any scenario regressed.

### How the gate decides

- **Metric:** mean wall-clock per iteration, 500 samples per
  scenario.
- **Baselines:** hard-coded in `benchmarks/src/Benchmarks/Gate.purs`,
  copied verbatim from the headline table above (Apple M1 Pro /
  node 20.15.1).
- **Tolerance:** a single constant `tolerance = 3.0` applied to
  every scenario. A scenario fails when its measured mean is
  more than 3x the baseline mean.

The tolerance is deliberately generous. Run-to-run variance on
the higher-noise scenarios (parTraverse over pure work, the
single-round-trip `fail` + `catchTag`) is 10 to 30 percent,
and machine-to-machine variance is larger still. A 3x threshold
catches catastrophic regressions (an accidental O(n^2), a
re-wrapping bug doubling per-bind overhead) without flapping on
the kind of noise CI runners introduce.

### Updating the baseline

When you intentionally change a hot path:

1. Run `npx spago run -p rio-benchmarks` and capture the headline
   numbers from the new version.
2. Update the corresponding rows in the table at the top of this
   file and the `baselineMeanNs` constants in
   `benchmarks/src/Benchmarks/Gate.purs`.
3. Note the rationale for the change in the PR description so a
   future reader can see why the baseline shifted.

The two locations are kept in sync by hand; we may move them to
a single JSON file in a later phase.

### Why the gate is not (yet) wired into CI

CI runners have different CPUs, memory pressure, and JIT
warm-up behaviour than the reference hardware, so the same code
that passes the gate locally can fail on a slow runner and pass
on a fast one. Until we capture a CI-environment baseline (a
follow-up task), the gate is informational: CI builds it
(verifying the module compiles and its baseline references stay
in sync with the surface) but does not run it.

## What is *not* in this phase

**Profile-driven `unsafeCoerce` hot-path tightening.** The risk
table in `PROJECT_BUILD_PLAN.md` calls out the `Record` /
`Variant` indirection cost as a Medium risk; the headline numbers
above suggest the indirection is already cheap enough (service
lookup is indistinguishable from a pure loop, the `Variant`
round-trip is sub-microsecond). We will revisit if a real
program's profile flags either as a bottleneck.
