# Performance baseline

This page captures baseline performance numbers for the
**rio-aff** runtime. The goal is not absolute benchmark heroics;
it is to make the cost model legible so users can reason about
hot paths and so changes can spot regressions.

The premier **rio-fiber** runtime is faster than rio-aff on the
bind hot path (about 10 ns per `bind` versus 33 ns) and on
fork-heavy fan-out (`forkAll x16 + joinAll` runs at roughly 5x
the speed of `forkAff x16 + joinFiber`); see the rio-fiber
README for the headline rio-fiber numbers and the
`Benchmarks.VsAff` / `Benchmarks.VsFiber` modules under
`benchmarks/src/Benchmarks/` for the head-to-head sources.

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
| `runRIO' (pure unit)` baseline            | ~265 ns     | ~210 ns    | fixed cost of one `runOp` entry      |
| `Aff (pure unit)` baseline                | ~150 ns     | ~83 ns     | underlying `Aff` cost                |
| Bind chain, 100 binds                     | ~17.6 μs    | ~7.3 μs    | ~90 to 175 ns per bind               |
| Bind chain, 10 000 binds                  | ~936 μs     | ~784 μs    | ~90 ns per bind, amortised           |
| `ask` + record read, 100 iterations       | ~11.8 μs    | ~8.0 μs    | ~80 to 120 ns per service lookup     |
| `traverse`, 32 pure elements              | ~9.9 μs     | ~5.6 μs    | sequential baseline                  |
| `parTraverse`, 32 pure elements           | ~28.4 μs    | ~18.9 μs   | ParAff overhead with no overlap      |
| `fail` + `catchTag`, 1 round-trip         | ~930 ns     | ~458 ns    | `Variant.inj` + `Variant.on`         |

## Dominant costs

### `bind` through the Op interpreter

The hottest path in any RIO program is the bind chain. Each
`bind` appends a BIND `Op` onto the operation list; when the
interpreter resumes, it walks the list in a tight while loop,
applying each continuation against the environment record and
threading the typed-error state through. Synchronous binds
never cross into `Aff` at all; only true async steps do.

The amortised cost per bind is about 90 ns at 10 000 binds
(the longer chain hides per-run overhead in the constant
factor); at 100 binds the per-bind share rises into the 130
to 175 ns range because the fixed runner cost is more
visible. For comparison, a direct `Aff` pure-unit run costs
about 150 ns. The Op loop is competitive because synchronous
binds skip the `Aff` trampoline entirely; the cost above is
the JS while loop plus typed-error bookkeeping.

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
- **Baselines:** keyed per scenario inside
  `benchmarks/src/Benchmarks/Gate.purs`, selected by profile.
  The active profile comes from the `RIO_GATE_PROFILE`
  environment variable; an unset value defaults to
  `local-m1-pro`. Two profiles ship today:
  - `local-m1-pro` (Apple M1 Pro / node 20.15.1), copied
    verbatim from the headline table above.
  - `ci-ubuntu-latest` (GitHub Actions `ubuntu-latest` /
    node 20), captured from CI output.
- **Tolerance:** a single constant `tolerance = 3.0` applied to
  every scenario with a baseline. A scenario fails when its
  measured mean is more than 3x the baseline mean. Scenarios
  with no baseline in the active profile show up as `n/a` in
  the report and do not contribute to the regression count.

The tolerance is deliberately generous. Run-to-run variance on
the higher-noise scenarios (parTraverse over pure work, the
single-round-trip `fail` + `catchTag`) is 10 to 30 percent,
and machine-to-machine variance is larger still. A 3x threshold
catches catastrophic regressions (an accidental O(n^2), a
re-wrapping bug doubling per-bind overhead) without flapping on
the kind of noise CI runners introduce.

### Updating the local-m1-pro baseline

When you intentionally change a hot path:

1. Run `npx spago run -p rio-benchmarks` and capture the headline
   numbers from the new version.
2. Update the corresponding rows in the table at the top of this
   file and the values returned from `localM1ProBaseline` in
   `benchmarks/src/Benchmarks/Gate.purs`.
3. Note the rationale for the change in the PR description so a
   future reader can see why the baseline shifted.

### Updating the ci-ubuntu-latest baseline

The CI baseline is captured from the gate's own output, not
hand-edited from observations. Every gate run prints a single
`BASELINE_JSON` line whose `means` object maps scenario keys
to observed nanoseconds, e.g.:

```
BASELINE_JSON {"profile":"ci-ubuntu-latest","means":{"bindChain.100":21300.0,...}}
```

To refresh the CI baseline:

1. Push a PR (or trigger the workflow manually) and let the
   "Perf regression gate (informational)" step run on the
   `node 20` matrix leg.
2. Find the `BASELINE_JSON` line in that step's log. Each
   `<key>:<mean>` pair maps directly to one of the
   `ciUbuntuLatestBaseline` cases in
   `benchmarks/src/Benchmarks/Gate.purs`.
3. Update the function so each scenario key returns `Just <mean>`
   for the observed value. Keep cases you don't yet have data
   for as `Nothing`; the gate will report them as `n/a` until
   you fill them in.
4. Re-run CI on the same branch to verify the gate now compares
   against real numbers and reports `OK`.

### Promoting the gate from informational to required

The CI step is currently `continue-on-error: true` so the gate
runs on every build but a regression does not fail CI. Once the
`ci-ubuntu-latest` baseline has been captured and at least one
green run has confirmed the numbers are stable, remove
`continue-on-error` from the step in `.github/workflows/ci.yml`.

This is the only flip required. The gate already exits with a
non-zero status on regression and a zero status on success, so
GitHub Actions picks up the right semantics automatically.

## What the benchmarks intentionally exclude

**Profile-driven `unsafeCoerce` hot-path tightening.** `Record`
and `Variant` indirection costs were the most-watched performance
risk during the initial build, but the headline numbers above
suggest the indirection is already cheap enough (service lookup
is indistinguishable from a pure loop, the `Variant` round-trip
is sub-microsecond). We will revisit if a real program's profile
flags either as a bottleneck.
