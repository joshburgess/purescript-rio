# rio-benchmarks

Microbenchmarks for the rio-aff runtime. Four scenarios:

1. Monadic bind in a tight loop (100 binds and 10 000 binds).
2. Service-lookup overhead (`ask` + `Record.get` in a tight loop).
3. Sequential vs parallel traversal of a 32-element array.
4. Typed failure round-trip (`fail` + `catchTag`).

Plus baselines for `runRIO' (pure unit)`, raw `Aff (pure unit)`,
and a service-free pure loop.

## Running

From the workspace root:

```
npx spago run -p rio-benchmarks
```

Each scenario prints `mean / stddev / min / max`. Numbers are
wall-clock per iteration, sampled with `process.hrtime()`.

Run node with `--expose-gc` for the most stable numbers:

```
node --expose-gc output/Benchmarks.Main/index.js
```

(after `npx spago build -p rio-benchmarks`).

## What the numbers mean

See `docs/performance.md` for the headline baselines and a
discussion of dominant costs.
