# Phase 5 Review: Layered Application

**Status:** Complete.

**Recommendation:** **GO.** The layer machinery from Phases 5.1 to 5.4
handles a realistic six-service application across three scenarios
(happy path, failing layer, program failure after service use)
without surprises. The single DX issue worth tracking is the absence
of a passthrough operator for sequential composition, captured below
as DX-1 with a recommended fix for a future phase.

## Method

A workspace sub-package, `spike-phase-5-review`, depends on the real
`rio-aff` package and exercises a six-service layered application
against the production API only. No internal modules are reached
into; everything goes through `RIO.Aff.Core` (plus the `<+>` / `>>>`
operators from `RIO.Aff.Layer`, which `RIO.Aff.Core` deliberately does not
re-export to avoid clashing with `Prelude.(>>>)`).

### Services

Six services across three rows:

| Service       | Operations                          | Layer          |
| ------------- | ----------------------------------- | -------------- |
| `Config`      | static record                       | `platformLayer`|
| `Logger`      | `log :: String -> Aff Unit`         | `platformLayer`|
| `Clock`       | `now :: Aff Int`                    | `platformLayer`|
| `Cache`       | `get`, `put`                        | `dataLayer`    |
| `Database`    | `fetchUser :: Int -> Aff (Maybe String)` | `dataLayer` |
| `UserService` | `greet :: Int -> Aff String`        | `userServiceLayer` |

### Layers

```
platformLayer events cfg
  :: Layer () e (config :: Config, logger :: Logger, clock :: Clock)
platformLayer events cfg =
  configLayer cfg <+> loggerLayer events <+> clockLayer events

dataLayer events
  :: Layer (config :: Config, logger :: Logger, clock :: Clock)
           (dbConnect :: String)
           ( cache :: Cache
           , database :: Database
           , logger :: Logger
           , clock :: Clock
           )

userServiceLayer
  :: forall e
   . Layer ( cache :: Cache
           , database :: Database
           , logger :: Logger
           , clock :: Clock
           )
           e
           (userService :: UserService)

appLayer events cfg
  :: Layer () (dbConnect :: String) (userService :: UserService)
appLayer events cfg =
  platformLayer events cfg >>> dataLayer events >>> userServiceLayer
```

Notes:

- `platformLayer` uses Phase 5.2's `<+>` to combine three independent
  sub-layers built with `fromRecord` (config, logger) and `fromRIO`
  (clock, which needs to allocate a `Ref`).
- `dataLayer` is both a *failing* layer (raises `dbConnect` when
  `databaseUrl` is empty) and a *resourceful* layer (registers
  `cache-flush` and `db-close` finalizers via the `scope` service).
- `dataLayer` also *re-emits* the upstream `logger` and `clock`
  services in its output record. Today, `>>>` discards the
  intermediate row's services that aren't returned by the upstream
  layer, so a downstream layer that needs both upstream and
  intermediate services must have them surfaced explicitly by the
  middle layer (see DX-1).
- `userServiceLayer` is a single `fromRIO` body that reads four
  services and composes one downstream operation.

### Scenarios

`main` runs three scenarios via `provideLayer (appLayer events cfg)
program` and asserts the recorded event log byte-for-byte:

* **A (happy path).** Valid config; the program greets users 1, 2,
  3 twice. Expected log:

  ```
  clock:init, cache-open cap=32, db-open url=postgres://demo/db,
  log:greet@1 uid=1, log:greet@2 uid=2, log:greet@3 uid=3,
  log:greet@4 uid=1, log:greet@5 uid=2, log:greet@6 uid=3,
  db-close, cache-flush
  ```

  This validates that `>>>` plumbs services correctly, `<+>`'s output
  row unification works, `clock.now` monotonically increases across
  the program, the cache short-circuits on the second pass (no extra
  `db-open` calls), and `provideLayer` runs finalizers in LIFO order
  *after* the program completes.

* **B (failing layer).** Config with empty `databaseUrl`; the
  `dataLayer` raises `dbConnect "empty database url"` before opening
  any resource. Expected runner result: `Left dbConnect`. Expected
  log: `[clock:init]` only. The program body must not run.

* **C (program failure after service use).** Valid config; the
  program calls `greet 7` once, then `fail (Proxy :: Proxy "progBoom")
  unit`. Expected runner result: `Left progBoom` (the row is
  `(dbConnect :: String, progBoom :: Unit)`, the union of layer and
  program rows). Expected log includes `cache-flush` and `db-close`
  *after* the program failure, proving resource cleanup runs on the
  program-side failure path.

All three scenarios pass on every run.

## Results

```
$ npx spago run -p spike-phase-5-review
Phase 5 review: layered application, three scenarios.
OK    A: happy path
OK    B: failing layer (got dbConnect: empty database url)
OK    C: program failure after use
OK: 3 scenarios passed.
```

The harness asserts on the *event sequence*, not just length, so a
reordering of `db-close` / `cache-flush` would fail the run.

## What This Validates

- **Phase 5.1.** `fromRecord` and `fromRIO` both produce usable
  `Layer ()` values, and `buildLayer` is not the only path to running
  them: `provideLayer` is the user-facing combinator and works on
  closed (`Layer ()`) and open (`Layer rIn`) layers alike when paired
  with `>>>`.
- **Phase 5.2.** `<+>` correctly unions three independent output
  rows. `>>>` correctly chains the platform layer's output into the
  data layer's input and the data layer's output into the user
  service layer's input. The compiler refuses combinations whose row
  union is ill-formed (overlapping labels).
- **Phase 5.3.** `provideLayer` unions a non-empty layer error row
  (`dbConnect`) with a non-empty program error row (`progBoom`)
  into a single `(dbConnect, progBoom)` output row, exactly the shape
  the Phase 0.4 row-inference spike predicted. The
  `unsafeCoerce`-based program-side expansion documented in
  `src/RIO/Layer.purs` behaves correctly at runtime; no values are
  lost or duplicated.
- **Phase 5.4.** Layer-registered finalizers (`cache-flush`,
  `db-close`) run after the program on every observed termination
  path: success (scenario A), program-level typed failure (scenario
  C), and layer-level typed failure (scenario B, where finalizers
  for resources that never opened are correctly not registered).
  Finalizer order is LIFO: `db-close` (registered second) runs
  before `cache-flush` (registered first), matching the documented
  contract in `RIO.Aff.Resource.scoped`.

## What This Does **Not** Validate

- Defect (`die`) program paths under `provideLayer`. Phase 5.4's unit
  test in `test/Test/RIO/LayerSpec.purs` covers this. The review
  harness exercises the layer-level failure and the program-level
  typed-failure paths only; the defect path is intentionally left to
  the unit test because adding a `die` scenario here would have to
  use `attempt` to recover, and that obscures the event-log
  assertion.
- Cross-fiber concurrency. Phase 6 will exercise layers under
  `fork` / `race`; this review is single-fiber.

## DX Observations

### DX-1: No passthrough operator for sequential composition.

When the middle layer of `a >>> b >>> c` produces services that
both `b` and `c` need, the middle layer must manually re-emit them.
In our `dataLayer`, the platform's `logger` and `clock` are needed
both inside `dataLayer` (for logging cache misses, timing) and
inside `userServiceLayer` (for the `greet` operation), so we list
them in the output record alongside `cache` and `database`. The
boilerplate is small (two extra `<- ask` calls and two extra fields
in the returned record), but it grows with the number of upstream
services and is the kind of thing users will reach for an operator
to elide.

In ZIO and Effect-TS this is `++` (or `>+>` in some sources): "this
layer's output plus its input". A sketch of the type:

```
passthrough
  :: forall rIn e rOut rOut'
   . Row.Union rIn rOut rOut'
  => Layer rIn e rOut
  -> Layer rIn e rOut'
```

I.e., produce a layer whose output is the disjoint union of the
inputs and the original output. Sequential composition with the
downstream layer then sees both. We are not adding this in Phase 5
because the build plan defers it; tracking as a candidate for a
later phase (most likely a 5.5 follow-up or a Phase 6 utility).

### DX-2: `forall e.` on every closed layer signature.

`platformLayer`, `loggerLayer`, `configLayer`, `clockLayer`, and
`userServiceLayer` all declare `forall e. Layer ... e ...`: their
error row is polymorphic because they don't fail. Without that
quantifier, `>>>` doesn't unify the rows of the platform layer (no
error) and the data layer (`dbConnect`).

This is the same shape that already shows up in
`spikes/phase-2-review/`: services that don't fail are
parametrically polymorphic in `e`. Worth a one-paragraph mention in
the upcoming `docs/04-layers.md` (which we have not written yet),
not a code change.

### DX-3: `appLayer` returns the union of every error.

The `dbConnect` failure mode of `dataLayer` shows up in `appLayer`'s
type as `Layer () (dbConnect :: String) (userService :: UserService)`,
even though the surrounding code only ever inspects it once at the
runner. This is by design (the error row is the type-level summary
of what can go wrong), and the compiler-inferred signatures of the
three test scenarios all came out as predicted by the Phase 0.4
spike's findings. No DX issue, just confirmation that the row stays
informative as layers compose.

### DX-4: The `scope` service is not visible to users at the
        call site.

Inside `dataLayer`'s `fromRIO`, we `ask (Proxy :: Proxy "scope")` to
get a `Scope` for `addFinalizer`. The `scope` row is automatically
added by the `Layer` newtype, so callers don't see it in any
signature, but inside `fromRIO` you must remember it's there. The
existing `docs/02-services.md` doesn't cover this; it should be
called out in `docs/04-layers.md` as the idiomatic way to register
resources.

## Pointers

- Harness:   `src/Spike/Phase5Review/Main.purs`
- Layers:    `src/Spike/Phase5Review/Layers.purs`
- Services:  `src/Spike/Phase5Review/Services.purs`
- Underlying primitives: `src/RIO/Layer.purs`, `src/RIO/Resource.purs`
- Row-inference contract: `spikes/row-inference/FINDINGS.md`
