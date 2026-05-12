# Phase 2 Review: Inference Quality of the Real `RIO.Core` API

**Status:** Complete.

**Recommendation:** **GO.** All 10 realistic service compositions
type-check without any user-supplied type signatures against the
production `RIO.Core` API. The Phase 0.4 spike's sharp edge (LE-1, the
`Lacks "logger" t190` leak in `provide`) is **not** present in the
production API; the inferred type of `example5` confirms `Lacks` was
successfully dropped in Phase 2.2 without losing safety. No regressions
versus the spike. One new pattern not covered by the spike (branching
that asks different services on each side of an `if`) infers cleanly.

## Method

A workspace sub-package, `spike-phase-2-review`, depends on the real
`rio` package and contains 10 top-level bindings, **all unannotated**,
covering single ask, projection, multi-service composition, structured
failures, `provide`, `provideAll`, smart-constructor style, the kitchen
sink, branching, and helper reuse.

The package builds successfully (`npx spago build -p spike-phase-2-review`
exits 0). On a fresh build (after `rm -rf output`) it emits exactly
10 `MissingTypeDeclaration` warnings, one per binding. The compiler's
inferred types from those warnings are reproduced verbatim below.

## Inferred Types for Each Example

Reproduced verbatim from the compiler's `MissingTypeDeclaration` output.
Unification variable names (`a336`, `r'287`, `t283`, ...) are preserved
to make reproduction unambiguous.

### Example 1: single `ask`

```
example1 = ask (Proxy :: Proxy "logger")

forall a336 r'337 e339. RIO ( logger :: a336 | r'337 ) e339 a336
```

Open service row with `logger` at the head; the looked-up type is
universally quantified; error row stays free. **Matches spike Example 1.**

### Example 2: `asks` projection

```
example2 = asks (Proxy :: Proxy "config") _.greeting

forall r'287 e289 b290 t295.
  RIO ( config :: { greeting :: b290 | t295 } | r'287 ) e289 b290
```

The inner record's row is open (`t295`): any record with at least a
`greeting` field is accepted. **Matches spike Example 4.**

### Example 3: three disjoint services

```
example3 = do
  logger <- ask (Proxy :: Proxy "logger")
  cfg <- ask (Proxy :: Proxy "config")
  db <- ask (Proxy :: Proxy "database")
  pure { logger, cfg, db }

forall a248 e254 a259 a270 t283.
  RIO ( config :: a259, database :: a270, logger :: a248 | t283 )
      e254
      { cfg :: a259, db :: a270, logger :: a248 }
```

Three labels unioned automatically, tail stays open, returned record
threads each service's type through to its field. **Generalises spike
Example 3 from two services to three; still no annotation needed.**

### Example 4: `fail` with a structured payload

```
example4 = fail (Proxy :: Proxy "notFound") { id: 7, kind: "user" }

forall r239 e'240 b242.
  RIO r239 ( notFound :: { id :: Int, kind :: String } | e'240 ) b242
```

Error row open with `notFound` at the head, payload type captured.
Value type is universally quantified (the computation never returns).
**Matches spike Example 6.**

### Example 5: `provide` shrinks the row (LE-1 regression check)

```
example5 =
  let inner = do
        logger <- ask (Proxy :: Proxy "logger")
        cfg <- ask (Proxy :: Proxy "config")
        pure { logger, cfg }
      fakeLogger :: Logger
      fakeLogger = { log: \_ -> pure unit }
  in provide (Proxy :: Proxy "logger") fakeLogger inner

forall a212 e230 t235.
  RIO ( config :: a212 | t235 )
      e230
      { cfg :: a212
      , logger :: { log :: String -> Aff Unit }
      }
```

**KEY RESULT.** Compare with spike Example 5:

```
spike:       Lacks "logger" t190 => RIO ( database :: a150 | t190 ) e185 Unit
production:                         RIO ( config   :: a212 | t235 ) e230 { ... }
```

The `Lacks` constraint that leaked into the inferred type in the spike
is **gone** in the production API. This validates the Phase 2.2 decision
to drop `Lacks` from `provide`'s signature: the inferred type is now
clean and the `Cons` constraint alone suffices.

### Example 6: `provideAll` discharges the full row

```
example6 =
  let inner = do
        logger <- ask (Proxy :: Proxy "logger")
        cfg <- ask (Proxy :: Proxy "config")
        liftAff (logger.log "ready")
        pure cfg.greeting
  in provideAll { logger: ..., config: ... } inner

forall e191. RIO () e191 String
```

Service row collapses to `()`; only the error row stays free. This is
exactly the shape `runRIO` and `runRIO'` accept, so a `provideAll`
result is directly runnable. **No spike analogue; this is a real-API
addition.**

### Example 7: idiomatic smart-constructor

```
logInfo msg = do
  logger <- ask (Proxy :: Proxy "logger")
  liftAff (logger.log msg)

forall t21 b24 r'27 e29 t36.
  t21
  -> RIO ( logger :: { log :: t21 -> Aff b24 | t36 } | r'27 ) e29 b24
```

The helper is **fully polymorphic** in the message type (`t21`), the
return type of `log` (`b24`), the rest of the logger's record (`t36`),
the rest of the environment row (`r'27`), and the error row (`e29`).
This is the most general possible inference: a caller that uses
`logInfo` against a concrete logger pins all five variables at use
sites and the helper composes without any annotation effort. **This is
the production payoff of the row-inference design.**

### Example 8: services + lifts + a typed failure

```
example8 = do
  cfg <- ask (Proxy :: Proxy "config")
  logger <- ask (Proxy :: Proxy "logger")
  liftEffect (Console.log "starting")
  _ <- fail (Proxy :: Proxy "notFound") { id: 99 }
  liftAff (logger.log "won't reach")
  pure { host: cfg.host, port: cfg.port }

forall e'132 a138 t144 t148 t150 t152 t153.
  Discard a138
  => RIO
       ( config :: { host :: t148, port :: t150 | t152 }
       , logger :: { log :: String -> Aff a138 | t144 }
       | t153
       )
       ( notFound :: { id :: Int } | e'132 )
       { host :: t148, port :: t150 }
```

**THE BIG ONE.** Services unioned (`config`, `logger`), error row picks
up `notFound`, value type is the record literal at the end, `liftEffect`
and `liftAff` are transparent to the row machinery. Zero annotations.
**Generalises spike Example 9.**

The `Discard a138` constraint is incidental: it comes from the standard
PureScript do-notation rule that the result of a discarded statement
(`liftAff (logger.log "won't reach")`) must have a `Discard` instance.
Not RIO-specific.

### Example 9: branching keeps the row union

```
example9 useDb = do
  cfg <- ask (Proxy :: Proxy "config")
  if useDb then do
    db <- ask (Proxy :: Proxy "database")
    rows <- liftAff (db.find cfg.port)
    pure rows
  else do
    logger <- ask (Proxy :: Proxy "logger")
    liftAff (logger.log "skip")
    pure []

forall e47 t68 t69 t70 a85 t91 t95 t97.
  Discard a85
  => Boolean
  -> RIO
       ( config :: { port :: t69 | t70 }
       , database :: { find :: t69 -> Aff (Array t95) | t68 }
       , logger :: { log :: String -> Aff a85 | t91 }
       | t97
       )
       e47
       (Array t95)
```

**NEW PATTERN, NOT IN SPIKE.** Branching where each branch asks a
different service: the join-point row contains the union of both
branches' requirements. Crucially the type-checker does **not** require
the unused-on-this-side service to be present in the other branch's
local row; both branches contribute requirements that get unioned at
the bind point. This is the row-union behaviour Phase 2 needs for
non-trivial control flow and it works without intervention.

### Example 10: helper reuse with different services

```
example10 = do
  greeting <- asks (Proxy :: Proxy "config") _.greeting
  rows <- asks (Proxy :: Proxy "database") _.find
  one <- liftAff (rows 1)
  pure { greeting, one }

forall a298 e304 t310 t324 a326 t333.
  RIO
    ( config :: { greeting :: a298 | t310 }
    , database :: { find :: Int -> Aff a326 | t324 }
    | t333
    )
    e304
    { greeting :: a298, one :: a326 }
```

Two `asks` calls in one do-block, each with a distinct proxy, produce
two row entries with their own open tails. The function-valued field
`find` projected via `asks` returns a usable function that we call with
`rows 1`. Note this is the "extract the function, then call it"
half-pattern, not the trap discussed in `docs/02-services.md` (which
warns against composing `asks` *with* its argument as a single op call).

## Cross-Reference Against Phase 0.4 Spike

| Phenomenon                              | Spike   | Production (this review) |
|-----------------------------------------|---------|--------------------------|
| Single `ask` infers open row            | PASS    | PASS (Example 1)         |
| `asks` projection, inner row open       | PASS    | PASS (Example 2)         |
| Two-service union                       | PASS    | PASS (subsumed by Ex. 3) |
| Three-service union                     | not run | PASS (Example 3)         |
| `fail` with structured payload          | PASS    | PASS (Example 4)         |
| `provide` shrinks row                   | PASS w/ LE-1 caveat | **PASS, no LE-1** (Example 5) |
| `provideAll` to `RIO () e a`            | not run | PASS (Example 6)         |
| Smart-constructor helper polymorphism   | not run | PASS (Example 7)         |
| Kitchen sink (services + fail + lifts)  | PASS    | PASS (Example 8)         |
| Branching with different services       | not run | PASS (Example 9)         |
| Helper reuse across services            | PASS    | PASS (Example 10)        |

**LE-1 resolved.** The `Lacks` leak the spike flagged is gone in the
production API. The Phase 2.2 acceptance criterion (no manual
annotation needed for documented ergonomic patterns) holds across all
10 patterns surveyed here.

## Patterns That Did NOT Require an Annotation

All 10. No regression to file.

## Sharp Edges Worth Documenting

- **`Discard` constraint surfaces in some inferred types** (Examples 8
  and 9). This is unrelated to RIO; it's the standard PureScript
  do-notation rule that statements whose results are dropped must
  have a `Discard` instance. Users won't see this unless they write
  explicit signatures and copy-paste the inferred type. Worth mentioning
  in the FAQ section of the next docs update but not a fix candidate.

- **No issue with function-valued operations via `asks`** (Example 10).
  The trap documented in `docs/02-services.md` is about treating
  `asks _.op` as if it were the operation itself (it returns the
  function, which still must be called). The trap doc remains accurate.

## Decisions Feeding Phase 3

1. **No changes to `RIO.Core`, `RIO.Env`, or `RIO.Error` required**
   based on this review.
2. **Phase 3 can build on the validated row-inference foundation**
   without revisiting the type-level shape of the existing primitives.
3. **`catchTag`** in Phase 3.1 should use the type signature recorded
   in the Phase 0.4 spike's "Decisions" section, unchanged.

## Reproducing the Findings

```sh
rm -rf output
npx spago build -p spike-phase-2-review 2>&1 | grep -A 18 "inferred type of example"
```

This prints all 10 inferred types verbatim with their unification
variable names. Subsequent incremental builds exit clean without
re-emitting the warnings because no modules are recompiled.
