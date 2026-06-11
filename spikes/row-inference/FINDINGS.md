# Spike 0.4: Row Inference for Service and Error Composition

**Status:** Complete.

**Recommendation:** **GO.** PureScript's row machinery handles the API shapes
we want. Inference is robust for all 10 example patterns without any
user-supplied type signatures. One sharp edge (the `Lacks` constraint leaking
from `provide`) is identified below with a recommended fix that we should
apply before locking in the Phase 2 API.

## Method

A prototype `RIO r e a` newtype was built in `src/Spike/RowInference/Prototype.purs`
exposing `pure`, `bind`, `ask`, `asks`, `fail`, `provide`, `catchTag`,
`liftAff`, and `runRIO`. Ten example programs were written in
`src/Spike/RowInference/Examples.purs` covering service composition, error
composition, and mixed cases. **Every binding in `Examples.purs` is written
without a top-level type signature**, so the compiler's inferred types are
captured via `MissingTypeDeclaration` warnings.

Three deliberate-mistake cases were exercised in `src/Spike/RowInference/Negative.purs`
to inspect compiler error quality. (Cases are currently commented out; uncomment
one at a time to reproduce.)

The package builds successfully (`npx spago build -p spike-row-inference`
exits 0). On a fresh build (i.e. after `rm -rf output`) it emits 41
warnings, all intentional: 10 `MissingTypeDeclaration` (one per top-level
example), 1 for `allExamples`, and 30 `WildcardInferredType` (the three
wildcards in `RIO _ _ _` for each of the 10 examples inside `allExamples`).
The compiler's reports of inferred types in those warnings are the
deliverables of this spike and are reproduced verbatim below. Incremental
builds emit no warnings because modules aren't recompiled.

## Inferred Types for Each Example

Reproduced verbatim from the compiler's `MissingTypeDeclaration` output, with
the de Bruijn-style unification variable names (`a292`, `r'235`, ...) preserved.

### Example 1: simple `ask`

```
example1 = ask (Proxy :: Proxy "logger")

forall a r' e. RIO ( logger :: a | r' ) e a
```

Open service row with `logger` at the head. Error row stays free. Value type
is the looked-up service type. **Perfect inference.**

### Example 2: two reads of the same service

```
example2 = do
  log1 <- ask (Proxy :: Proxy "logger")
  log2 <- ask (Proxy :: Proxy "logger")
  pure { log1, log2 }

forall a r' e. RIO ( logger :: a | r' ) e { log1 :: a, log2 :: a }
```

Single row entry; both lookups unify to the same type variable `a`. No duplication.

### Example 3: compose disjoint services

```
example3 = do
  logger <- ask (Proxy :: Proxy "logger")
  db <- ask (Proxy :: Proxy "database")
  pure { logger, db }

forall a205 e211 a216 t228.
  RIO ( database :: a216, logger :: a205 | t228 ) e211 { db :: a216, logger :: a205 }
```

**KEY RESULT.** Row is the union, inferred automatically. This was the
highest-risk pattern going in; it works without intervention. The compiler
accepts the open tail `t228` and the two labels are added correctly.

### Example 4: `asks` with a field selector

```
example4 = asks (Proxy :: Proxy "config") _.greeting

forall r'194 e196 b197 t202.
  RIO ( config :: { greeting :: b197 | t202 } | r'194 ) e196 b197
```

Note that the inner record's row is also open (`t202`). This is great:
the inferred type accepts any record that *has at least* a `greeting`
field, not just the canonical `Config`.

### Example 5: `provide` shrinks the service row

```
example5 =
  let needsTwo = ...        -- requires logger + database
      fakeLogger = ...
  in provide (Proxy :: Proxy "logger") fakeLogger needsTwo

forall a150 e185 t190.
  Lacks "logger" t190 => RIO ( database :: a150 | t190 ) e185 Unit
```

`provide` removes `logger` from the required row, as expected.

**Sharp edge (LE-1):** The `Lacks "logger" t190` constraint **leaks into the
public type**. This means downstream code that hasn't provided `logger`
will be fine (its tail won't contain `logger`), but any annotation a user
writes for this binding has to repeat the `Lacks` constraint. This is
ergonomically poor and surprising.

**Recommendation:** Drop the `Lacks` constraint from `provide`. With only
`Cons sym a r' r`, `provide` becomes: "take an `r` that has `sym`, and
produce one shape `r'` that's `r` minus `sym`." The `Cons` constraint
already forces `r'` to be the row minus `sym`; `Lacks` is structurally
redundant in PureScript's row encoding. Removing it cleans up the inferred
type without losing safety. **This corroborates the note already added to
the revised plan's item 2.2.**

### Example 6: single `fail`

```
example6 = fail (Proxy :: Proxy "notFound") { id: 42 }

forall r e' b. RIO r ( notFound :: { id :: Int } | e' ) b
```

Open error row, single tag, with the payload type captured from the
expression. Value type is universally quantified (computation never returns).

### Example 7: `catchTag` shrinks the error row

```
example7 =
  let program = do
        _ <- fail (Proxy :: Proxy "notFound") { id: 99 }
        fail (Proxy :: Proxy "parse") "bad json"
  in catchTag (Proxy :: Proxy "notFound") (\_ -> pure "fallback") program

forall r t125. RIO r ( parse :: String | t125 ) String
```

**KEY RESULT.** The starting error row had two tags (`notFound`, `parse`).
After `catchTag _notFound`, the row contains only `parse`. The handler's
return path (`pure "fallback"`) fixes the result to `String`, which the
non-error path of `program` also produces, so the value type is `String`.

### Example 8: compose disjoint errors

```
example8 = do
  _ <- fail (Proxy :: Proxy "notFound") { id: 1 }
  fail (Proxy :: Proxy "parse") "oops"

forall b r t90.
  RIO r ( notFound :: { id :: Int }, parse :: String | t90 ) b
```

**KEY RESULT.** Error row union inferred just as cleanly as service-row
union (Example 3). Both tags appear; tail stays open.

### Example 9: kitchen sink (services + errors + `liftAff`)

```
example9 = do
  logger <- ask (Proxy :: Proxy "logger")
  _ <- liftAff (Console.log "hi")
  _ <- fail (Proxy :: Proxy "notFound") { id: 7 }
  db <- ask (Proxy :: Proxy "database")
  pure { logger, db }

forall a25 e'48 a55 t67.
  RIO ( database :: a55, logger :: a25 | t67 )
      ( notFound :: { id :: Int } | e'48 )
      { db :: a55, logger :: a25 }
```

**THE BIG ONE.** Services unioned correctly, error row carries the failure
tag, `liftAff` is transparent, value type is the record literal at the end.
Zero annotations. This is the whole library's value proposition compiling
under inference.

### Example 10: layer-like binding across `provide`

```
example10 =
  let buildGreeter = do
        cfg <- ask (Proxy :: Proxy "config")
        pure (\name -> cfg.greeting <> ", " <> name <> "!")
      program greeter = do
        _ <- liftAff (Console.log (greeter "world"))
        pure unit
  in do
    greeter <- buildGreeter
    program greeter

forall r'273 e275 t284.
  RIO ( config :: { greeting :: String | t284 } | r'273 ) e275 Unit
```

The `let` binding for `buildGreeter` is inferred polymorphically and the
outer `do` block unifies it with `program`'s requirements. No annotation
needed even though the layer-builder pattern crosses a let boundary.

## Negative Cases: Error Message Quality

### NEG-1: provide a service of the wrong type

```purescript
neg1 =
  provide (Proxy :: Proxy "logger") (42 :: Int)
    (ask (Proxy :: Proxy "logger") :: RIO ( logger :: String ) () String)
```

Compiler output:

```
Could not match type
  Int
with type
  String
while matching label logger
while trying to match type
                             ( logger :: Int
                             ...
                             | t0
                             )
  with type
              ( logger :: String
              ...
              )
while solving type class constraint
  Prim.Row.Cons "logger" Int t0 ( logger :: String )
```

**Quality: GOOD.** The first line names the mismatch directly. The label
is named explicitly. The expected and provided types are both shown.

### NEG-2: catch a tag that isn't in the error row

```purescript
neg2 =
  let program = fail (Proxy :: Proxy "parse") "oops"
  in catchTag (Proxy :: Proxy "notFound") (\_ -> pure "fallback")
       (program :: RIO () ( parse :: String ) String)
```

Compiler output:

```
Could not match type
  ( notFound :: t1
  ...
  | t2
  )
with type
  ( parse :: String
  ...
  )
while solving type class constraint
  Prim.Row.Cons "notFound" t1 t2 ( parse :: String )
while applying a function catchTag
  of type IsSymbol t0 => Cons @Type t0 t1 t2 t3 => ...
```

**Quality: ACCEPTABLE.** The rows are shown side by side and the missing
tag is visible. The mention of `Prim.Row.Cons` and the function-type
spelling-out at the end are noisy but not misleading.

A `Fail` instance triggered when a `Cons` constraint fails on a closed row
would let us replace this with something like *"the error `notFound` is not
in this program's error row. Possible errors are: parse."* This is a
candidate for the v0.2 "Improved compiler error messages" backlog item.

### NEG-3: `runRIO` against an open service row

```purescript
neg3 = runRIO (ask (Proxy :: Proxy "logger"))
```

Compiler output:

```
Could not match type
  ( logger :: t1
  | t4
  )
with type
  ()
while solving type class constraint
  Prim.Row.Cons "logger" t1 t4 (() @Type)
```

**Quality: ACCEPTABLE.** The error correctly identifies that an empty
service row (`()`) cannot satisfy a `Cons "logger"` constraint. A reader
who understands the model gets the message. A new user might not realise
the connection to "you forgot to provide a service" without an entry in
the docs.

## Summary

| Aspect                                  | Result   | Notes |
|-----------------------------------------|----------|-------|
| Service-row union inferred without help | PASS     | Examples 3, 9 |
| Error-row union inferred without help   | PASS     | Examples 8, 9 |
| `ask` lookup                            | PASS     | Examples 1, 2 |
| `asks` with selector                    | PASS     | Example 4 |
| `fail` injection                        | PASS     | Examples 6, 9 |
| `provide` shrinks row                   | PASS     | Example 5 (with caveat) |
| `catchTag` shrinks error row            | PASS     | Example 7 |
| Layer-like binding across `provide`     | PASS     | Example 10 |
| Wrong-type provide, error quality       | GOOD     | NEG-1 |
| Wrong-tag catch, error quality          | ACCEPTABLE | NEG-2 |
| Forgot-to-provide, error quality        | ACCEPTABLE | NEG-3 |

## Decisions Feeding Phase 1+

1. **GO with the current `RIO r e a` shape.** The newtype over
   `Record r -> Aff (Either (Variant e) a)` produces excellent inference
   in all surveyed patterns.

   **Update (2026-06):** the public `RIO r e a` shape is unchanged, but
   the internal representation is now `RIO (Op r e a)` (an interpreter
   instruction tree in `RIO.Aff.Internal`); the `Record r -> Aff (Either
   (Variant e) a)` form survives as the `unRIO` boundary function. The
   inference conclusions still hold.

2. **Drop the `Lacks` constraint from `provide`.** (Originally LE-1 above.)
   This propagates to the revised Phase 2.2 item, which already flagged
   the same redundancy.

3. **`catchTag` final type signature** is the one in the prototype:

   ```purescript
   catchTag
     :: forall sym a e' e r b
      . IsSymbol sym
     => Cons sym a e' e
     => Proxy sym
     -> (a -> RIO r e' b)
     -> RIO r e b
     -> RIO r e' b
   ```

   No revision required for Phase 3.1.

   **Update (2026-06):** the shipped `catchTag` carries one extra
   constraint, `CatchableErrorTag sym a e` (alongside `Row.Cons sym a e'
   e`), added to drive the friendly missing-tag error message (see item
   6 below). The term-level shape is otherwise as written.

4. **`fail` final type signature** (informing Phase 1.3, which is now
   final in Phase 1, not revisited in Phase 3):

   ```purescript
   fail
     :: forall sym a r e' e b
      . IsSymbol sym
     => Cons sym a e' e
     => Proxy sym
     -> a
     -> RIO r e b
   ```

5. **`provideLayer` error-row union** (Phase 5.3): the `Union e e' eOut`
   shape proposed in the plan was not exercised by this spike, but the fact
   that `Variant` already builds on row-union semantics in `purescript-variant`
   suggests the same pattern will work. Phase 5.3 should re-confirm.

6. **Backlog for v0.2:** custom `Fail` instances to improve NEG-2 and NEG-3
   error messages.

   **Update (2026-06):** done for NEG-2: `catchTag` now routes a
   missing tag through a `CatchableErrorTag` constraint whose `Fail`
   instance emits a friendly message (`RIO.Aff.Error`). NEG-3 still
   produces the raw `Prim.Row.Cons` message.

7. **No need for a custom runtime layer for inference reasons.** The
   `Record r -> Aff (...)` representation cooperates with `Cons`/`Lacks`
   exactly as needed.

   **Update (2026-06):** a custom interpreter runtime (the `Op` tree)
   was later adopted for performance, not inference; the inference
   properties described here are unchanged.

## Reproducing the Findings

The compiler only emits `MissingTypeDeclaration` warnings on a fresh build,
so wipe the output directory first:

```sh
rm -rf output
npx spago build -p spike-row-inference 2>&1 \
  | grep -A 16 "inferred type of example"
```

This prints all 10 examples with their full inferred types, including the
de Bruijn-style unification variable names used in this document. Subsequent
incremental builds will exit clean (`Warnings 0 0 0`) without re-emitting
the warnings because no modules are recompiled.

To reproduce a negative case, edit `src/Spike/RowInference/Negative.purs` to
uncomment one of the three blocks (and the imports at the top), then run
`npx spago build -p spike-row-inference`.
