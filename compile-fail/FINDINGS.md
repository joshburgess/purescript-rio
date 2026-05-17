# Compile-fail error-message quality audit

A running record of how the compiler reports each `compile-fail` case.
Phase 2's review cycle introduced the first case; Phase 3's review
adds the error-handling mistakes most likely to bite a new user. The
goal is not to pin the exact error text (that's the test driver's job
via `.expected` substring matching) but to track **readability**: would
a user new to RIO understand what went wrong?

Where the answer is "acceptable but noisy," we leave a marker. Those
markers feed the v0.2 backlog item "Improved compiler error messages
via custom `Fail` instances."

## Case 01: `provide` with a wrong-typed value (Phase 2.2)

A value of the wrong type passed to `provide`. The compiler reports:

```
Could not match type
  Int
with type
  { name :: String
  }
while trying to match type
                             ( logger :: Int
                             ...
                             | t0
                             )
  with type
              ( logger :: { name :: String
                          }
              )
while solving type class constraint
  Prim.Row.Cons "logger" Int t0 ( logger :: { name :: String } )
```

**Quality: GOOD.** The mismatched types are named on the first two
lines; the rows are shown side by side. A user reads "I passed an Int
where a `{ name :: String }` was wanted under the label `logger`" and
fixes it.

## Case 02: `runRIO'` with a leftover error tag (Phase 3.1/3.2)

A program with a typed failure handed directly to `runRIO'`. The
compiler reports:

```
Could not match type
  ( boom :: Unit
  )
with type
  ()
while trying to match type RIO (() @Type)
                             ( boom :: Unit
                             )
  with type RIO (() @Type) (() @Type)
```

**Quality: GOOD.** The leftover tag (`boom :: Unit`) is named on the
first line; the target shape (`()`) is named on the third. A user
immediately sees "I have an unhandled failure called `boom`; either
handle it or call `runRIO` instead."

## Case 03: `catchTag` with a wrong payload type (Phase 3.1)

A handler that claims the wrong type for the tag's payload. The
compiler reports (after the `FindErrorTag` row-list walk shipped in
`RIO.Error`):

```
Could not match type
  String
with type
  Int
while solving type class constraint
  RIO.Error.FindErrorTag "parse"
                         t6
                         Int
while applying a function catchTag
  of type IsSymbol t0 => CatchableErrorTag t0 t1 t2 => Cons … =>
          Proxy t0 -> (t1 -> RIO t4 t3 t5) -> RIO t4 t2 t5 -> RIO t4 t3 t5
```

**Quality: GOOD.** The mismatched payload types are named on the
first two lines (`String` vs `Int`) and the row-list lookup names
the tag (`"parse"`). A reader skimming the error sees "my handler
took `Int` but tag `parse` carries `String`" and fixes it.

This case used to be ACCEPTABLE (NOISY) because the underlying
`Prim.Row.Cons` constraint dragged a `( parse :: Int | e0 )` vs
`( parse :: String )` row mismatch into the top of the error. The
`FindErrorTagInRow` / `FindErrorTag` row-list walk added in
`RIO.Error` lifts the payload-type lookup into a single constraint
keyed by symbol, so the wrong-typed handler now produces a clean
"Could not match" pointed at the two payload types directly.

## Case 04: `catchTag` for a tag that isn't in the error row

`inner :: RIO () (parse :: String) Int`, called with
`catchTag (Proxy :: Proxy "notFound") handler inner`. The compiler
reports:

```
Could not match type
  ( notFound :: t1
  ...
  | e6
  )
with type
  ( parse :: String
  ...
  )
while solving type class constraint
  Prim.Row.Cons "notFound"
                t1
                e6
                ( parse :: String
                )
```

**Quality: ACCEPTABLE (NOISY).** The compiler shows the two rows side
by side and the user can read "I asked to peel `notFound` off a row
that only contains `parse`." The trailing `Prim.Row.Cons` block is
correct but more jargon than a new user needs.

The `FindErrorTagInRow` row-list walk in `RIO.Error` defines a Fail
instance for the "tag missing from row" case (its message reads
`RIO.catchTag: the error tag '…' is not present in the error row.`),
but the `Prim.Row.Cons` constraint on `catchTag`'s signature
(needed for the residual-row calculation and for `Variant.on`
inside the body) fires its row-mismatch error first at the use
site and shadows the friendlier Fail.

We investigated two restructures that could in principle let the
friendly Fail win:

1. **Fold `Row.Cons` into the `CatchableErrorTag` instance head.**
   The user-facing signature shrinks to one constraint, but the
   compiler still reports the `Prim.Row.Cons` mismatch from
   inside that head before reaching the row-list walk's `Fail`.
   No change to the user-visible error.

2. **Replace `Row.Cons` with a custom row-list walk that removes
   the tag and rebuilds the residual row via `FromRowList`.**
   This avoids the "subtract a missing label" mode of `Row.Cons`
   entirely, and the friendly `Fail` does win for closed rows.
   But the rebuild requires `Prim.RowList.RowToList`, which is
   only defined for closed rows. Real `catchTag` call sites work
   on programs with open error rows (e.g. `RIO r (database ::
   DatabaseError | s) a` from `failWith`), so this approach
   regresses the happy path with "No type class instance was
   found for `RowToList`".

The fundamental issue is that `Prim.Row.Cons` is symmetric and
its mismatch error fires eagerly, while open-row support requires
the symmetric subtraction direction it provides. No restructure
within today's PureScript type system has let us suppress the
mismatch without losing open-row support. We leave the residual
jargon in place pending either an upstream improvement to
`Prim.Row.Cons` error reporting or a fundamentally different
encoding of typed-error rows.

## Case 05: `mapError` followed by `runRIO'` with a non-empty residual row

`inner :: RIO () (parse :: String) Int`; `mapError` rewrites the
`parse` failure into `notFound :: Unit`; the program is then passed
to `runRIO'`, which wants `RIO () () a`. The compiler reports:

```
Could not match type
  ( notFound :: Unit
  )
with type
  ()
while trying to match type RIO () (notFound :: Unit)
  with type RIO () ()
```

**Quality: GOOD.** The mismatch is on the second and third lines
verbatim: `(notFound :: Unit)` vs `()`. A user reads "my error row
still has a `notFound` tag in it" and either keeps catching or
swaps `runRIO'` for `runRIO`.

## Case 06: `provideAll` for a record missing a required field

`inner :: forall e. RIO ( logger :: { name :: String }, requestId :: String ) e String`,
handed to `provideAll { logger: { name: "outer" } }` (no `requestId`).
The compiler reports:

```
Could not match type
  ( logger :: { name :: String }, requestId :: String )
  ...
with type
  ( logger :: { name :: String } )
  ...
while trying to match type RIO
                             ( logger :: { name :: String }
                             , requestId :: String
                             )
  with type RIO ( logger :: { name :: String } )
```

**Quality: GOOD.** The full row appears on both sides of the
mismatch; the missing label (`requestId`) is the obvious diff a
visual scan picks out.

## Case 07: `Layer >>>` chain with mismatched intermediate row

`configLayer :: Layer rIn e (config :: Config)`,
`dbLayer :: Layer (dsn :: String) e (db :: Database)`. The chain
`configLayer >>> dbLayer` must reject because the first layer's
output row `(config :: Config)` is not the second layer's input
row `(dsn :: String)`. The compiler reports:

```
Could not match type
  ( dsn :: String
  ...
  )
with type
  ( config :: { host :: String
              }
  ...
  )
while trying to match type Layer ( dsn :: String )
  with type Layer ( config :: { host :: String } )
```

**Quality: GOOD.** The two intermediate rows appear directly. A
user reads "I wired a layer whose input is `dsn` to one whose
output is `config`" and either inserts a translator or fixes the
upstream layer.

## Case 08: `Layer <+>` with overlapping output labels

Both `consoleLogger` and `fileLogger` produce `(logger ::
Logger)`. The combined output row would carry `logger` twice;
`Row.Union` (the constraint behind `combine` / `<+>`) cannot
unify the duplicate against the caller-annotated singleton. The
compiler reports:

```
while solving type class constraint
  Prim.Row.Union ( logger :: ... )
                 ( logger :: ... )
                 ( logger :: ... )
while applying a function combine
```

**Quality: GOOD.** The three rows in the `Union` constraint
make the duplicate explicit: a user reads "both sides produce
`logger`, the union wants to be just one `logger`" and either
renames one output or picks a single source of truth.

## Case 09: `runRIO'` with a leftover service requirement

`inner :: forall e. RIO (logger :: Logger) e String` handed
directly to `runRIO' :: RIO () () a -> Aff a`. The unsatisfied
service row surfaces as:

```
Could not match type
  ( logger :: { name :: String
              }
  )
with type
  ()
while trying to match type RIO ( logger :: { name :: String } )
  with type RIO (() @Type)
```

**Quality: GOOD.** Mirrors case 02 for the error row: the
leftover row is named on the first line, the target shape on
the third. A user reads "I never provided `logger`" and inserts
`provide` / `provideAll` before the `runRIO'` call.

## Patterns we have NOT yet captured

These remain on the v0.2 `Fail`-polish backlog:

- `provide` called with a label that ALREADY exists in the inner
  row. This case turns out to typecheck under row polymorphism (the
  outer label is added to a fresh-row tail, not the same row), so
  it isn't a compile-fail target at all; the "you provided twice"
  trap is at most a warning candidate, not an error.

Case 03 was promoted from ACCEPTABLE to GOOD by the
`FindErrorTagInRow` / `FindErrorTag` work in `RIO.Error`. Case 04
remains ACCEPTABLE-NOISY: the row-list walk does carry a
custom Fail for the missing-tag case, but the parallel
`Prim.Row.Cons` constraint that `catchTag` still needs (for the
residual row and for `Variant.on`'s dispatch) fires its
row-mismatch first at the use site. The investigation under
case 04 above documents two restructures we tried and why
each was a dead end.
