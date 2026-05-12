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
compiler reports:

```
Could not match type
  Int
with type
  String
while matching label parse
while trying to match type
                             ( parse :: Int
                             ...
                             | e0
                             )
  with type
              ( parse :: String
              ...
              )
while solving type class constraint
  Prim.Row.Cons "parse"
                Int
                e0
                ( parse :: String
                )
```

**Quality: ACCEPTABLE (NOISY).** The first three lines say the right
thing ("Could not match Int with String while matching label parse").
The trailing "while solving type class constraint Prim.Row.Cons …"
block is correct but jargon. A new user who has not internalised
`Prim.Row` machinery may bounce off it.

**Candidate for a custom `Fail` instance in v0.2:** a `Fail` instance
attached to the `Cons sym a e' e` constraint that says *"the handler
for tag `parse` expects a `String` payload, but you wrote a handler
that takes `Int`."* That sentence already lives in the first three
lines of the current error; the goal is to lead with it and suppress
the row-machinery context.

## Patterns we have NOT yet captured

These are the next batch to add when the next phase's review revisits
this file:

- `catchTag` for a tag that isn't in the error row. (Same shape as
  case 03; quality likely similar.)
- `mapError` that translates to a row missing a label the original
  could produce. (`Cons` lookup failure on the dual side.)
- `provide` for a label that already exists in the row (the trap
  documented in `docs/02-services.md`'s mention of `Cons` strictness).

None of these are critical right now: the existing error messages
are intelligible after a short induction. They go on the v0.2 backlog
as candidates for `Fail` polish.
