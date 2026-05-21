# Errors and Error Narrowing

`fail` is the raising side of the typed-error channel;
`catchTag` / `catchAll` / `mapError` / `rethrow` are the handling
side. This document covers:

1. The four shapes of handler: `catchTag`, `catchAll`, `mapError`, and
   `rethrow`.
2. A worked example whose error row shrinks from three tags to zero,
   with the **compiler's actual inferred type** quoted at each step.
3. Defects (`die`, `sandbox`, `unsandbox`) and how they differ from
   typed failures.

The fixture `spikes/phase-2-review/src/Spike/ErrorsDocFixture.purs` is
the source of truth for the inferred types quoted in section 2. Build
the spike on a fresh output directory to regenerate them.

## A quick map of the handlers

| Combinator   | What it catches                  | Effect on the error row     |
|--------------|----------------------------------|-----------------------------|
| `catchTag`   | One named tag                    | Removes that one tag        |
| `catchAll`   | Any failure                      | Replaces the row entirely   |
| `mapError`   | Any failure (pure handler)       | Replaces the row entirely   |
| `rethrow`    | Nothing (re-raises a `Variant`)  | Same row (it is `fail`-ish) |

The first three all shrink the row in some way; `rethrow` is the
"identity" handler you'd use inside `catchAll` when you want to look at
a failure, decide whether to handle it, and pass the rest along.

## Narrowing the row, one tag at a time

The fixture defines a program that can fail in three ways:

```purescript
type ThreeErrors =
  ( notFound :: { id :: Int }
  , parse :: String
  , unauthorized :: Unit
  )

step0 :: forall r. RIO r ThreeErrors Int
step0 = do
  _ <- fail (Proxy :: Proxy "notFound") { id: 99 }
  _ <- fail (Proxy :: Proxy "parse") "bad json"
  fail (Proxy :: Proxy "unauthorized") unit
```

We then handle the failures one at a time. Each step below has **no
top-level signature**; the compiler reports the inferred type as a
`MissingTypeDeclaration` warning. The quoted blocks are taken verbatim
from those warnings.

### Step 1: handle `notFound`

```purescript
step1 = catchTag (Proxy :: Proxy "notFound") (\_ -> pure 0) step0
```

Inferred type:

```
forall r46.
  RIO r46
    ( parse :: String
    , unauthorized :: Unit
    )
    Int
```

The `notFound` tag is gone from the error row. `parse` and
`unauthorized` remain. The environment row stays polymorphic in `r46`.

### Step 2: handle `parse`

```purescript
step2 = catchTag (Proxy :: Proxy "parse") (\_ -> pure (-1)) step1
```

Inferred type:

```
forall r59.
  RIO r59
    ( unauthorized :: Unit
    )
    Int
```

Only `unauthorized` is left.

### Step 3: handle `unauthorized`

```purescript
step3 = catchTag (Proxy :: Proxy "unauthorized") (\_ -> pure (-2)) step2
```

Inferred type:

```
forall r73. RIO r73 () Int
```

The error row is exactly `()`. Nothing can fail any more (typed-wise),
and the program is runnable with `runRIO'`:

```purescript
runStep3 = runRIO' step3
```

Inferred type:

```
Aff Int
```

`runRIO'` accepts only `RIO () () a` and produces `Aff a`, no `Either`
wrapper, because the error row is uninhabited.

## What `catchAll` and `mapError` add

`catchTag` is the right tool when a handler is specific to one failure.
For cross-cutting strategies, two coarser combinators replace the row
wholesale.

### `catchAll` for a single handler over every tag

```purescript
catchAll
  :: forall r e e' a
   . (Variant e -> RIO r e' a)
  -> RIO r e a
  -> RIO r e' a
```

The handler runs on the raw `Variant`. Use it for:

- **Log-and-rethrow:** inspect every failure, log it, decide whether to
  re-raise via `rethrow` or convert to a value.
- **Default value on any failure:** `catchAll (\_ -> pure defaultValue)`.
- **Translate to a different error scheme** when the new row's shape
  isn't structurally derived from the old one.

Identity property: `catchAll rethrow ≡ id`. See
`test/Test/RIO/ErrorHandlingSpec.purs` for the exercised version.

### `mapError` for a pure translation

```purescript
mapError
  :: forall r e e' a
   . (Variant e -> Variant e')
  -> RIO r e a
  -> RIO r e' a
```

When the new failure shape is a direct translation of the old (no new
effects, no service reads), `mapError` says exactly that. Useful at
module boundaries where you want a layer's failures to surface as the
calling layer's vocabulary.

## Defects: failures that bypass the row

Some failures are not part of the program's domain: a JSON
serialisation that should always succeed, an array index that should
always be in bounds, a programmer's invariant that was supposed to
hold. RIO models these as **defects**: they ride the underlying `Aff`'s
exception channel, not the typed-error row.

### `die` to raise one

```purescript
die :: forall r e a. Error -> RIO r e a
```

`die` ignores the error row (`e` stays polymorphic; the defect doesn't
add to it). It also ignores the rest of the row, the environment, and
any pending continuations:

```purescript
program :: RIO () (notFound :: NotFound) Int
program = die (error "this should never happen")
```

`runRIO program` will not produce `Left`; it will produce an `Aff` that
rejects with the error. Defects are visible only by `attempt`-ing the
returned `Aff` (or by `sandbox`).

### `sandbox` to reify them

```purescript
sandbox :: forall r e a. RIO r e a -> RIO r e (Either Error a)
```

`sandbox` runs the inner program and:

- **Success → `Right (Right a)`.** Normal path.
- **Defect → `Right (Left err)`.** The defect is now a value, visible
  to the rest of the program.
- **Typed failure → `Left v`.** Typed failures are **not** absorbed;
  they continue to propagate. `sandbox` is for defects only.

Note the error row is unchanged: `sandbox` doesn't lie about typed
failures, only adds visibility for defects.

### `unsandbox` to put a defect back

```purescript
unsandbox :: forall r e a. RIO r e (Either Error a) -> RIO r e a
```

If a sandbox-style result has an inner `Left err`, `unsandbox`
re-raises it as a defect. Use it to round-trip: sandbox, inspect or
log the defect, decide whether to swallow it or `unsandbox` to let it
keep flowing.

## When to fail typed and when to die

A rough rule:

- **`fail`** if the caller has a reasonable response. "User not
  found", "request body unparseable", "rate limit exceeded": the caller
  can pattern-match on the tag and do something sensible.

- **`die`** if the caller has no reasonable response. "Internal
  invariant broken", "deserialisation of a value we just serialised
  failed": the answer is "crash the request, log the bug, page someone."

The error row is for the first set. The defect channel is for the
second. Mixing them on purpose (typed failures that the surrounding
code expects to handle) is fine. Mixing them by accident (a defect
where you meant `fail`) becomes a runtime surprise that the type
system won't catch.

## Pointers

- Source:
  [`rio-aff/src/RIO/Aff/Error.purs`](../rio-aff/src/RIO/Aff/Error.purs)
  and
  [`rio-fiber/src/RIO/Fiber/Error.purs`](../rio-fiber/src/RIO/Fiber/Error.purs).
- Spec coverage:
  [`rio-aff/test/Test/RIO/Aff/ErrorHandlingSpec.purs`](../rio-aff/test/Test/RIO/Aff/ErrorHandlingSpec.purs)
  pins `catchTag` / `catchAll` / `mapError` / `rethrow` / `sandbox`;
  [`rio-aff/test/Test/RIO/Aff/FailSpec.purs`](../rio-aff/test/Test/RIO/Aff/FailSpec.purs)
  covers `fail` and `die`; the
  [`rio-aff/test/Test/RIO/Aff/Error/`](../rio-aff/test/Test/RIO/Aff/Error/)
  subdirectory adds `catchSome`, `orElse`, `refine`, and `tap`
  combinator coverage.
- Defect channel and the `Cause` tree it lands in:
  [`docs/14-causes.md`](./14-causes.md).
- Worked example:
  [`examples/worker-pool/`](../examples/worker-pool/) raises a
  `jobFailed :: String` typed failure from each worker, retries
  on it via `RIO.Aff.Schedule.retry`, and runs a
  `parTraverseCause` pre-flight pass so multiple validation
  errors render as a `Parallel` cause tree.
