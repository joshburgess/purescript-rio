# qualified-do spike: findings

## Goal

Try out PureScript's qualified-do syntax as ergonomic sugar over
`RIO` patterns. The question was whether any of it earns its
keep enough to ship as a sibling module in `rio` proper.

## What worked

### `Resource.do`: flattens nested `acquireRelease`

`Spike.QualifiedDo.Resource` exposes `bind` / `discard` / `pure`
plus a small `Acquire r e a` newtype carrying an
`acquire :: RIO r e a` paired with a
`release :: a -> RIO r () Unit`. With it imported qualified, you
get:

```purescript
example = Resource.do
  h <- Resource.acquire openH closeH
  c <- Resource.acquire openC closeC
  body h c
```

which desugars exactly to:

```purescript
acquireRelease openH closeH \h ->
  acquireRelease openC closeC \c ->
    body h c
```

The runtime check in this spike (`Spike.QualifiedDo.Main`) opens
two named resources, runs a use step, and verifies the LIFO
release order:

```
Resource.do events = ["open:h","open:c","use:H+C","close:c","close:h"]
nested events      = ["open:h","open:c","use:H+C","close:c","close:h"]
events match       = true
```

Verdict: the flat layout is a real readability win in code that
opens four or five resources; nested brackets at that depth slide
off the right side of the screen.

### `Par.ado`: applicative parallel via `Control.Parallel`

`Spike.QualifiedDo.Par` exposes `apply` / `map` / `pure` so
`Par.ado` desugars each independent `<-` into branches that run
concurrently under `ParAff`:

```purescript
result = Par.ado
  a <- slow 100.0 "A"
  b <- slow 100.0 "B"
  c <- slow 100.0 "C"
  in { a, b, c }
```

The spike clocks the parallel block at 102ms vs 302ms for the
sequential `do` version with the same three branches.

Verdict: the win is identical to writing the `parallel` /
`sequential` boilerplate by hand, with cleaner syntax. The only
real subtlety is that this `apply` does **not** short-circuit on
typed failure (both branches run to completion; the leftmost
failure wins). For short-circuiting fan-out, callers still want
`RIO.Aff.Concurrency.parPair` / `parTuple`.

## What didn't work

### Plain `RIO` statements inside `Resource.do`

Every `<-` in a `Resource.do` block desugars through
`Resource.bind`, whose LHS is `Acquire r e a`. A plain
`RIO r e a` won't fit, so:

```purescript
Resource.do
  h <- Resource.acquire openH closeH
  _ <- logInfo "opened h"   -- typechecks?  NO, this is RIO not Acquire
  body h
```

fails to typecheck. The spike documents the workaround:
`Resource.liftRIO :: RIO r e a -> Acquire r e a` wraps a plain
action in an `Acquire` with a no-op release, so the line above
becomes `_ <- Resource.liftRIO (logInfo "opened h")`.

This is mildly annoying. If `Resource.do` ships, the docs need to
lead with the rule "every `<-` is an `Acquire`; lift plain actions
explicitly", or users will hit this on day one.

### Generator-style direct syntax (the original motivation)

Qualified-do gives you sugar over user-chosen `bind`/`apply`/etc.
What it does **not** give you:

  * Effect-row inference at the `<-` site (no way to write
    `logger <- ask` and have the compiler infer
    `Proxy "logger"`). You still have to write `ask (Proxy ::
    Proxy "logger")`, or wait for a language feature like
    `do.notation` with custom `<-` resolution.

  * Inversion of control over the surrounding monad. Qualified-do
    is purely a desugaring; it can't change how the host
    expression composes once the block ends.

  * Implicit `atomically` lifting for STM inside RIO. You can
    write `STM.do` whose `bind` lives in `STM`, but mixing a
    `RIO` step inside the same block isn't possible without
    leaving the STM monad, which is the whole point of running
    `atomically` once around the whole transaction.

  * Anything resembling true generator syntax (`yield`,
    `await`). That needs compiler-level CPS transformation, which
    is what the PureScript ecosystem doesn't have today.

So the original "generator-style direct syntax" item in the
build plan stays unactionable. Qualified-do narrows the gap on
specific patterns (resources, applicative parallel) but is not a
replacement for the missing language feature.

## Recommendation

  * **Ship `Resource.do`** as `RIO.Aff.Resource.Do` (or a sibling
    `RIO.Aff.Do.Resource`). Real, measurable ergonomic win for
    multi-resource opens. Pair with `liftRIO` and a one-paragraph
    note about the lift rule.

  * **Ship `Par.ado`** as `RIO.Aff.Concurrency.Par` (or similar).
    The applicative-only constraint matches `ado`'s shape
    perfectly. Document the no-short-circuit semantics and point
    at `parPair`/`parTuple` for the short-circuiting case.

  * **Don't try** to fake direct-style or generator syntax with
    qualified-do. The pieces aren't there, and any attempt
    produces a leaky abstraction that will surprise users worse
    than just writing the explicit calls.

## Running the spike

```
npx spago run -p spike-qualified-do
```

Expected output: both candidates execute, events match between
`Resource.do` and nested `acquireRelease`, and `Par.ado` wall
clock is roughly `slowest-branch` rather than `sum-of-branches`.
