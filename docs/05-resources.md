# Resources

RIO's resource-safety primitives are `acquireRelease`, `ensuring`,
and `Scope` / `scoped`. These all build on `Aff.bracket`, whose
release phase is uninterruptible by default (verified in
`spikes/aff-interruption/FINDINGS.md`, scenario S6). The guarantees
they offer:

- The release of an acquired resource runs on every path:
  success, typed failure, defect (`die` or any `Aff`
  exception), and external fiber kill.
- If `acquire` itself fails, the release does **not** run,
  because there is nothing to release. The failure propagates
  unchanged.
- The release phase is uninterruptible: a kill landing during
  the release is queued until it completes.

This document covers:

1. `acquireRelease`: the bracket-style primitive.
2. `ensuring`: the simpler "run a finalizer no matter what".
3. `Scope` / `scoped` / `addFinalizer`: LIFO finalizer stacks.
4. `RIO.Resource.Do`: qualified-do sugar that flattens nested
   brackets.
5. The failure model and the empty-row release.

## `acquireRelease`

```purescript
acquireRelease
  :: forall r e a b
   . RIO r e a              -- acquire
  -> (a -> RIO r () Unit)   -- release (empty error row)
  -> (a -> RIO r e b)       -- use
  -> RIO r e b
```

```purescript
readContents :: forall r e. String -> RIO r e String
readContents path = acquireRelease
  (liftAff (FS.openRead path))
  (\h -> liftAff (FS.close h))
  (\h -> liftAff (FS.readAll h))
```

The release's error row is `()`: cleanup cannot fail with a
typed error because there's no caller-visible place to surface
one. Defects in the release path *will* propagate as `Aff`
exceptions and are observable at the call site via
`RIO.Error.sandbox`.

If `acquire` fails (typed or defect), `release` is not called.

## `ensuring`

`ensuring` is the `try/finally` shape for cases without a
distinct acquire/use split:

```purescript
ensuring
  :: forall r e a
   . RIO r e a
  -> RIO r () Unit
  -> RIO r e a
```

```purescript
drainOnExit :: forall r e. RIO r e Report
drainOnExit = ensuring serveRequests drainPool
```

The finalizer runs in the release phase of the underlying
`Aff.finally`: uninterruptible, on every termination path. Its
error row is `()` for the same reason as
`acquireRelease`'s release.

## `Scope` and `scoped`

When several resources share a lifetime, a `Scope` collects
their finalizers and runs them all on exit:

```purescript
newtype Scope = Scope (Ref (Array (Aff Unit)))

addFinalizer :: forall r e. Scope -> Aff Unit -> RIO r e Unit

scoped
  :: forall r e a
   . RIO (scope :: Scope | r) e a
  -> RIO r e a
```

`scoped` introduces a fresh scope as a service under the label
`scope`, runs the inner program, and runs every registered
finalizer on exit:

```purescript
program = scoped do
  scope <- ask (Proxy :: Proxy "scope")
  resA <- openA
  addFinalizer scope (closeA resA)
  resB <- openB resA
  addFinalizer scope (closeB resB)
  useBoth resA resB
```

Finalizers run **LIFO**: the last registered runs first, just
like nested `acquireRelease` calls would. Every finalizer is
attempted even if a previous one throws; exceptions are swallowed
so a single bad finalizer doesn't cascade through the stack.
This trades aggregated error reporting for guaranteed release; a
future change could collect the exceptions into a `Cause` tree
without changing the semantics for the success path.

The `Scope` data constructor is exported for use inside the
library (specifically `RIO.Layer.provideLayer`, which needs to
share one scope across a layer-build phase and a program-run
phase). `RIO.Core` re-exports only the opaque type, so user
code that reaches the library through that module cannot
construct a `Scope` directly.

## `RIO.Resource.Do`: qualified-do sugar

A computation that opens several resources before using them is
a ladder of nested brackets:

```purescript
example = acquireRelease openHandle closeHandle \h ->
  acquireRelease openConn closeConn \conn ->
    acquireRelease openPool closePool \pool ->
      buildReport h conn pool
```

`RIO.Resource.Do` flattens that with a qualified-do block:

```purescript
import RIO.Resource.Do as Resource

example :: forall r e. RIO r e Report
example = Resource.do
  h    <- Resource.acquire openHandle closeHandle
  conn <- Resource.acquire openConn   closeConn
  pool <- Resource.acquire openPool   closePool
  buildReport h conn pool
```

The two expressions are equivalent. Release ordering is LIFO,
matching `acquireRelease`; every release runs on every
termination path.

Plain `RIO` statements interleaved between acquisitions need an
explicit `liftRIO` wrap because every `<-` must produce an
`Acquire`:

```purescript
Resource.do
  h <- Resource.acquire openHandle closeHandle
  _ <- Resource.liftRIO (logInfo "opened handle")
  useHandle h
```

The trailing expression that closes the block is a plain `RIO`
action that may reference all of the bound resources; the block
as a whole has type `RIO r e b` where `b` is the trailing
expression's result type.

## The failure model

The empty error row on release (`RIO r () Unit`) is a
deliberate constraint, not a limitation. A release path with a
typed failure would need a caller-visible place to put it, and
there isn't one: by the time release runs, the `acquireRelease`
or `scoped` call is either returning a value or propagating an
upstream failure. Surfacing a second, parallel failure from the
release would either:

- shadow the original (the user loses visibility into the
  underlying problem), or
- merge with it (a cause tree, which is what `RIO.Cause`
  exists for, but isn't yet wired into resource release).

The current behaviour: defects from release propagate as `Aff`
exceptions. `RIO.Error.sandbox` at the call site materialises
them. The typed channel is reserved for failures that callers
are expected to recover from; resource release isn't one of
those.

`Cause.acquireReleaseCause` is the variant that records both a
body failure and a release failure into a `Sequential` cause
tree when they both happen; it's available for callers who want
that observation. The non-Cause primitives in this module keep
their simpler shape on purpose so the common case doesn't pay
for cause-tree construction.

## Interruption guarantees

Every primitive in this module sits on `Aff.bracket` or
`Aff.finally`, whose release phase is uninterruptible:

- A fiber kill landing during release is **queued** until the
  release completes.
- A defect inside release is **caught** and reported via the
  underlying `Aff` exception channel; subsequent finalizers in
  the same `Scope` still run.
- A `delay (Milliseconds 0.0)` inside release is a natural
  cancellation point for cooperative cancellation, but the
  default is "release runs to completion".

The full evidence for these guarantees is
`spikes/aff-interruption/FINDINGS.md`, scenarios S5 and S6. The
design of `acquireRelease` and `Scope` is what those scenarios
were written to support.

## Comparison with ZIO / Effect

| Concept             | ZIO                                | Effect               | `purescript-rio`     |
| ------------------- | ---------------------------------- | ----------------------- | -------------------- |
| Bracket             | `ZIO.acquireRelease`               | `Effect.acquireRelease` | `acquireRelease`     |
| Finally             | `ZIO.ensuring`                     | `Effect.ensuring`       | `ensuring`           |
| Scope               | `Scope`                            | `Scope`                 | `Scope`              |
| Scope introduction  | `ZIO.scoped`                       | `Effect.scoped`         | `scoped`             |
| Register finalizer  | `Scope.addFinalizer`               | `Scope.addFinalizer`    | `addFinalizer`       |
| Multi-acquire sugar | `for` in scoped block              | `Effect.gen`            | `RIO.Resource.Do`    |
| Cause-aware release | `ZIO.acquireReleaseExitCause`      | `Effect.acquireExit`    | `Cause.acquireReleaseCause` |

The interrupt guarantees in this library are weaker than ZIO's
fiber-supervisor variant in one respect: defects in finalizers
are swallowed rather than collected. Everything else matches
ZIO 2.x's `Scope` semantics, including LIFO order and the
uninterruptible release phase.

## Pointers

- Source: [`src/RIO/Resource.purs`](../src/RIO/Resource.purs).
- Do-notation sugar:
  [`src/RIO/Resource/Do.purs`](../src/RIO/Resource/Do.purs).
- Spec coverage:
  [`test/Test/RIO/ResourceSpec.purs`](../test/Test/RIO/ResourceSpec.purs),
  [`test/Test/RIO/Resource/DoSpec.purs`](../test/Test/RIO/Resource/DoSpec.purs).
- Cause-aware variant:
  [`src/RIO/Cause.purs`](../src/RIO/Cause.purs)
  (`acquireReleaseCause`).
- Layers (the abstraction that orchestrates many resources):
  [`docs/04-layers.md`](./04-layers.md).
- Interrupt guarantees: `spikes/aff-interruption/FINDINGS.md`,
  scenarios S5 and S6.
- Worked examples:
  [`examples/notify/`](../examples/notify/) holds the
  `RIO.Postgres.Notify` subscriber connection inside a `scoped`
  block so the dedicated client tears down on every exit path;
  [`examples/todo-api/`](../examples/todo-api/) takes the
  `postgresLayer` + HTTPurple server through `provideLayer`, so
  the pool and the listening socket release together when the
  surrounding scope ends.
