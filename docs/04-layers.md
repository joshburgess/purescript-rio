# Layers

> **Naming convention.** Code samples in this guide use the
> `RIO.Aff.*` module names. The same concepts exist under
> `RIO.Fiber.*` for the premier rio-fiber package, with several
> renames worth knowing: rio-aff's `fromRecord` is rio-fiber's
> `fromValue`; rio-aff's `provideLayer` is rio-fiber's
> `provide` (both open a fresh scope internally and keep it
> open for the inner program), and rio-fiber's `provideScoped`
> is a convenience alias for `provide (scoped build)` for
> inline scope-consuming build functions. rio-fiber has no
> direct counterpart to rio-aff's `buildLayer` (the "build the
> record once, close the scope, return the record" runner);
> use `provide` with a thin program if you need that shape.
> rio-aff's composition
> combinators `andThen` (infix `>>>`) and `combine` (infix
> `<+>`) are rio-fiber's `chainLayer` and `mergeLayers` (rio-fiber
> defines no infix-operator aliases for these). Where the
> walkthrough uses an rio-aff name, rio-fiber readers substitute
> the matching rio-fiber name. The symbol-indexed reader
> `ask (Proxy ..)` inside layer bodies is `askAt (Proxy ..)` in
> rio-fiber, and the `fromRIO` examples that pull a scope via
> `ask (Proxy :: Proxy "scope")` should be read against
> rio-aff's `scoped`-injects-env-row shape; rio-fiber's `scoped`
> hands the `Scope` to the body as a lambda argument instead
> (see `docs/05-resources.md`). The `addFinalizer scope
> closeFoo` calls in the layer-body examples take an `Aff Unit`
> finalizer in rio-aff but `Effect Unit` in rio-fiber; the
> rio-fiber-side `addFinalizerRIO` is the equivalent for a
> `RIO r () Unit` finalizer.

A `Layer rIn e rOut` is a recipe for constructing a record of
services `rOut` from a record of services `rIn`, possibly
failing with a typed error in `Variant e`. Layers compose
vertically (`andThen`, infix `>>>`) and horizontally (`combine`,
infix `<+>`); they may register finalizers in the surrounding
scope so resources are released when the providing scope exits.

Layers build on RIO's resource safety (`acquireRelease`, `Scope`,
`scoped`): every layer runs inside a `Scope` so resource-owning
layers are safe by construction. This document covers:

1. Constructing layers (`fromRecord`, `fromRIO`).
2. Composing them (`andThen`, `combine`, `passthrough`).
3. Running them (`buildLayer`, `provideLayer`).
4. The failure model (the `Union` constraint on the error row).
5. How layers interact with `Scope`.

The source is `rio-aff/src/RIO/Aff/Layer.purs` (the fiber-side
mirror is `rio-fiber/src/RIO/Fiber/Layer.purs`). The qualified-do
sugar for resources, frequently used inside layer bodies, is
`rio-aff/src/RIO/Aff/Resource/Do.purs` (see also
`docs/05-resources.md`).

## Constructing layers

The simplest layer is one whose services are statically known:

```purescript
import RIO.Aff.Layer (Layer, fromRecord)

consoleLoggerLayer
  :: forall rIn e. Layer rIn e (logger :: Logger)
consoleLoggerLayer = fromRecord
  { logger: { log: \msg -> liftEffect (Console.log msg) } }
```

`fromRecord` leaves the input row free, so this layer composes
into any context. The output row says "I produce a `logger`
service".

For layers that need to read other services, allocate state, or
register finalizers, `fromRIO` is the constructor:

```purescript
import RIO.Aff.Layer (Layer, fromRIO)

counterStoreLayer
  :: forall rIn e. Layer rIn e (counter :: { incr :: Aff Int })
counterStoreLayer = fromRIO do
  ref <- liftEffect (Ref.new 0)
  pure { counter: { incr: liftEffect (Ref.modify (_ + 1) ref) } }
```

The `RIO` action inside `fromRIO` runs in
`(scope :: Scope | rIn)` in rio-aff: it can `ask` for upstream
services, lift `Aff`, and register finalizers with the
surrounding scope. In rio-fiber, `fromRIO` discards the scope
(its definition is `fromRIO build = Layer (\_ -> build)`), so
the body runs in plain `rIn` and cannot register finalizers
on the build scope; use rio-fiber's `scoped` constructor
(which takes the scope as an argument) when you need to attach
finalizers to a layer's resources. What `fromRIO` returns in
either package is the produced record.

## Sequential composition: `andThen` / `>>>`

`andThen` plumbs the first layer's output into the second
layer's input:

```purescript
-- configLayer :: Layer () e (config :: Config)
-- dbLayer     :: Layer (config :: Config) e (db :: Database)
appLayer :: Layer () e (db :: Database)
appLayer = configLayer >>> dbLayer
```

Both layers run in the same surrounding scope, so finalizers
from either fire (LIFO) when that scope exits. If the first
layer fails, the second never runs. If either fails the failure
propagates unchanged on the shared error row.

The infix `>>>` shadows `Control.Semigroupoid.(>>>)` from
`Prelude` when both are imported. Hide one or reach for the
named form (`andThen`) when both are needed in the same module.

## Horizontal composition: `combine` / `<+>`

`combine` runs two layers with the same input requirements and
unions their outputs:

```purescript
infraLayer
  :: forall e
   . Layer () e (logger :: Logger, store :: Store)
infraLayer = consoleLoggerLayer <+> inMemoryStoreLayer
```

Both layers run in the same scope. Their output rows must be
disjoint; sharing a label produces an ill-formed combined row
and the compiler rejects the call.

## Carrying inputs forward: `passthrough`

`>>>` "consumes" the input row: `configLayer >>> dbLayer` has
output `(db :: Database)`, with `(config :: Config)` no longer
visible downstream. When downstream code wants both:

```purescript
appLayer :: Layer () e (config :: Config, db :: Database)
appLayer = configLayer >>> passthrough dbLayer
```

`passthrough` adds the layer's input row back into its output
via a `Union rOut rIn rPassed` constraint. If the input and
output rows aren't disjoint the compiler rejects the call.

## Running layers: `buildLayer`

`buildLayer` opens a fresh scope, runs the layer, and hands back
the produced record:

```purescript
buildLayer
  :: forall e rOut
   . Layer () e rOut
  -> Aff (Either (Variant e) (Record rOut))
```

The scope opens and closes *inside* `buildLayer`, so any
finalizers the layer registered fire **before** the function
returns. That makes `buildLayer` appropriate for stateless test
layers (a recording logger, a static config record) but unsafe
for resource-owning layers: the returned services would
reference resources that have already been released. Reach for
`provideLayer` instead in that case.

## Running layers safely: `provideLayer`

`provideLayer` is the resource-safe runner. It builds the
layer, feeds the services into a program, and runs the program,
all inside one shared scope:

```purescript
provideLayer
  :: forall rIn rOut e e' eOut a
   . Union e e' eOut
  => Layer rIn e rOut
  -> RIO rOut e' a
  -> RIO rIn eOut a
```

The scope spans the entire call: layer-registered finalizers
run *after* the inner program completes, on every termination
path (success, typed failure, defect, fiber kill). The
underlying cancellation guarantee comes from `Aff.bracket`'s
uninterruptible release phase (verified by
`spikes/aff-interruption/FINDINGS.md`, scenario S6).

```purescript
main :: Effect Unit
main = launchAff_ do
  result <- runRIO (provideLayer appLayer program)
  case result of
    Right a -> ...
    Left v -> ...
```

The `Union e e' eOut` constraint unifies the layer's typed
failures and the program's typed failures into one output error
row.

## The failure model

A layer's error row is the failures its build phase can raise
(e.g. `dbConnect :: String` if connecting fails). A program's
error row is the failures its body can raise. `provideLayer`
unions both via `Union e e' eOut`.

- If the layer fails, the program never runs and the failure
  surfaces under the layer's tag.
- If the layer succeeds and the program fails, the program's
  failure surfaces under its own tag.
- A `Variant` is open on its row, so the inferred output row at
  the call site grows monotonically as more layers are stacked.

Defects (`die`, JavaScript exceptions, fiber kills) flow
through `Aff` and are observable via `RIO.Aff.Error.sandbox`
(rio-fiber: `RIO.Fiber.Error.causeOf`, which returns
`Either (Cause e) a` so defects surface as `Die` constructors
in the cause tree) at the call site; they bypass the typed
`Variant` channel by design.

## Resource-safe layers

A layer that opens a resource is built with `fromRIO` plus
`addFinalizer`:

```purescript
import RIO.Aff.Resource (Scope, addFinalizer)

dbLayer :: Layer (config :: Config) (dbConnect :: String) (db :: Database)
dbLayer = fromRIO do
  cfg <- ask (Proxy :: Proxy "config")
  scope <- ask (Proxy :: Proxy "scope")
  conn <- openConnection cfg
  addFinalizer scope (closeConnection conn)
  pure { db: connectionToDatabase conn }
```

When this layer is plumbed via `provideLayer`, the connection
opens, the program runs, and the connection closes. If the
program dies mid-flight the connection still closes because the
finalizer runs in the release phase of `provideLayer`'s
underlying `bracket`.

If acquiring fails (the `openConnection` call raises), no
finalizer is registered, and the layer's typed failure
propagates. The matching mental model is the same as
`acquireRelease`: nothing was opened, so nothing needs closing.

## Comparison with ZIO / Effect

| Concept                  | ZIO                     | Effect               | `rio-aff`                   | `rio-fiber`                 |
| ------------------------ | ----------------------- | ----------------------- | --------------------------- | --------------------------- |
| Layer type               | `ZLayer[RIn, E, ROut]`  | `Layer<RIn, E, ROut>`   | `Layer rIn e rOut`          | `Layer e rIn rOut`          |
| Build from value         | `ZLayer.succeed`        | `Layer.succeed`         | `fromRecord`                | `fromValue`                 |
| Build from effect        | `ZLayer.fromZIO`        | `Layer.effect`          | `fromRIO`                   | `fromRIO`                   |
| Sequential composition   | `>>>`                   | `Layer.provide`         | `>>>` (`andThen`)           | `chainLayer`                |
| Horizontal composition   | `++`                    | `Layer.merge`           | `<+>` (`combine`)           | `mergeLayers`               |
| Carry inputs forward     | `>+>`                   | `Layer.provideMerge`    | `passthrough`               | `passthrough`               |
| Run                      | `ZIO.provideLayer`      | `Effect.provide`        | `provideLayer`              | `provide` / `provideScoped` |
| Resource safety          | `ZLayer.scoped`         | `Layer.scoped`          | built-in via `Scope`        | built-in via `Scope`        |

The output-row union via `Union` is the row-polymorphism
analogue of ZIO's `RIn` / `ROut` parameters; the `Variant` error
row is the analogue of ZIO's `E` channel.

## Pointers

- Source: [`rio-aff/src/RIO/Aff/Layer.purs`](../rio-aff/src/RIO/Aff/Layer.purs)
  and [`rio-fiber/src/RIO/Fiber/Layer.purs`](../rio-fiber/src/RIO/Fiber/Layer.purs).
- Spec coverage:
  [`rio-aff/test/Test/RIO/Aff/LayerSpec.purs`](../rio-aff/test/Test/RIO/Aff/LayerSpec.purs).
- Resources (the safety primitives layers build on):
  [`docs/05-resources.md`](./05-resources.md).
- Worked layered example:
  [`examples/todo-api/`](../examples/todo-api/).
