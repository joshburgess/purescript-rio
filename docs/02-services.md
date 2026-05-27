# Services and the Environment Row

> **Naming convention.** Code samples in this guide use the
> `RIO.Aff.*` module names and the rio-aff symbol-indexed
> spellings (`ask` / `asks` / `provide` plus `provideAll`).
> rio-fiber spells the symbol-indexed forms `askAt` / `asksAt`
> / `provideAt` so they can coexist with `RIO.Fiber.Core`'s
> whole-record `ask` / `asks` (which return the entire `Record
> r` without taking a `Proxy`). `provideAll` matches in both
> packages. The `liftAff` calls in the code samples below
> rely on rio-aff's `MonadAff` instance; rio-fiber does not
> implement `MonadAff`, so fiber callers substitute `fromAff`
> (from `RIO.Fiber.Aff`). The `LogLevel` constructor `LogInfo`
> below is rio-aff's spelling; rio-fiber spells the same
> constructor `Info` (no `Log` prefix). The `main` snippet
> below also uses `launchAff_` because rio-aff's `runRIO`
> returns `Aff`; rio-fiber's `runRIO` returns `Effect` and is
> run directly (no `launchAff_` wrapper).

RIO uses `ask` / `asks` for reading a service out of the
environment, and `provide` / `provideAll` for supplying one. This
document covers:

1. The idiomatic shape of a service.
2. How `ask` and `provide` make the environment row grow and shrink.
3. Two non-obvious traps to avoid.

The `examples/logger/` example is the running reference.

## The shape of a service

A service in RIO is a **record of operations**. Each operation returns
a concrete `Aff` (for asynchronous work) or `Effect` (for synchronous
work); it does not return `RIO r e a` and is not polymorphic over the
caller's monad.

```purescript
type Logger =
  { log :: LogLevel -> String -> Aff Unit
  }
```

This `Logger` shape is a pedagogical illustration of the service
pattern; the real `RIO.Aff.Logger.Logger` is structurally richer
(it carries scoped annotations and its `log` field returns
`Effect Unit`). See `docs/12-logging.md` for the production
shape; the rest of this section uses the simpler form above to
keep the example small.

The smart constructors that callers actually use are `RIO`-valued. They
`ask` the record out of the environment and `liftAff` the chosen
operation back into `RIO`:

```purescript
info :: forall r e. String -> RIO (logger :: Logger | r) e Unit
info msg = do
  logger <- ask (Proxy :: Proxy "logger")
  liftAff (logger.log LogInfo msg)
```

The split (concrete operations on the record, smart constructors in
`RIO`) is what makes the service ergonomic to consume. Callers write
`info "hello"`; they do not write `do l <- ask _; liftAff (l.log LogInfo "hello")`.

## Wiring it up

You build a concrete implementation of the service and hand it to
`provide` or `provideAll`. Nothing here is reflection; it's just record
construction:

```purescript
consoleLogger :: Logger
consoleLogger =
  { log: \lvl msg -> Console.log (prefix lvl <> msg)
  }

main = launchAff_ do
  result <- runRIO (provideAll { logger: consoleLogger } program)
  ...
```

For partial wiring (some services known up front, others injected
later), use `provide` one service at a time:

```purescript
-- program :: RIO (logger :: Logger, db :: Database) e a
withLoggerOnly :: RIO (db :: Database) e a
withLoggerOnly = provide (Proxy :: Proxy "logger") consoleLogger program
```

After `provide`-ing `logger`, the resulting computation no longer
requires it; the inferred row has shrunk by one field. `provideAll`
shrinks it to `()` in one step.

## How the row grows and shrinks

  * `ask (Proxy :: Proxy "k") :: RIO (k :: T | r) e T`. The requirement
    aggregates into the inferred row.
  * `do { a <- ask _k1; b <- ask _k2; ... }`'s row carries both `k1` and
    `k2`, automatically.
  * `provide _k v inner` shrinks the row by exactly one field.
  * `provideAll env inner` shrinks the row to `()`.

The compiler does the bookkeeping. You do not write out the row of
required services manually except in top-level signatures (and even
there only when you want to fix the row's shape for documentation or
to nail down inference; the row-inference spike confirmed the
patterns here all infer cleanly without explicit annotations).

## Trap 1: don't make service operations polymorphic over `m`

A natural-looking but bad design:

```purescript
-- DON'T do this.
type Logger =
  { log :: forall m. MonadAff m => LogLevel -> String -> m Unit
  }
```

The intention is "any caller in any `MonadAff` can call it". The reality
is that PureScript's row solver cannot project a rank-N field out of a
record at a concrete instantiation, and the call site
`logger.log LogInfo "hello"` fails to infer `m`. The error is verbose and
points at the wrong place.

Keep operations at concrete `Aff` (or `Effect`) and lift them in the
smart constructors. The polymorphism that callers care about
(`I can use this from any RIO program`) is provided by `ask` plus
`liftAff` at the *call* site, not by the field's type.

## Trap 2: don't reach for `asks` to project a polymorphic operation

For a service whose operations are monomorphic, `asks` is convenient:

```purescript
getPort :: forall r e. RIO (config :: { port :: Int } | r) e Int
getPort = asks (Proxy :: Proxy "config") _.port
```

For a service whose operations are functions, prefer the two-step
`ask` + apply form shown above. Even with operations at concrete `Aff`,
applying a projector through `asks` requires the projected value to be
unifiable with the rest of the do-block's monad, and the inference path
through `asks` is more fragile than through plain `ask` + record-field
access. In particular, when an operation in `Aff` is `liftAff`-ed into
`RIO`, going through `ask` keeps the operation's `Aff` type visible to
the elaborator one step longer.

## Running the example

```sh
npx spago run -p rio-example-logger
```

Expected output:

```
[info]  starting up
[warn]  this is a warning
[error] and an error, for variety
[info]  done
example: ok
```

Source under `examples/logger/`.

## Composing services

A program that uses two services from disjoint rows produces a program
whose row is the union, inferred automatically:

```purescript
both :: forall r e. RIO (logger :: Logger, db :: Database | r) e Unit
both = do
  info "starting query"
  rows <- queryAll
  info ("got " <> show (length rows) <> " rows")
```

You did not have to write `logger :: Logger, db :: Database` yourself
unless you wanted to nail down the top-level signature; the body alone
would force inference into the same shape. The row-inference spike
documented exactly which composition patterns infer this cleanly and
which need annotations; in summary, do-blocks always do, point-free
compositions sometimes need help, and `provide` and `provideAll` both
shrink the row in a way that infers without annotation in every case
exercised by the spike.

## What's next

  * [`03-errors.md`](./03-errors.md): typed error handling via
    `catchTag` / `catchAll` / `mapError`. The `e` row narrows on
    catch the same way the `r` row narrows on `provide`.
  * [`05-resources.md`](./05-resources.md): resource safety
    (`acquireRelease`, `scoped`).

## Pointers

- Source: `ask` / `asks` / `provide` / `provideAll` live in
  [`rio-aff/src/RIO/Aff/Env.purs`](../rio-aff/src/RIO/Aff/Env.purs)
  and are re-exported through
  [`rio-aff/src/RIO/Aff/Core.purs`](../rio-aff/src/RIO/Aff/Core.purs).
  The fiber-side equivalent is
  [`rio-fiber/src/RIO/Fiber/Env.purs`](../rio-fiber/src/RIO/Fiber/Env.purs).
- Spec coverage:
  [`rio-aff/test/Test/RIO/Aff/EnvSpec.purs`](../rio-aff/test/Test/RIO/Aff/EnvSpec.purs)
  pins `ask` / `asks` / `provide` / `provideAll` against
  several row shapes.
- Layers (`Layer rIn e rOut`, the unit of service wiring at the
  top of `main`): [`docs/04-layers.md`](./04-layers.md).
- Worked example:
  [`examples/logger/`](../examples/logger/) is the minimum
  service-shape walk-through;
  [`examples/todo-api/`](../examples/todo-api/) composes
  several services (`Logger`, `Clock`, `Postgres`, `Local`)
  through `appLayer`.
