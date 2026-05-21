# Migrating from ZIO

This guide maps ZIO idioms to RIO. RIO is a PureScript port of
the same pattern (a triple-parameter effect type with a typed
environment, a typed error channel, and a success channel) so
most idioms transfer one-to-one with a small adjustment for
PureScript's row types.

If you're new to RIO, the docs in `docs/01-core-type.md`
through `docs/07-testing.md` walk the model from first
principles. This page is for ZIO users who want a quick
reference.

## The core type

ZIO uses three slots: requirements, errors, success.

```scala
trait ZIO[-R, +E, +A]
```

RIO uses the same three slots, but the requirements and errors
are PureScript rows rather than intersection / union types.

```purescript
newtype RIO r e a = ...
-- r is a row of services, e.g. (logger :: Logger, db :: Database)
-- e is a row of typed errors, e.g. (notFound :: Int, parse :: String)
-- a is the success type
```

In ZIO `R` is read as "the set of services I require"; in RIO
`r` is read as "the record of services I require, indexed by
name". The empty environment in ZIO is `Any`; in RIO it is the
empty row `()`. The empty error in ZIO is `Nothing`; in RIO it
is the empty row `()`.

| ZIO                               | RIO                                   |
| --------------------------------- | ------------------------------------- |
| `ZIO[Any, Nothing, A]`            | `RIO () () a`                         |
| `ZIO[Any, E, A]`                  | `RIO () e a`                          |
| `ZIO[R, Nothing, A]`              | `RIO r () a`                          |
| `ZIO[R, E, A]`                    | `RIO r e a`                           |
| `UIO[A]` (alias for first)        | `RIO () () a`                         |
| `Task[A]` (= `ZIO[Any, Throwable, A]`) | `RIO () () a` plus `die` for defects |

## Lifting values

```scala
ZIO.succeed(42)
ZIO.fail("oops")
ZIO.attempt { computeInt() }
```

```purescript
pure 42
fail (Proxy :: Proxy "oops") unit
liftEffect computeInt
```

`ZIO.fail` takes a value; RIO's `fail` takes a `Proxy` tag plus
a payload, which adds the tag to the inferred error row. See
`docs/03-errors.md` for the rationale (it lets multiple
failures coexist in the row without a sealed hierarchy).

`ZIO.attempt` captures any thrown exception. RIO splits this
into two: `liftEffect` for a synchronous `Effect`, `liftAff`
for an `Aff`. Neither catches; thrown exceptions land in the
defect channel, recoverable via `sandbox` (mirroring ZIO's
`sandbox` / `catchAllCause` distinction).

## Composing

```scala
for {
  a <- step1
  b <- step2(a)
} yield (a, b)
```

```purescript
do
  a <- step1
  b <- step2 a
  pure (Tuple a b)
```

`flatMap` is `bind` in PureScript; the `do` block reads the
same in both languages.

## Services

ZIO's services are tagged with implicit `Tag[A]`:

```scala
trait Logger {
  def info(s: String): UIO[Unit]
}

object Logger {
  def info(s: String) =
    ZIO.serviceWithZIO[Logger](_.info(s))
}
```

RIO's services are rows of `Aff`-valued operations indexed by
symbol:

```purescript
type Logger =
  { info :: String -> Aff Unit
  }

info :: forall r e. String -> RIO (logger :: Logger | r) e Unit
info msg = do
  l <- ask (Proxy :: Proxy "logger")
  liftAff (l.info msg)
```

The row-typed `ask` gives you the same "I need a `Logger`
somewhere upstream" guarantee ZIO's `ZIO.serviceWith` does,
checked at the type level. See `docs/02-services.md`.

## Providing services

```scala
program.provideLayer(ZLayer.succeed(myLogger))
program.provide(myLogger)
program.provideSomeLayer[Other](ZLayer.succeed(myLogger))
```

```purescript
provide (Proxy :: Proxy "logger") myLogger program
provideAll { logger: myLogger, db: myDb } program
provideLayer (myLogger <+> myDb) program
```

`provide` adds one service; `provideAll` adds an entire record
at once; `provideLayer` runs a `Layer` and provides its output.
See `docs/02-services.md` for the smaller helpers; for the
layer story the in-source comments in `src/RIO/Layer.purs` and
the worked example in `spikes/phase-5-review/` are the
reference.

## Typed errors

```scala
val program: ZIO[R, NotFound | ParseError, A] = ...
program.catchSome {
  case NotFound(id) => fallback(id)
}
program.catchAll(_ => ZIO.unit)
program.mapError(...)
```

```purescript
program :: RIO r (notFound :: Int, parse :: String) A
catchTag (Proxy :: Proxy "notFound") (\id -> fallback id) program
catchAll (\_ -> pure unit) program
mapError handler program
```

`catchTag` removes exactly one tag from the row and returns a
program with that tag gone (or replaced if the handler
introduces new ones). `catchAll` replaces the whole row with
whatever the handler's return type uses. `mapError`
re-translates the row without changing its shape. See
`docs/03-errors.md`.

## Resource safety

```scala
ZIO.acquireReleaseWith(acquire)(release)(use)
ZIO.scoped { ... }
```

```purescript
acquireRelease acquire release use
scoped do
  resource <- acquire
  ... use resource
```

The release runs on every termination path: success,
typed-failure, defect, interruption. This is the same guarantee
ZIO provides, implemented on top of `Effect.Aff.bracket`. See
`docs/05-resources.md` and
`spikes/aff-interruption/FINDINGS.md` for the underlying `Aff`
guarantees.

## Concurrency

ZIO's `fork`, `race`, `raceAll`, and `ZIO.foreachPar` all have
direct RIO counterparts:

| ZIO                          | RIO                                |
| ---------------------------- | ---------------------------------- |
| `program.fork`               | `fork program`                     |
| `program.race(other)`        | `race program other`               |
| `ZIO.raceAll(programs)`      | `raceAll programs`                 |
| `ZIO.foreachPar(xs)(f)`      | `parTraverse f xs`                 |
| `ZIO.foreachParN(n)(xs)(f)`  | `parTraverseN n f xs`              |
| `Fiber.interrupt`            | `interrupt fiber`                  |
| `ZIO.uninterruptible`        | `uninterruptible`                  |

Failure semantics:

- `parTraverse` and `parSequence` short-circuit on the first
  typed failure, cancelling sibling fibers, matching ZIO's
  `foreachPar`. `Par.ado` runs every branch to completion and
  returns the leftmost failure if you need that shape.
- `race` returns the first completion (success or failure) and
  interrupts the loser, exactly as ZIO does.
- `raceAll` ditto for any number of branches.

See `docs/06-concurrency.md` for the cooperative-cancellation
caveat (you may need a `liftAff (delay (Milliseconds 0.0))`
yield in some places, since `Aff` is cooperative rather than
preemptive) and the interaction with `Scope`.

## Layers

ZIO's `ZLayer` and RIO's `Layer` share the same shape: an
effectful recipe for producing services from other services,
with sequential (`>>>`) and horizontal (`++` / `<+>`)
composition and the same resource-safety guarantees.

```scala
val app: ZLayer[Any, DbError, UserService] =
  configLayer >>> dbLayer >>> userServiceLayer
program.provideLayer(app)
```

```purescript
appLayer :: Layer () (dbConnect :: String) (userService :: UserService)
appLayer = configLayer >>> dbLayer >>> userServiceLayer
provideLayer appLayer program
```

Passthrough composition: if layer `B` between `A` and `C`
produces services that `C` needs but doesn't itself consume,
`B` must either re-emit them in its output row or be wrapped
with `RIO.Layer.passthrough`, which extends the layer's output
row with the labels it required as input.

## Testing

```scala
test("greets") {
  for {
    _ <- Greeter.greet("world")
    out <- TestConsole.output
  } yield assertTrue(out == Chunk("hello, world"))
}.provide(Greeter.live, TestConsole.layer)
```

```purescript
itRIO_ "greets" { greeter: mockGreeter, console: testConsole } do
  greet "world"
  out <- liftAff testConsole.output
  liftAff (out `shouldEqual` [ "hello, world" ])
```

`itRIO` and `itRIO_` are `purescript-spec` adapters that run
an `RIO` program as a test body. `RIO.Test.recording` is a
small "record every call into a `Ref`" helper for assertions
on service interactions. `RIO.Test.Clock.newTestClock` is the
direct counterpart of ZIO's `TestClock`: virtual time, an
explicit `advance` controller, deterministic across forks. See
`docs/07-testing.md` for the full surface.

## Things ZIO has that RIO does not (yet)

- **`Fiber.children`, `ZIO.descriptor`, full supervisor model.**
  Out of scope; `docs/06-concurrency.md` calls these out under
  "what RIO does not give you". `forkScoped` covers the
  common "fiber bounded by enclosing scope" case.
- **Full interrupt-with-cause distinguishing interrupter
  identity.** ZIO carries a structured `Cause` through
  interruption that records *who* did the interrupting and
  *why*. rio-fiber's `Cause` keeps an `Interrupt FiberId` leaf
  (so interrupter id survives) but does not yet model
  "interrupted-due-to-failure-elsewhere" as a distinct case;
  rio-aff folds interruption into `Die` because `Aff` does not
  expose a structured kill signal at the user level.
- **Property-test integration tuned for effectful programs.**
  Plain `purescript-quickcheck` works, but RIO has no
  Aff-aware generators or shrinkers yet.

## Things RIO has that ZIO does not

- **The error row is structural.** `RIO r (notFound :: Int,
  parse :: String) a` is the inferred type at the use site;
  you don't have to name a sealed `enum`. Adding a new failure
  tag propagates by inference. ZIO's preferred style is a
  sealed hierarchy of error types; RIO leans on row
  polymorphism instead.
- **`Layer` shrinks the input row by exactly what it
  produces.** Compiles the partial-application pattern of
  composing layers into the type system without needing a
  separate "this layer can be combined with that one" lemma.
