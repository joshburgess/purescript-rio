# Migrating from Effect

This guide maps Effect idioms to RIO. Effect is a TypeScript
port of ZIO, so most of the model is already familiar: a
three-parameter effect type with a typed requirements channel, a
typed error channel, and a success channel. RIO is the same
shape in PureScript, with the requirements and errors expressed
as rows rather than intersection / union types.

If you're new to RIO, the docs in `docs/01-core-type.md`
through `docs/07-testing.md` walk the model from first
principles. This page is for Effect users who want a quick
reference. The sibling `docs/migrating-from-zio.md` covers the
same ground from the Scala / ZIO angle.

## The core type

Effect uses three type parameters: success, error,
requirements.

```ts
import { Effect } from "effect"

type Eff<R, E, A> = Effect.Effect<A, E, R>
```

RIO uses the same three slots, but the requirements and errors
are PureScript rows rather than intersection / union types.

```purescript
newtype RIO r e a = ...
-- r is a row of services, e.g. (logger :: Logger, db :: Database)
-- e is a row of typed errors, e.g. (notFound :: Int, parse :: String)
-- a is the success type
```

In Effect, `R` is read as "the intersection of services I
require"; in RIO `r` is read as "the record of services I
require, indexed by name". The empty environment in Effect
is `never`; in RIO it is the empty row `()`. The empty error in
Effect is `never`; in RIO it is the empty row `()`.

| Effect                              | RIO                                   |
| -------------------------------------- | ------------------------------------- |
| `Effect.Effect<A, never, never>`       | `RIO () () a`                         |
| `Effect.Effect<A, E, never>`           | `RIO () e a`                          |
| `Effect.Effect<A, never, R>`           | `RIO r () a`                          |
| `Effect.Effect<A, E, R>`               | `RIO r e a`                           |

## Lifting values

```ts
Effect.succeed(42)
Effect.fail("oops")
Effect.try(() => computeInt())
Effect.tryPromise(() => fetchUser())
```

```purescript
pure 42
fail (Proxy :: Proxy "oops") unit
liftEffect computeInt
liftAff fetchUser
```

`Effect.fail` takes a value; RIO's `fail` takes a `Proxy` tag
plus a payload, which adds the tag to the inferred error row.
See `docs/03-errors.md` for the rationale (it lets multiple
failures coexist in the row without a sealed union type).

`Effect.try` captures any thrown exception. RIO splits this
into two: `liftEffect` for a synchronous `Effect`, `liftAff`
for an `Aff`. Neither catches; thrown exceptions land in the
defect channel, recoverable via `sandbox` (mirroring
Effect's `Effect.sandbox` / `Effect.catchAllCause`).

## Composing

```ts
const program = Effect.gen(function* () {
  const a = yield* step1
  const b = yield* step2(a)
  return [a, b] as const
})
```

```purescript
program = do
  a <- step1
  b <- step2 a
  pure (Tuple a b)
```

`Effect.gen` is the closest analogue to `do`-notation. The
binding behavior is the same: each `yield*` extracts a value
from an effect, just like `<-` in PureScript.

## Services

Effect tags services with `Context.Tag`:

```ts
class Logger extends Context.Tag("Logger")<Logger, {
  readonly info: (s: string) => Effect.Effect<void>
}>() {}

const log = (s: string) =>
  Logger.pipe(Effect.flatMap((l) => l.info(s)))
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

Code samples in this guide use rio-aff spellings; rio-fiber
readers substitute `askAt` / `asksAt` / `provideAt` for the
symbol-indexed forms (`ask` / `asks` / `provide` taking a
`Proxy`), and `fromAff` (from `RIO.Fiber.Aff`) for `liftAff`
since rio-fiber does not implement `MonadAff` directly. A few
more rio-fiber substitutions worth knowing up front:
`scoped do ...` in code samples below is rio-aff's env-row
form; rio-fiber's `scoped` takes the `Scope` as a lambda
argument (`scoped \scope -> ...`). The layer combinators
`>>>` (`andThen`) and `<+>` (`combine`) used in the Layers
section are rio-aff infix aliases; rio-fiber spells them
`chainLayer` and `mergeLayers` with no infix forms. Both
designs check at the type level that the
service is present somewhere upstream. Effect uses a
`Context.Tag` and intersection in `R`; RIO uses a `Proxy`
symbol and a row label. See `docs/02-services.md`.

## Providing services

```ts
program.pipe(Effect.provideService(Logger, myLogger))
program.pipe(Effect.provide(Layer.succeed(Logger, myLogger)))
program.pipe(Effect.provide(MainLayer))
```

```purescript
provide (Proxy :: Proxy "logger") myLogger program
provideAll { logger: myLogger, db: myDb } program
provideLayer (myLogger <+> myDb) program
```

`provide` adds one service; `provideAll` adds an entire record
at once; `provideLayer` runs a `Layer` and provides its output.
See `docs/02-services.md` for the smaller helpers; for the
layer story the in-source comments in
`rio-aff/src/RIO/Aff/Layer.purs` (and the fiber-side mirror
`rio-fiber/src/RIO/Fiber/Layer.purs`) and the worked example in
`spikes/phase-5-review/` are the reference.

## Typed errors

```ts
const program: Effect.Effect<A, NotFound | ParseError, R> = ...

program.pipe(
  Effect.catchTag("NotFound", ({ id }) => fallback(id))
)
program.pipe(Effect.catchAll(() => Effect.void))
program.pipe(Effect.mapError(handler))
```

```purescript
program :: RIO r (notFound :: Int, parse :: String) A
catchTag (Proxy :: Proxy "notFound") (\id -> fallback id) program
catchAll (\_ -> pure unit) program
mapError handler program
```

Effect's `catchTag` and RIO's `catchTag` line up almost
exactly: both pick out one tag from the union / row, run a
handler, and the result type has that tag removed. RIO's row
phrasing makes the "tag removed" part visible in the type
(`(notFound :: Int, parse :: String)` shrinks to
`(parse :: String)`); Effect's union does the same job via
exhaustiveness in the type checker. See `docs/03-errors.md`.

## Resource safety

```ts
Effect.acquireUseRelease(acquire, use, release)
Effect.scoped(program)
```

```purescript
acquireRelease acquire release use
scoped do
  resource <- acquire
  ... use resource
```

The release runs on every termination path: success,
typed-failure, defect, interruption. This is the same guarantee
Effect provides, implemented on top of `Effect.Aff.bracket`.
See `docs/05-resources.md` and
`spikes/aff-interruption/FINDINGS.md` for the underlying `Aff`
guarantees.

## Concurrency

Effect's `Effect.fork`, `Effect.race`, `Effect.raceAll`, and
`Effect.forEach({ concurrency: "unbounded" })` all have direct
RIO counterparts:

| Effect                                                | RIO                                       |
| -------------------------------------------------------- | ----------------------------------------- |
| `Effect.fork(program)`                                   | `fork program`                            |
| `Effect.race(a, b)`                                      | `race a b`                                |
| `Effect.raceAll([a, b, c])`                              | `raceAll [a, b, c]`                       |
| `Effect.forEach(xs, f, { concurrency: "unbounded" })`    | `parTraverse f xs`                        |
| `Effect.forEach(xs, f, { concurrency: n })`              | `parTraverseN n f xs`                     |
| `Fiber.interrupt(fiber)`                                 | `interrupt fiber`                         |
| `Effect.uninterruptible(program)`                        | `uninterruptible`                         |

Failure semantics:

- `parTraverse` and `parSequence` short-circuit on the first
  typed failure, cancelling sibling fibers, matching Effect's
  `forEach` with unbounded concurrency. `Par.ado` runs every
  branch to completion and returns the leftmost failure if you
  need that shape.
- `race` returns the first completion (success or failure) and
  interrupts the loser, exactly as Effect does.
- `raceAll` ditto for any number of branches.

See `docs/06-concurrency.md` for the cooperative-cancellation
caveat (you may need a `liftAff (delay (Milliseconds 0.0))`
yield in some places, since `Aff` is cooperative rather than
preemptive) and the interaction with `Scope`.

## Layers

Effect's `Layer` and RIO's `Layer` share the same shape: an
effectful recipe for producing services from other services,
with sequential (`Layer.provide`) and horizontal
(`Layer.merge`) composition and the same resource-safety
guarantees.

```ts
const AppLayer = ConfigLayer.pipe(
  Layer.provide(DbLayer),
  Layer.provide(UserServiceLayer),
)

program.pipe(Effect.provide(AppLayer))
```

```purescript
appLayer :: Layer () (dbConnect :: String) (userService :: UserService)
appLayer = configLayer >>> dbLayer >>> userServiceLayer
provideLayer appLayer program
```

Passthrough composition: if layer `B` between `A` and `C`
produces services that `C` needs but doesn't itself consume,
`B` must either re-emit them in its output row or be wrapped
with `RIO.Aff.Layer.passthrough` (rio-fiber:
`RIO.Fiber.Layer.passthrough`), the direct analogue of
Effect's `Layer.passthrough`.

## Testing

```ts
import { describe, it, expect } from "@effect/vitest"

it.effect("greets", () =>
  Effect.gen(function* () {
    yield* Greeter.greet("world")
    const out = yield* TestConsole.output
    expect(out).toEqual(["hello, world"])
  }).pipe(Effect.provide(TestLayer))
)
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
direct counterpart of Effect's `TestClock`: virtual time, an
explicit `advance` controller, deterministic across forks. See
`docs/07-testing.md` for the full surface.

## Things Effect has that RIO does not (yet)

- **`Fiber.children`, `Effect.descriptor`, full supervisor model.**
  Out of scope; `docs/06-concurrency.md` calls these out under
  "what RIO does not give you". `forkScoped` covers the
  common "fiber bounded by enclosing scope" case.
- **Full interrupt-with-cause distinguishing interrupter
  identity.** Effect carries a structured `Cause` through
  interruption. rio-fiber's `Cause` keeps an `Interrupt FiberId`
  leaf (interrupter id survives) but does not yet model
  "interrupted-due-to-failure-elsewhere" as a distinct case;
  rio-aff folds interruption into `Die` because `Aff` does not
  expose a structured kill signal at the user level.

## Things RIO has that Effect does not

- **The error row is structural.** `RIO r (notFound :: Int,
  parse :: String) a` is the inferred type at the use site;
  you don't have to name a tagged class. Adding a new failure
  tag propagates by inference. Effect's preferred style is
  `Data.TaggedError` classes joined by union; RIO leans on row
  polymorphism instead.
- **`Layer` shrinks the input row by exactly what it
  produces.** Compiles the partial-application pattern of
  composing layers into the type system without needing a
  separate combinator for "this layer can be combined with
  that one".
- **rio-aff: no JS runtime overhead from a fiber scheduler.**
  The rio-aff package runs on `Aff`, which compiles to plain
  Promise-like callbacks in the standard PureScript output. The
  cost model is closer to hand-written async / await than to a
  managed fiber pool. The premier rio-fiber package does ship a
  custom fiber interpreter, but it is intentionally lighter than
  ZIO's or Effect's: a single-threaded scheduler tuned for the
  JS event loop, not a work-stealing pool.
