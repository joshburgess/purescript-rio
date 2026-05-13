# rio

A ZIO / Effect-TS style effect type for PureScript: typed
environment, typed errors, resource safety, layers, and
structural concurrency, all sitting on top of `Aff`.

```purescript
newtype RIO r e a = RIO (Record r -> Aff (Either (Variant e) a))
```

- `r` is a **row of services** the computation needs.
- `e` is a **row of typed failures** it may raise.
- `a` is the **success value** on the happy path.

Both rows are open by default, so requirements aggregate
automatically on composition and shrink as services are
provided and failures are handled.

## Why

If you've used ZIO in Scala or Effect-TS in TypeScript, you
already know the shape. PureScript's row types let the same
three-channel design feel native: the requirements channel is
a record row, the error channel is a `Variant` row, and the
type inferer aggregates and shrinks both as your program
composes.

If you have not used those libraries, the short version: `RIO`
is a description of an asynchronous computation that knows what
it needs, what it can fail with, and what it produces, and
gives you uniform plumbing to wire those together. You write
handlers, services, and tests against typed interfaces; you
provide implementations once at the edge.

## A 30-second tour

```purescript
import RIO.Core
import Type.Proxy (Proxy(..))

type Logger = { log :: String -> Aff Unit }

greet
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
greet name = do
  logger <- ask (Proxy :: Proxy "logger")
  liftAff (logger.log ("hello, " <> name))

main :: Effect Unit
main = launchAff_ do
  let consoleLogger = { log: \s -> liftEffect (Console.log s) }
  runRIO' (provideAll { logger: consoleLogger } (greet "world"))
```

A handler that may fail picks up its tag automatically:

```purescript
findTodo :: Int -> RIO (store :: TodoStore) (notFound :: { id :: Int }) Todo
findTodo tid = do
  store <- ask (Proxy :: Proxy "store")
  row <- liftAff (store.get tid)
  case row of
    Nothing -> fail (Proxy :: Proxy "notFound") { id: tid }
    Just todo -> pure todo
```

The `notFound` tag flows up through callers until a
`catchTag (Proxy :: _ "notFound")` removes it from the row,
or `runRIO` surfaces it as a `Left` for the entry point to
render.

## Install

`rio` is published to the PureScript registry. Add it to your
`spago.yaml` as a dependency:

```yaml
dependencies:
  - rio
```

Then `npx spago install`.

## What's in v0.1.0

- `RIO.Core`: the type, runners, and the most common
  re-exports.
- `RIO.Env`: `ask` / `asks` to read services, `provide` /
  `provideAll` to inject them.
- `RIO.Error`: `fail`, `catchTag`, `catchAll`, `mapError`,
  plus `die` / `sandbox` / `unsandbox` for the defect channel.
- `RIO.Resource`: `acquireRelease`, `ensuring`, and `Scope` with
  LIFO finalizers; release runs on success, typed failure,
  defect, and interruption.
- `RIO.Layer`: `Layer rIn e rOut`, with sequential (`>>>`)
  and horizontal (`<+>`) composition, `buildLayer`,
  `provideLayer`, and `passthrough` for keeping a layer's input
  services visible downstream.
- `RIO.Concurrency`: `fork`, `forkScoped`, `join`, `interrupt`,
  `uninterruptible`, `timeout`, `parTraverse` (short-circuit on
  first failure), `parTraverseN` (bounded), `parSequence`,
  `zipPar`, `race`, `raceAll`. Cancellation is cooperative
  (`Aff`-style); resources held by losers in a race or a
  short-circuited traversal are released.
- `RIO.Deferred`: one-shot write-once cell over
  `Effect.Aff.AVar` for fiber handshakes (`makeDeferred`,
  `succeedDeferred`, `failDeferred`, `awaitDeferred`,
  `pollDeferred`).
- `RIO.Clock`: the `Clock` service plus `liveClock`.
- `RIO.Schedule`: pure scheduling policies (`recurs`, `spaced`,
  `exponential`, `jittered`, `intersect`, `andThen`, `whileInput`)
  with runners `repeat`, `retry`, `retryOrElse` that sleep through
  `Clock` so the virtual-time test clock can drive them.
- `RIO.STM`: software-transactional memory. `TRef`, `STM e a`,
  `newTRef` / `readTRef` / `writeTRef` / `modifyTRef`, `retry` /
  `check`, `orElse`, `failSTM`, and `atomically`. Single-event-loop
  atomicity: no version checks, no spinning.
- `RIO.Spec`: `itRIO` / `itRIO_` adapters for
  `purescript-spec`.
- `RIO.Test`: `mockService`, `recording` for service-call
  assertions.
- `RIO.Test.Clock`: `newTestClock` for deterministic
  virtual-time testing.

## Documentation

Walkthrough docs:

- [`docs/01-core-type.md`](./docs/01-core-type.md): the three
  parameters, how the channels compose.
- [`docs/02-services.md`](./docs/02-services.md): service
  convention and idiomatic provision.
- [`docs/03-errors.md`](./docs/03-errors.md): typed failures,
  catching, and the defect channel.
- [`docs/06-concurrency.md`](./docs/06-concurrency.md): fork,
  race, parallel traversal, cancellation caveats.
- [`docs/07-testing.md`](./docs/07-testing.md): the spec
  adapters and the virtual-time clock.
- [`docs/08-scheduling.md`](./docs/08-scheduling.md): retry and
  repeat policies, combinators, and how to drive them
  deterministically in tests.
- [`docs/09-stm.md`](./docs/09-stm.md): the STM type, primitives,
  atomicity semantics on the JS event loop, and `retry` / `orElse`.
- [`docs/performance.md`](./docs/performance.md): benchmark
  baselines and dominant costs.

Migration guides for users coming from other ecosystems:

- [`docs/migrating-from-zio.md`](./docs/migrating-from-zio.md)
- [`docs/migrating-from-effect-ts.md`](./docs/migrating-from-effect-ts.md)

Worked example:

- [`examples/todo-api/`](./examples/todo-api/): a small
  HTTPurple service demonstrating layers, typed failures,
  in-memory persistence, and JSON codec bridging.

## Build

```sh
npm install
npx spago build
npx spago test
```

Run the example:

```sh
npx spago run -p rio-example-todo-api
```

Run the benchmark suite:

```sh
npx spago run -p rio-benchmarks
```

## Status

v0.1.0 covers the production core: services, typed errors,
resource safety, layers, concurrency, and a testing toolkit.
The v0.2 backlog tracks tracing / metrics and a perf regression
gate. Bounded-concurrency traversal (`parTraverseN`), `timeout`,
`uninterruptible`, `forkScoped`, `Deferred`, `ensuring`,
`Layer.passthrough`, short-circuiting parallel traversal,
`RIO.Schedule` (retry / repeat policies), and `RIO.STM`
(`TRef` + `atomically`) have already landed on `main`. See
[`PROJECT_BUILD_PLAN.md`](./PROJECT_BUILD_PLAN.md).

## License

MIT. See [`LICENSE`](./LICENSE).
