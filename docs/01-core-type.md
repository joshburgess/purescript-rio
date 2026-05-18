# The RIO Type, Explained

RIO is a single monad that tracks three orthogonal concerns at once: a row
of required services (the **R**eader environment), a row of typed failures
(the error channel), and asynchronous **IO** running on top of `Aff`.

```purescript
newtype RIO r e a = RIO (Op r e a)
```

`Op r e a` is an instruction tree run by a step / resume interpreter
in `RIO.Internal`. Semantically a `RIO r e a` is the same as
`Record r -> Aff (Either (Variant e) a)`: given an environment of
services in row `r`, it performs `Aff` work that either fails with a
tagged error in row `e` or produces a value of type `a`. The
instruction-tree form lets synchronous binds stay in a tight JS while
loop and only cross into `Aff` for true async work.

This document walks through what each parameter means in practice and
compares the shape to its closest cousins in ZIO (Scala) and Effect
(TypeScript).

## The three parameters

### `r`: required services

`r` is a row of named services the computation can read from. You ask
for one by name with `ask`:

```purescript
import RIO.Core (RIO)
import Type.Proxy (Proxy(..))

-- The inferred type carries the requirement:
--   readPort :: forall e. RIO (config :: { port :: Int } | _) e Int
readPort = do
  cfg <- ask (Proxy :: Proxy "config")
  pure cfg.port
```

The row is open by default: requirements accumulate automatically when
you sequence computations that need different services, and shrink as
those services are supplied with `provide`. (`ask`, `asks`, `provide`
arrive in Phase 2; the row machinery itself is already in place.)

When the row is empty (`r = ()`), the computation needs nothing from its
environment and can be handed to `runRIO` directly.

### `e`: typed failures

`e` is a row of named, payloaded failure cases. You raise one with `fail`:

```purescript
import RIO.Core (fail)
import Type.Proxy (Proxy(..))

-- The inferred type carries the failure:
--   notFound :: forall r b. RIO r (notFound :: { id :: Int } | _) b
notFound id = fail (Proxy :: Proxy "notFound") { id }
```

Two effects that can fail with different sets of tags compose into one
whose failure row is the union; no shared error supertype is required.
Catching a tag (Phase 3) removes it from the row, so the type tells you
which failures are still possible at any point in the program.

The success type `a` is independent of the failure row, and a `fail`
short-circuits any subsequent binds.

### `a`: the success value

Nothing surprising. `a` is whatever you produce on the happy path.

## How it composes

The newtype wraps `Op r e a`, but the boundary form
`unRIO m env :: Aff (Either (Variant e) a)` is what determines the
observable semantics:

  * `pure a` ignores the environment and produces `Right a`.
  * `bind` runs the first action, short-circuits on `Left`, otherwise
    threads the environment through the continuation. Synchronous
    binds stay in the interpreter's inner loop and never cross into
    `Aff`.
  * `liftEffect` (from `Effect.Class`) and `liftAff` (from
    `Effect.Aff.Class`) ignore the environment and produce `Right`, so
    effects raised through them never become typed failures. Uncaught
    runtime exceptions surface as `Aff` defects, exposed by `sandbox`.

Semantically this is the same algebra as
`ReaderT (Record r) (ExceptT (Variant e) Aff)`; the `Op` instruction
tree is an implementation detail that lets synchronous binds stay in
a tight loop rather than thread through `Aff`'s callback chain.

## Comparison with ZIO and Effect

| Concept                    | RIO (PureScript)                       | ZIO (Scala)               | Effect (TypeScript)         |
|---                         |---                                     |---                        |---                          |
| Type signature             | `RIO r e a`                            | `ZIO[R, E, A]`            | `Effect<A, E, R>`           |
| Required services          | row, `Record r`                        | intersection of types     | intersection of context     |
| Error channel              | row, `Variant e`                       | single `E` (often sealed) | union, `E`                  |
| Composition over services  | row union, inferred                    | type-level intersection   | type-level intersection     |
| Composition over errors    | row union, inferred                    | unification or `Throwable`| union                       |
| Empty services / errors    | `r = ()` / `e = ()`                    | `Any` / `Nothing`         | `never` / `never`           |
| IO base                    | `Aff`                                  | runtime fibers            | runtime fibers              |
| Run with full discharge    | `runRIO' :: RIO () () a -> Aff a`      | `unsafeRun(io)`           | `Effect.runPromise(eff)`    |

A third runner, `unsafeRunRIO :: RIO r e a -> Record r -> Aff (Either (Variant e) a)`,
is the raw inverse of the newtype. It is exported for advanced cases
(custom runners, test harnesses, FFI shims) and bypasses the discharge
checks `runRIO` and `runRIO'` provide; reach for it only when neither of
those fits.

Two practical differences are worth flagging:

  * **PureScript has no intersection types.** RIO uses rows instead. The
    end-user experience is similar (you don't write out long environment
    types yourself), but error messages and inference quirks are
    row-shaped, not intersection-shaped.
  * **`Aff` is cooperative.** PureScript's `Aff` does not preempt
    synchronous bind chains; the
    [Phase 0.5 spike](../spikes/aff-interruption/FINDINGS.md) covers
    exactly what is and isn't guaranteed. ZIO and Effect both have the
    same cooperative property; the difference is what their `yieldNow`
    primitive is called.

## A worked example

This is the smallest interesting program at the level Phase 1 supports.
Nothing here uses `ask` or `catchTag`; those are Phase 2 and Phase 3.

```purescript
module Example where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO)

-- A computation that may fail with a `notFound` tag.
lookupUser
  :: forall r
   . Int
  -> RIO r (notFound :: { id :: Int }) String
lookupUser id =
  if id == 1 then pure "alice"
  else fail (Proxy :: Proxy "notFound") { id }

main :: Effect Unit
main = launchAff_ do
  result <- runRIO do
    name <- lookupUser 1
    liftEffect (Console.log ("hello, " <> name))
    pure name
  liftEffect case result of
    Right name -> Console.log ("ok: " <> name)
    Left _ -> Console.log "not found"
```

The inferred type of the do-block is
`RIO () (notFound :: { id :: Int }) String`; PureScript figures the row
out from the body. The `Left` branch carries a `Variant` you can pattern
match with `Variant.on` (Phase 3 adds `catchTag` to do this in a
type-narrowing way inside the monad).

## What's next

  * Phase 2 adds `ask` / `asks` / `provide` and the service pattern.
  * Phase 3 adds `catchTag` / `catchAll` / `mapError` and the
    typed/defect split.
  * Phase 4 adds resource safety (`acquireRelease`, `scoped`).
  * Phase 6 adds concurrency primitives (`fork`, `join`, `race`).

## Pointers

- Source: [`src/RIO/Core.purs`](../src/RIO/Core.purs) (re-export
  surface) and [`src/RIO/Internal.purs`](../src/RIO/Internal.purs)
  (the `RIO r e a` newtype and its
  `Functor` / `Apply` / `Bind` / `Monad` / `MonadEffect` /
  `MonadAff` instances).
- Spec coverage:
  [`test/Test/RIO/CoreSpec.purs`](../test/Test/RIO/CoreSpec.purs)
  for the monad laws, and
  [`test/Test/RIO/EffectAndFailSpec.purs`](../test/Test/RIO/EffectAndFailSpec.purs)
  for `liftEffect` / `liftAff` / `fail`.
- Services and `ask` / `provide`:
  [`docs/02-services.md`](./02-services.md).
- Typed failures and `catchTag` / `catchAll`:
  [`docs/03-errors.md`](./03-errors.md).
