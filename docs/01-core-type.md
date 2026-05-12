# The RIO Type, Explained

RIO is a single monad that tracks three orthogonal concerns at once: a row
of required services (the **R**eader environment), a row of typed failures
(the error channel), and asynchronous **IO** running on top of `Aff`.

```purescript
newtype RIO r e a = RIO (Record r -> Aff (Either (Variant e) a))
```

This document walks through what each parameter means in practice and
compares the shape to its closest cousins in ZIO (Scala) and Effect-TS
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

The newtype unwraps to `Record r -> Aff (Either (Variant e) a)`. That
shape determines all the typeclass instances:

  * `pure a` ignores the environment and produces `Right a`.
  * `bind` runs the first action, short-circuits on `Left`, otherwise
    threads the environment through the continuation.
  * `liftEffect` and `liftAff` ignore the environment and produce `Right`,
    so effects raised through them never become typed failures. Uncaught
    runtime exceptions surface as `Aff` defects, exposed by `sandbox`
    (Phase 3.3).

There is nothing exotic here: it is `ReaderT (Record r) (ExceptT (Variant e) Aff)`
written as a newtype for better inference and a smaller error-message
footprint. The row-typed environment and error channel are what make the
shape carry more information than the bare transformer stack would.

## Comparison with ZIO and Effect-TS

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
