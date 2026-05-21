-- | Per-symbol environment-row primitives for `RIO`.
-- |
-- | `RIO.Fiber.Core` already exposes `ask :: RIO r e (Record r)` and
-- | `asks :: (Record r -> a) -> RIO r e a`, which return the whole
-- | environment record. This module adds the *symbol-indexed*
-- | variants used by service-style code (and by `RIO.Fiber.Tag`):
-- | given a `Proxy sym`, read out a single field, project from it,
-- | or supply one field at a time when running an inner program.
-- |
-- | The names are suffixed `At` (`askAt`, `asksAt`, `provideAt`) so
-- | they coexist with the whole-record `ask` / `asks` from
-- | `RIO.Fiber.Core`. `provideAll` matches the aff naming because
-- | it takes the whole record and shrinks the required row to `()`,
-- | just like the corresponding `RIO.Aff.Env.provideAll`.
-- |
-- | These are the building blocks for `RIO.Fiber.Tag` (typed,
-- | reusable service handles) and for service modules such as
-- | `RIO.Fiber.System` that want to phrase "read the `system`
-- | service" without writing `asks _.system` at every call site.
module RIO.Fiber.Env
  ( askAt
  , asksAt
  , provideAt
  , provideAll
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons) as Row
import Record as Record
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy)

import RIO.Fiber.Internal (RIO(..))
import RIO.Fiber.Internal as Internal

-- | Read a single service out of the environment by name.
-- |
-- | The `Cons sym a r' r` constraint expresses "row `r` has a field
-- | `sym` of type `a`, with `r'` covering the other fields". In
-- | practice, the compiler picks up the requirement from how the
-- | result is used; you rarely need to write the row out yourself.
-- |
-- | ```purescript
-- | -- Inferred:
-- | --   forall e r'. RIO (logger :: Logger | r') e Logger
-- | getLogger = askAt (Proxy :: Proxy "logger")
-- | ```
askAt
  :: forall sym a r' r e
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> RIO r e a
askAt sym = map (Record.get sym) (RIO Internal.opAsk)

-- | Read a single service and project a value out of it in one step.
-- | Equivalent to `map f (askAt sym)`, but the named version is
-- | easier on the eye in service-heavy code.
-- |
-- | ```purescript
-- | -- Inferred:
-- | --   forall e r'. RIO (config :: { port :: Int } | r') e Int
-- | getPort = asksAt (Proxy :: Proxy "config") _.port
-- | ```
asksAt
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> (a -> b)
  -> RIO r e b
asksAt sym f = map f (askAt sym)

-- | Supply a single service to an inner `RIO`, shrinking the
-- | required environment row by exactly one field.
-- |
-- | The `Cons sym a r' r` constraint says "the inner computation
-- | needs row `r`, which is `r'` plus the field `sym :: a`". Calling
-- | `provideAt` hands the missing `a` to the inner computation and
-- | returns a new `RIO r'` that no longer needs `sym`.
-- |
-- | ```purescript
-- | -- inner :: RIO (logger :: Logger, db :: Database) e a
-- | -- result :: RIO (db :: Database) e a
-- | result = provideAt (Proxy :: Proxy "logger") myLogger inner
-- | ```
-- |
-- | Following the aff convention, no `Lacks sym r'` constraint is
-- | imposed: the internal insertion is performed by `unsafeSet`,
-- | which is safe under `Cons sym a r' r` because `r'` is `r` minus
-- | `sym` by construction.
provideAt
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> a
  -> RIO r e b
  -> RIO r' e b
provideAt sym v (RIO inner) =
  RIO (Internal.opLocal (\r' -> unsafeSet (reflectSymbol sym) v r') inner)

-- | Supply the entire environment to an inner `RIO`, leaving an
-- | empty required row. After `provideAll`, the resulting
-- | `RIO () e a` can be handed directly to `runRIO` / `runRIO'`.
-- |
-- | ```purescript
-- | -- inner :: RIO (logger :: Logger, db :: Database) e a
-- | runnable :: RIO () e a
-- | runnable =
-- |   provideAll
-- |     { logger: myLogger, db: myDatabase }
-- |     inner
-- | ```
provideAll :: forall r e a. Record r -> RIO r e a -> RIO () e a
provideAll env (RIO inner) =
  RIO (Internal.opLocal (\_ -> env) inner)
