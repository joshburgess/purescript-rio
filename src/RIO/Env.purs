-- | Environment-row primitives for `RIO`.
-- |
-- | Phase 2.1 ships the reading side (`ask`, `asks`). Phase 2.2 adds
-- | `provide` for supplying a single service.
module RIO.Env
  ( ask
  , asks
  , provide
  , provideAll
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.Row (class Cons) as Row
import Record as Record
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy)

import RIO.Internal (RIO(..), mkEffectRIO, mkRIO, unsafeUnRIO)

-- | Read a single service out of the environment by name.
-- |
-- | The `Cons sym a r' r` constraint expresses "row `r` has a field `sym`
-- | of type `a`, with `r'` covering the other fields". In practice, the
-- | compiler picks up the requirement from how the result is used; you
-- | rarely need to write the row out yourself.
-- |
-- | ```purescript
-- | -- Inferred:
-- | --   forall e r'. RIO (logger :: Logger | r') e Logger
-- | getLogger = ask (Proxy :: Proxy "logger")
-- | ```
ask
  :: forall sym a r' r e
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> RIO r e a
ask sym = mkEffectRIO \r -> pure (Record.get sym r)

-- | Read a single service and project a value out of it in one step.
-- | Equivalent to `map f (ask sym)`, but the named version is easier on
-- | the eye in service-heavy code.
-- |
-- | ```purescript
-- | -- Inferred:
-- | --   forall e r'. RIO (config :: { port :: Int } | r') e Int
-- | getPort = asks (Proxy :: Proxy "config") _.port
-- | ```
asks
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> (a -> b)
  -> RIO r e b
asks sym f = map f (ask sym)

-- | Supply a single service to an inner `RIO`, shrinking the required
-- | environment row by exactly one field.
-- |
-- | The `Cons sym a r' r` constraint says "the inner computation needs
-- | row `r`, which is `r'` plus the field `sym :: a`". Calling `provide`
-- | hands the missing `a` to the inner computation and returns a new
-- | `RIO r'` that no longer needs `sym`.
-- |
-- | ```purescript
-- | -- inner :: RIO (logger :: Logger, db :: Database) e a
-- | -- result :: RIO (db :: Database) e a
-- | result = provide (Proxy :: Proxy "logger") myLogger inner
-- | ```
-- |
-- | The original API draft also carried a `Lacks sym r'` constraint;
-- | the Phase 0.4 spike's findings (LE-1) recommended dropping it for
-- | better inference and shorter error messages. The internal insertion
-- | is performed by `unsafeSet`, which is safe under the `Cons` relation
-- | because `r'` is the row `r` minus `sym` by construction.
provide
  :: forall sym a r' r e b
   . IsSymbol sym
  => Row.Cons sym a r' r
  => Proxy sym
  -> a
  -> RIO r e b
  -> RIO r' e b
provide sym v inner = mkRIO \r' ->
  unsafeUnRIO inner (unsafeSet (reflectSymbol sym) v r')

-- | Supply the entire environment to an inner `RIO`, leaving an empty
-- | required row. After `provideAll`, the resulting `RIO () e a` can be
-- | handed directly to `runRIO` or `runRIO'`.
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
provideAll env inner = mkRIO \_ -> unsafeUnRIO inner env
