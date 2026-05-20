-- | Layer wiring for the `Postgres` service.
-- |
-- | `postgresLayer` allocates a `node-postgres` connection pool
-- | from a config record and registers the pool's shutdown
-- | (`Pool.end`) as a finalizer on the surrounding scope. Use it
-- | as the outermost layer in an application's `Layer`
-- | composition; the pool is drained and the underlying
-- | connections closed when the scope exits (success, typed
-- | failure, defect, or external kill).
-- |
-- | The pool config row is whatever subset of
-- | `Effect.Postgres.Pool.Config` the caller wants to pin. A
-- | typical production wiring uses `connectionString`; tests
-- | typically use `host`, `port`, `database`, `user`, `password`,
-- | and `max`. See `node-postgres` docs for the full set.
module RIO.Aff.Postgres.Layer
  ( postgresLayer
  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Postgres.Pool (end) as PG.Pool
import Effect.Class (liftEffect)
import Effect.Postgres.Pool (Config, make) as PG.Pool
import Prim.Row (class Union)

import RIO.Aff.Core (RIO)
import RIO.Aff.Env (ask)
import RIO.Aff.Layer (Layer, fromRIO)
import RIO.Aff.Resource (Scope, addFinalizer)
import Type.Proxy (Proxy(..))

import RIO.Aff.Postgres (Postgres(..))

-- | Build a `Postgres` service from a pool config. The pool is
-- | created with `Effect.Postgres.Pool.make`; its shutdown is
-- | registered as the most-recent finalizer on the surrounding
-- | scope, so a fresh pool is created on each `provideLayer` and
-- | drained when the layer's scope exits.
-- |
-- | The error row is left polymorphic: `Pool.make` is a plain
-- | `Effect` and does not fail typed, and finalizer errors during
-- | shutdown are swallowed (they can't be surfaced on the typed
-- | row from inside a finalizer).
-- |
-- | ```purescript
-- | -- in main:
-- | let layer = postgresLayer { connectionString: "postgres://..." }
-- | runRIO' (provideLayer layer program)
-- | ```
postgresLayer
  :: forall config missing trash rIn
   . Union config missing (PG.Pool.Config trash)
  => Record config
  -> Layer rIn () (postgres :: Postgres)
postgresLayer cfg = fromRIO acquire
  where
  acquire
    :: RIO (scope :: Scope | rIn) () { postgres :: Postgres }
  acquire = do
    pool <- liftEffect (PG.Pool.make @config @missing @trash cfg)
    scope <- ask (Proxy :: Proxy "scope")
    addFinalizer scope (liftAff (void (runExceptT (PG.Pool.end pool))))
    pure { postgres: Postgres { pool } }
