-- | Layer wiring for the `Postgres` service.
-- |
-- | `postgresLayer` allocates a `node-postgres` connection pool
-- | from a config record and registers the pool's shutdown
-- | (`Pool.end`) as a finalizer on the surrounding scope. Pass
-- | it to `RIO.Fiber.Layer.provideScoped` to wrap a program; the
-- | pool is drained and the underlying connections closed when
-- | the scope exits (success, typed failure, defect, or external
-- | kill).
-- |
-- | The pool config row is whatever subset of
-- | `Effect.Postgres.Pool.Config` the caller wants to pin. A
-- | typical production wiring uses `connectionString`; tests
-- | typically use `host`, `port`, `database`, `user`, `password`,
-- | and `max`. See `node-postgres` docs for the full set.
module RIO.Fiber.Postgres.Layer
  ( postgresLayer
  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Effect.Aff (launchAff_)
import Effect.Postgres.Pool (Config, make) as PG.Pool
import Effect.Aff.Postgres.Pool (end) as PG.Pool.Aff
import Prim.Row (class Union)

import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Scope (Scope, addFinalizer)

import RIO.Fiber.Postgres (Postgres(..))

-- | Build a `Postgres` service from a pool config. The pool is
-- | created with `Effect.Postgres.Pool.make`; its shutdown is
-- | registered as a finalizer on the surrounding scope, so a
-- | fresh pool is created on each call to
-- | `provideScoped (postgresLayer cfg)` and drained when the
-- | inner program exits.
-- |
-- | The error row is left polymorphic: `Pool.make` is a plain
-- | `Effect` and does not fail typed, and finalizer errors during
-- | shutdown are swallowed (they can't be surfaced on the typed
-- | row from inside a finalizer).
-- |
-- | ```purescript
-- | -- in main:
-- | runRIO'
-- |   ( provideScoped
-- |       (postgresLayer { connectionString: "postgres://..." })
-- |       program
-- |   )
-- | ```
postgresLayer
  :: forall config missing trash rIn e
   . Union config missing (PG.Pool.Config trash)
  => Record config
  -> Scope
  -> RIO rIn e { postgres :: Postgres }
postgresLayer cfg scope = do
  pool <- liftEffect (PG.Pool.make @config @missing @trash cfg)
  addFinalizer scope
    (launchAff_ (void (runExceptT (PG.Pool.Aff.end pool))))
  pure { postgres: Postgres { pool } }
