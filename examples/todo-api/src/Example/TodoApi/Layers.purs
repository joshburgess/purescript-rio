-- | Layer wiring for the todo-api example.
-- |
-- | The app layer composes:
-- |
-- |   * `loggerLayer` (console logger)
-- |   * `postgresLayer` (a `node-postgres` connection pool; the
-- |     pool's `end` is registered as a finalizer on the layer's
-- |     scope so it drains when the surrounding program exits).
-- |
-- | The connection string is passed in by `Main.purs` after reading
-- | `DATABASE_URL`. A test rig could swap any of these via
-- | `provide` / `provideLayer` without touching the handlers.
module Example.TodoApi.Layers
  ( appLayer
  , migrate
  ) where

import Prelude hiding ((>>>))

import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (Layer, RIO, fromRIO)
import RIO.Aff.Layer ((<+>))
import RIO.Aff.Logger (consoleLogger)
import RIO.Aff.Postgres (PgError, Postgres)
import RIO.Aff.Postgres.Layer (postgresLayer)
import RIO.Aff.Postgres.Migrate (migrate, sqlMigration) as Migrate

import Example.TodoApi.Services (Logger)

loggerLayer :: forall e. Layer () e (logger :: Logger)
loggerLayer = fromRIO (liftEffect consoleLogger <#> \l -> { logger: l })

-- | The full app layer. Builds the console logger and the Postgres
-- | pool from `connectionString`.
appLayer
  :: String
  -> Layer () () (logger :: Logger, postgres :: Postgres)
appLayer connectionString =
  loggerLayer <+> postgresLayer { connectionString }

-- | Versioned schema bootstrap. Runs through
-- | `RIO.Aff.Postgres.Migrate`, which holds an advisory lock and
-- | records applied versions in `__rio_migrations`. Adding a new
-- | column or index later is a new entry here, not a re-run of the
-- | original DDL.
migrate
  :: forall r
   . RIO (postgres :: Postgres | r) (db :: PgError) Unit
migrate = Migrate.migrate (Proxy :: Proxy "db") $ Map.fromFoldable
  [ 1 /\ Migrate.sqlMigration (Proxy :: Proxy "db")
      "create table if not exists rio_todos \
      \( id serial primary key \
      \, title text not null \
      \, done boolean not null default false \
      \, created_at_ms double precision not null \
      \)"
  ]
