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

import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))

import RIO.Core (Layer, RIO, fromRIO)
import RIO.Layer ((<+>))
import RIO.Logger (consoleLogger)
import RIO.Postgres (PgError, Postgres, exec)
import RIO.Postgres.Layer (postgresLayer)

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

-- | Idempotent schema bootstrap. Runs once on startup so a fresh
-- | database is usable immediately; existing tables are left alone.
migrate
  :: forall r
   . RIO (postgres :: Postgres | r) (db :: PgError) Unit
migrate = do
  _ <- exec (Proxy :: Proxy "db")
    ( "create table if not exists rio_todos \
      \( id serial primary key \
      \, title text not null \
      \, done boolean not null default false \
      \, created_at_ms double precision not null \
      \)" :: String
    )
  pure unit
