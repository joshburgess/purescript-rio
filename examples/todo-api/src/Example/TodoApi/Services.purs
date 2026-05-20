-- | Service interfaces for the todo-api example.
-- |
-- | The persistence story is `rio-postgres`: handlers call
-- | `query` / `execParams` directly against the `Postgres` service
-- | in the environment, so this module no longer carries a
-- | `TodoStore` indirection.
-- |
-- | Re-exports the surrounding library services the example reads
-- | against (`Logger`, `Clock`, the request-id `Local`, and the
-- | `Postgres` token + `PgError` type) so the rest of the example
-- | imports one namespace.
module Example.TodoApi.Services
  ( Todo
  , module RIO.Aff.Clock
  , module RIO.Aff.Local
  , module RIO.Aff.Logger
  , module RIO.Aff.Postgres
  ) where

import Effect.Aff (Milliseconds)

import RIO.Aff.Clock (Clock)
import RIO.Aff.Local (Local)
import RIO.Aff.Logger (Logger)
import RIO.Aff.Postgres (PgError, Postgres)

-- | A todo as stored. `createdAt` is a wall-clock timestamp set by
-- | the handler at insert time so the response can echo it back.
-- | Persisted as `double precision` in the `rio_todos` table.
type Todo =
  { id :: Int
  , title :: String
  , done :: Boolean
  , createdAt :: Milliseconds
  }
