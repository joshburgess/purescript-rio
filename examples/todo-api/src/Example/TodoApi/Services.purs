-- | Service interfaces for the todo-api example.
-- |
-- | Three services, each a record of `Aff`-valued operations per the
-- | convention in `docs/02-services.md`:
-- |
-- |   * `Logger` writes a string somewhere (stdout in production,
-- |     an in-memory buffer in tests).
-- |   * `TodoStore` persists `Todo` rows. Multiple implementations
-- |     in `Layers.purs` show off the layer-swap story.
-- |   * `Clock` is re-exported from `RIO.Clock` for timestamping.
module Example.TodoApi.Services
  ( Logger
  , Todo
  , TodoStore
  , module RIO.Clock
  ) where

import Prelude (Unit)

import Data.Maybe (Maybe)
import Effect.Aff (Aff, Milliseconds)

import RIO.Clock (Clock)

type Logger =
  { log :: String -> Aff Unit
  }

-- | A todo as stored. `createdAt` is a wall-clock timestamp set by
-- | the handler at insert time so the response can echo it back.
type Todo =
  { id :: Int
  , title :: String
  , done :: Boolean
  , createdAt :: Milliseconds
  }

-- | Persistence interface. All operations are `Aff`-valued so the
-- | service slots into the standard row of services.
type TodoStore =
  { list :: Aff (Array Todo)
  , get :: Int -> Aff (Maybe Todo)
  , create :: { title :: String, createdAt :: Milliseconds } -> Aff Todo
  , delete :: Int -> Aff Boolean
  }
