-- | Service interfaces for the todo-api example.
-- |
-- | Two domain services plus re-exported library services:
-- |
-- |   * `TodoStore` persists `Todo` rows. Multiple implementations
-- |     in `Layers.purs` show off the layer-swap story.
-- |   * `Logger` is `RIO.Logger.Logger`, re-exported here so the
-- |     example reads against one namespace.
-- |   * `Clock` is re-exported from `RIO.Clock` for timestamping.
-- |   * `requestId` is a `Local String` carried in the env; the
-- |     request middleware opens a scope on it per request so
-- |     downstream code (handlers, persistence) can correlate
-- |     anything they emit to the originating HTTP request.
module Example.TodoApi.Services
  ( Todo
  , TodoStore
  , module RIO.Clock
  , module RIO.Local
  , module RIO.Logger
  ) where

import Data.Maybe (Maybe)
import Effect.Aff (Aff, Milliseconds)

import RIO.Clock (Clock)
import RIO.Local (Local)
import RIO.Logger (Logger)

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
