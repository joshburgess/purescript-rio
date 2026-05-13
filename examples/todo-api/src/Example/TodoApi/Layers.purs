-- | Production-ish implementations and the layer that wires them.
-- |
-- | The store is an in-memory `Ref (Array Todo)` with an
-- | auto-incrementing id counter, freshly allocated inside the
-- | layer. The logger writes to stdout via `Console.log`. The clock
-- | is `liveClock` from `RIO.Clock`.
-- |
-- | A test rig could swap any of these via `provide` /
-- | `provideLayer` without touching the handlers.
module Example.TodoApi.Layers
  ( appLayer
  , consoleLogger
  , inMemoryStore
  ) where

import Prelude hiding ((>>>))

import Data.Array (filter, find, snoc) as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log) as Console
import Effect.Ref as Ref

import RIO.Core (Layer, fromRIO, fromRecord)
import RIO.Layer ((<+>))

import Example.TodoApi.Services (Logger, Todo, TodoStore)

consoleLogger :: Logger
consoleLogger = { log: \s -> Console.log s }

-- | The store is allocated inside `fromRIO` so the counter and rows
-- | live as long as the surrounding scope. A handler-per-request
-- | model means the store is created once at server start, then
-- | shared across requests via the `provideAll` boundary in
-- | `Main.purs`.
inMemoryStore :: Aff TodoStore
inMemoryStore = do
  rowsRef <- liftEffect (Ref.new ([] :: Array Todo))
  nextIdRef <- liftEffect (Ref.new 1)
  pure
    { list: liftEffect (Ref.read rowsRef)
    , get: \tid -> liftEffect do
        rows <- Ref.read rowsRef
        pure (Array.find (\r -> r.id == tid) rows)
    , create: \{ title, createdAt } -> liftEffect do
        tid <- Ref.modify (_ + 1) nextIdRef
        let todo = { id: tid - 1, title, done: false, createdAt }
        Ref.modify_ (\xs -> Array.snoc xs todo) rowsRef
        pure todo
    , delete: \tid -> liftEffect do
        rows <- Ref.read rowsRef
        let
          present = case Array.find (\r -> r.id == tid) rows of
            Just _ -> true
            Nothing -> false
        when present
          (Ref.write (Array.filter (\r -> r.id /= tid) rows) rowsRef)
        pure present
    }

-- | The full app layer. Builds the store, then horizontally combines
-- | it with the static logger record. Clock is supplied as part of
-- | the layer's input row.
appLayer :: forall e. Layer () e (logger :: Logger, todoStore :: TodoStore)
appLayer =
  fromRecord { logger: consoleLogger }
    <+> fromRIO (liftAff inMemoryStore <#> \store -> { todoStore: store })
