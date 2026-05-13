-- | Production-ish implementations and the layer that wires them.
-- |
-- | The store is an in-memory `Ref (Array Todo)` with an
-- | auto-incrementing id counter, freshly allocated inside the
-- | layer. The logger is `RIO.Logger.consoleLogger`. The clock
-- | is `liveClock` from `RIO.Clock`.
-- |
-- | A test rig could swap any of these via `provide` /
-- | `provideLayer` without touching the handlers.
module Example.TodoApi.Layers
  ( appLayer
  , inMemoryStore
  ) where

import Prelude hiding ((>>>))

import Data.Array (filter, find, snoc) as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Core (Layer, fromRIO)
import RIO.Layer ((<+>))
import RIO.Logger (consoleLogger)

import Example.TodoApi.Services (Logger, Todo, TodoStore)

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

loggerLayer :: forall e. Layer () e (logger :: Logger)
loggerLayer = fromRIO (liftEffect consoleLogger <#> \l -> { logger: l })

storeLayer :: forall e. Layer () e (todoStore :: TodoStore)
storeLayer = fromRIO (liftAff inMemoryStore <#> \store -> { todoStore: store })

-- | The full app layer. Builds the store and the logger; the clock
-- | and request-id `Local` are supplied at startup in `Main.purs`.
appLayer :: forall e. Layer () e (logger :: Logger, todoStore :: TodoStore)
appLayer = loggerLayer <+> storeLayer
