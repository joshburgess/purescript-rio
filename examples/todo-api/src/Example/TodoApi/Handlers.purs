-- | HTTP handlers expressed as `RIO` programs.
-- |
-- | Each handler returns an `RIO` action over the app's service row
-- | (`logger`, `todoStore`, `clock`, `requestId`). Typed failures
-- | (`notFound`, `unauthorized`) flow up through the error row;
-- | the bridging function in `Main.purs` runs each handler with
-- | `runRIO` and turns its result into an HTTP `Response`.
-- |
-- | Pattern: `RIO` handlers stay focused on the domain. Logging,
-- | auth, and request-id correlation are layered on by the
-- | middleware wrapper in `Example.TodoApi.Middleware`; this
-- | module is just the domain logic.
module Example.TodoApi.Handlers
  ( Env
  , ApiError
  , createHandler
  , deleteHandler
  , getHandler
  , listHandler
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds)
import Effect.Aff.Class (liftAff)
import Type.Proxy (Proxy(..))

import RIO.Clock (now)
import RIO.Core (RIO, ask, fail)
import RIO.Logger (logInfo, withFields)

import Example.TodoApi.Codecs (CreateTodo)
import Example.TodoApi.Services (Clock, Local, Logger, Todo, TodoStore)

-- | The row of services every handler runs against.
type Env =
  ( logger :: Logger
  , todoStore :: TodoStore
  , clock :: Clock
  , requestId :: Local String
  )

-- | Typed failures the handlers can raise.
type ApiError =
  ( notFound :: { id :: Int }
  , unauthorized :: Unit
  )

-- | GET /todos
listHandler :: RIO Env ApiError (Array Todo)
listHandler = do
  store <- ask (Proxy :: Proxy "todoStore")
  logInfo "list todos"
  liftAff store.list

-- | GET /todos/:id
getHandler :: Int -> RIO Env ApiError Todo
getHandler tid = withFields [ Tuple "todo.id" (show tid) ] do
  store <- ask (Proxy :: Proxy "todoStore")
  logInfo "fetch todo"
  row <- liftAff (store.get tid)
  case row of
    Nothing -> fail (Proxy :: Proxy "notFound") { id: tid }
    Just todo -> pure todo

-- | POST /todos. `body` is the already-decoded payload; the
-- | upstream JSON-decode failure is rendered as 400 directly by
-- | the Main router and never reaches this handler.
createHandler :: CreateTodo -> RIO Env ApiError Todo
createHandler body = withFields [ Tuple "todo.title" body.title ] do
  store <- ask (Proxy :: Proxy "todoStore")
  ts <- now
  logInfo "create todo"
  liftAff (store.create { title: body.title, createdAt: ts :: Milliseconds })

-- | DELETE /todos/:id. Returns `unit` on success; on miss, raises
-- | `notFound` rather than silently 204-ing.
deleteHandler :: Int -> RIO Env ApiError Unit
deleteHandler tid = withFields [ Tuple "todo.id" (show tid) ] do
  store <- ask (Proxy :: Proxy "todoStore")
  logInfo "delete todo"
  hit <- liftAff (store.delete tid)
  if hit then pure unit
  else fail (Proxy :: Proxy "notFound") { id: tid }
