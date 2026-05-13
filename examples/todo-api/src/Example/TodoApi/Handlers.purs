-- | HTTP handlers expressed as `RIO` programs.
-- |
-- | Each handler returns an `RIO` action over the app's service row
-- | (`logger`, `todoStore`, `clock`). Typed failures (notFound,
-- | invalidBody) flow up through the error row; the bridging
-- | function in `Main.purs` runs each handler with `runRIO` and
-- | turns its result into an HTTP `Response`.
-- |
-- | Pattern: `RIO` handlers stay focused on the domain. HTTP-shaped
-- | concerns (parsing the body, choosing a status code) live in the
-- | thin runner around `runRIO`.
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
import Effect.Aff (Milliseconds)
import Effect.Aff.Class (liftAff)
import Type.Proxy (Proxy(..))

import RIO.Clock (now)
import RIO.Core (RIO, ask, fail)

import Example.TodoApi.Codecs (CreateTodo)
import Example.TodoApi.Services (Clock, Logger, Todo, TodoStore)

-- | The row of services every handler runs against.
type Env =
  ( logger :: Logger
  , todoStore :: TodoStore
  , clock :: Clock
  )

-- | The only typed failure a handler raises. Body-decode
-- | failures are surfaced as HTTP 400 by the bridging code in
-- | `Main.purs` rather than promoted into a typed failure, so
-- | the handler signatures stay focused on the domain.
type ApiError =
  ( notFound :: { id :: Int }
  )

-- | GET /todos
listHandler :: RIO Env ApiError (Array Todo)
listHandler = do
  logger <- ask (Proxy :: Proxy "logger")
  store <- ask (Proxy :: Proxy "todoStore")
  liftAff (logger.log "GET /todos")
  liftAff store.list

-- | GET /todos/:id
getHandler :: Int -> RIO Env ApiError Todo
getHandler tid = do
  logger <- ask (Proxy :: Proxy "logger")
  store <- ask (Proxy :: Proxy "todoStore")
  liftAff (logger.log ("GET /todos/" <> show tid))
  row <- liftAff (store.get tid)
  case row of
    Nothing -> fail (Proxy :: Proxy "notFound") { id: tid }
    Just todo -> pure todo

-- | POST /todos. `body` is the already-decoded payload; the
-- | invalidBody case in `ApiError` is raised by the runner if the
-- | upstream decode fails.
createHandler :: CreateTodo -> RIO Env ApiError Todo
createHandler body = do
  logger <- ask (Proxy :: Proxy "logger")
  store <- ask (Proxy :: Proxy "todoStore")
  ts <- now
  liftAff (logger.log ("POST /todos title=" <> body.title))
  liftAff (store.create { title: body.title, createdAt: ts :: Milliseconds })

-- | DELETE /todos/:id. Returns `unit` on success; on miss, raises
-- | `notFound` rather than silently 204-ing.
deleteHandler :: Int -> RIO Env ApiError Unit
deleteHandler tid = do
  logger <- ask (Proxy :: Proxy "logger")
  store <- ask (Proxy :: Proxy "todoStore")
  liftAff (logger.log ("DELETE /todos/" <> show tid))
  hit <- liftAff (store.delete tid)
  if hit then pure unit
  else fail (Proxy :: Proxy "notFound") { id: tid }
