-- | HTTP handlers expressed as `RIO` programs.
-- |
-- | Each handler returns an `RIO` action over the app's service row
-- | (`logger`, `postgres`, `clock`, `requestId`). Typed failures
-- | (`notFound`, `unauthorized`, `db`) flow up through the error
-- | row; the bridging function in `Main.purs` runs each handler
-- | with `runRIO` and turns its result into an HTTP `Response`.
-- |
-- | Persistence is `rio-postgres` directly: handlers call `query`,
-- | `queryParams`, and `execParams` against the pool service held
-- | in the env under `postgres`. The chosen typed-failure tag for
-- | driver errors is `db`.
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
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy(..))

import RIO.Clock (now)
import RIO.Core (RIO, fail)
import RIO.Logger (logInfo, withFields)
import RIO.Postgres (PgError, Postgres, execParams, query, queryParams)

import Example.TodoApi.Codecs (CreateTodo)
import Example.TodoApi.Services (Clock, Local, Logger, Todo)

-- | The row of services every handler runs against.
type Env =
  ( logger :: Logger
  , postgres :: Postgres
  , clock :: Clock
  , requestId :: Local String
  )

-- | Typed failures the handlers can raise.
type ApiError =
  ( notFound :: { id :: Int }
  , unauthorized :: Unit
  , db :: PgError
  )

dbTag :: Proxy "db"
dbTag = Proxy

-- | Shape of a row as decoded straight from `select id, title, done,
-- | created_at_ms from rio_todos`. `Number` is the `double precision`
-- | column, re-wrapped into `Milliseconds` for the response model.
type TodoRow = Int /\ String /\ Boolean /\ Number

rowToTodo :: TodoRow -> Todo
rowToTodo (id /\ title /\ done /\ ms) =
  { id, title, done, createdAt: Milliseconds ms }

-- | GET /todos
listHandler :: RIO Env ApiError (Array Todo)
listHandler = do
  logInfo "list todos"
  rows <- query dbTag
    ( "select id, title, done, created_at_ms from rio_todos order by id"
        :: String
    )
  pure (map rowToTodo (rows :: Array TodoRow))

-- | GET /todos/:id
getHandler :: Int -> RIO Env ApiError Todo
getHandler tid = withFields [ Tuple "todo.id" (show tid) ] do
  logInfo "fetch todo"
  row <- queryParams dbTag
    "select id, title, done, created_at_ms from rio_todos where id = $1"
    tid
  case (row :: Maybe TodoRow) of
    Nothing -> fail (Proxy :: Proxy "notFound") { id: tid }
    Just r -> pure (rowToTodo r)

-- | POST /todos. `body` is the already-decoded payload; the
-- | upstream JSON-decode failure is rendered as 400 directly by
-- | the Main router and never reaches this handler.
createHandler :: CreateTodo -> RIO Env ApiError Todo
createHandler body = withFields [ Tuple "todo.title" body.title ] do
  ts <- now
  logInfo "create todo"
  row <- queryParams dbTag
    "insert into rio_todos (title, created_at_ms) values ($1, $2) \
    \returning id, title, done, created_at_ms"
    (body.title /\ unwrap ts)
  case (row :: Maybe TodoRow) of
    Just r -> pure (rowToTodo r)
    Nothing -> unsafeCrashWith
      "todo-api: insert ... returning produced zero rows"

-- | DELETE /todos/:id. Returns `unit` on success; on miss, raises
-- | `notFound` rather than silently 204-ing.
deleteHandler :: Int -> RIO Env ApiError Unit
deleteHandler tid = withFields [ Tuple "todo.id" (show tid) ] do
  logInfo "delete todo"
  affected <- execParams dbTag
    "delete from rio_todos where id = $1"
    tid
  if affected > 0 then pure unit
  else fail (Proxy :: Proxy "notFound") { id: tid }
