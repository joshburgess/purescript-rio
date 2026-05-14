-- | Entry point: read `DATABASE_URL`, build the layer, run the
-- | one-shot schema migration, install the HTTPurple router, and
-- | park forever so the layer's scope keeps the pool alive while
-- | the server serves requests.
-- |
-- | Each incoming request is wrapped by `withRequestContext` from
-- | `Example.TodoApi.Middleware`, which scopes a fresh request id
-- | onto the env's `Local String` and tags every emitted log line
-- | with `request.id` / `request.method` / `request.path`.
-- | Mutating routes (POST, DELETE) additionally call `requireAuth`
-- | which raises the `unauthorized` typed failure if the
-- | Authorization header doesn't match.
-- |
-- | Run with:
-- |
-- | ```
-- | docker compose up -d postgres
-- | export DATABASE_URL="postgres://rio:rio@localhost:55432/rio_test"
-- | npx spago run -p rio-example-todo-api
-- | ```
-- |
-- | The server listens on `localhost:8080`. Smoke test from another
-- | terminal:
-- |
-- | ```
-- | curl -s localhost:8080/todos
-- | curl -s -X POST -d '{"title":"buy milk"}' \
-- |      -H 'Authorization: Bearer example-token' localhost:8080/todos
-- | curl -s localhost:8080/todos
-- | curl -s localhost:8080/todos/1
-- | curl -s -X DELETE -H 'Authorization: Bearer example-token' \
-- |      localhost:8080/todos/1
-- | curl -s -X POST -d '{"title":"x"}' localhost:8080/todos   # 401
-- | curl -s localhost:8080/todos/999                          # 404
-- | curl -s -X POST -d 'not-json' \
-- |      -H 'Authorization: Bearer example-token' localhost:8080/todos   # 400
-- | ```
module Example.TodoApi.Main (main) where

import Prelude

import Control.Monad.Cont (runContT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import HTTPurple
  ( JsonEncoder
  , Method(..)
  , Request
  , ResponseM
  , badRequest
  , jsonHeaders
  , lookup
  , methodNotAllowed
  , noContent
  , ok'
  , response
  , serve
  )
import HTTPurple.Json (fromJsonE, toJson)
import HTTPurple.Status as Status
import Node.Process (lookupEnv)

import RIO.Clock (Clock, liveClock)
import RIO.Core (RIO, provideAll, runRIO)
import RIO.Env (ask)
import RIO.Layer (provideLayer)
import RIO.Local (Local, newLocalEffect)
import RIO.Logger (Logger)
import RIO.Postgres (PgError, Postgres, pgErrorMessage)
import Type.Proxy (Proxy(..))

import Example.TodoApi.Codecs
  ( CreateTodo
  , createTodoDecoder
  , decodeError
  , encodeTodo
  , encodeTodos
  )
import Example.TodoApi.Handlers
  ( ApiError
  , Env
  , createHandler
  , deleteHandler
  , getHandler
  , listHandler
  )
import Example.TodoApi.Layers (appLayer, migrate)
import Example.TodoApi.Middleware
  ( RequestContext
  , defaultAuthConfig
  , requireAuth
  , withRequestContext
  )
import Example.TodoApi.Routes (Route(..), route)

type AppEnv =
  { logger :: Logger
  , postgres :: Postgres
  , clock :: Clock
  , requestId :: Local String
  , counter :: Ref.Ref Int
  }

main :: Effect Unit
main = launchAff_ do
  mConn <- liftEffect (lookupEnv "DATABASE_URL")
  case mConn of
    Nothing -> liftEffect
      ( log
          "todo-api: set DATABASE_URL (e.g. postgres://rio:rio@localhost:55432/rio_test) before starting"
      )
    Just connectionString -> runWithPool connectionString

runWithPool :: String -> Aff Unit
runWithPool connectionString = do
  result <- runRIO
    (provideLayer (appLayer connectionString) (boot connectionString))
  case result of
    Left v -> liftEffect (log ("todo-api: startup failed: " <> renderStartupError v))
    Right _ -> pure unit

-- | Errors that can escape the layer-scoped program: a `db` failure
-- | from `migrate`, plus the `()` carried by `appLayer` (which is
-- | uninhabited but unions in via `provideLayer`).
type StartupError = (db :: PgError)

renderStartupError :: Variant StartupError -> String
renderStartupError = Variant.case_
  # Variant.on (Proxy :: Proxy "db") pgErrorMessage

-- | The program that runs under `provideLayer (appLayer ...)`:
-- | migrate, hand the layer's logger + pool to the HTTP server,
-- | and park forever so the pool's finalizer doesn't fire until
-- | the process is killed.
boot
  :: String
  -> RIO (logger :: Logger, postgres :: Postgres) StartupError Unit
boot _connectionString = do
  migrate
  logger <- ask (Proxy :: Proxy "logger")
  postgres <- ask (Proxy :: Proxy "postgres")
  liftAff (startServer { logger, postgres })
  parkForever

-- | An `Aff` that never resolves. Holding the calling fiber inside
-- | `provideLayer`'s bracket keeps the pool's scope open for the
-- | lifetime of the Node process; on SIGINT/SIGTERM the process is
-- | killed by the OS and the OS reclaims the sockets.
parkForever :: forall r e. RIO r e Unit
parkForever = liftAff
  (makeAff \_ -> pure nonCanceler)

startServer
  :: { logger :: Logger, postgres :: Postgres }
  -> Aff Unit
startServer base = liftEffect do
  requestId <- newLocalEffect "<unset>"
  counter <- Ref.new 0
  let
    env =
      { logger: base.logger
      , postgres: base.postgres
      , clock: liveClock
      , requestId
      , counter
      }
  _ <- serve { port: 8080 } { route, router: mkRouter env }
  log "todo-api: listening on http://localhost:8080"

mkRouter :: AppEnv -> Request Route -> ResponseM
mkRouter env req = do
  ctx <- liftEffect (mkRequestContext env req)
  case req.method, req.route of
    Get, Todos ->
      runWithMiddleware env ctx listHandler (renderJsonOk encodeTodos)
    Get, TodoById tid ->
      runWithMiddleware env ctx (getHandler tid) (renderJsonOk encodeTodo)
    Post, Todos ->
      runContT
        ( fromJsonE createTodoDecoder
            (\err -> badRequest (decodeError err))
            req.body
        )
        ( \(payload :: CreateTodo) -> runWithMiddleware
            env
            ctx
            (requireAuth defaultAuthConfig ctx.headers *> createHandler payload)
            (renderJsonOk encodeTodo)
        )
    Delete, TodoById tid ->
      runWithMiddleware
        env
        ctx
        (requireAuth defaultAuthConfig ctx.headers *> deleteHandler tid)
        (\_ -> noContent)
    _, _ -> methodNotAllowed

-- | Capture the HTTP-shaped values the middleware needs. The
-- | request id is taken from the inbound `X-Request-Id` header if
-- | present, otherwise generated locally so logs are still
-- | correlatable for ad-hoc curls.
mkRequestContext :: AppEnv -> Request Route -> Effect RequestContext
mkRequestContext env req = do
  rid <- case lookup req.headers "X-Request-Id" of
    Just s -> pure s
    Nothing -> do
      n <- Ref.modify (_ + 1) env.counter
      pure ("req-" <> show n)
  pure
    { method: req.method
    , path: req.url
    , requestId: rid
    , headers: req.headers
    }

-- | Run a handler under the middleware wrapper and render the
-- | resulting `Either` to an HTTP response.
runWithMiddleware
  :: forall a
   . AppEnv
  -> RequestContext
  -> RIO Env ApiError a
  -> (a -> ResponseM)
  -> ResponseM
runWithMiddleware env ctx action onOk = do
  result <- runRIO (provideAll (envOf env) (withRequestContext ctx action))
  case result of
    Right a -> onOk a
    Left v -> renderApiError ctx.requestId v

envOf :: AppEnv -> { | Env }
envOf env =
  { logger: env.logger
  , postgres: env.postgres
  , clock: env.clock
  , requestId: env.requestId
  }

renderJsonOk :: forall a. JsonEncoder a -> a -> ResponseM
renderJsonOk encoder a = ok' jsonHeaders (toJson encoder a)

renderApiError :: String -> Variant ApiError -> ResponseM
renderApiError reqId = Variant.match
  { notFound: \{ id } ->
      response Status.notFound
        ("todo " <> show id <> " not found (request " <> reqId <> ")")
  , unauthorized: \_ ->
      response Status.unauthorized
        ("missing or invalid Authorization header (request " <> reqId <> ")")
  , db: \pgErr ->
      response Status.internalServerError
        ( "database error (request "
            <> reqId
            <> "): "
            <> pgErrorMessage pgErr
        )
  }
