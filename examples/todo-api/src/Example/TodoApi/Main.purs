-- | Entry point: build the layer once, install the HTTPurple
-- | router, run each request as an `RIO` action against the
-- | shared environment.
-- |
-- | Each incoming request is wrapped by `withRequestContext` from
-- | `Example.TodoApi.Middleware`, which scopes a fresh request id
-- | onto the env's `Local String` and tags every emitted log line
-- | with `request.id` / `request.method` / `request.path`.
-- | Mutating routes (POST, DELETE) additionally call `requireAuth`
-- | which raises the `unauthorized` typed failure if the
-- | Authorization header doesn't match.
-- |
-- | Run with `npx spago run -p rio-example-todo-api`; the server
-- | listens on `localhost:8080`. A quick smoke test from another
-- | terminal:
-- |
-- | ```
-- | curl -s localhost:8080/todos
-- | curl -s -X POST -d '{"title":"buy milk"}' \
-- |      -H 'Authorization: Bearer example-token' localhost:8080/todos
-- | curl -s localhost:8080/todos
-- | curl -s localhost:8080/todos/0
-- | curl -s -X DELETE -H 'Authorization: Bearer example-token' \
-- |      localhost:8080/todos/0
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
import Effect.Aff (launchAff_)
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

import RIO.Clock (Clock, liveClock)
import RIO.Core (RIO, provideAll, runRIO)
import RIO.Layer (buildLayer)
import RIO.Local (Local, newLocalEffect)
import RIO.Logger (Logger)

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
import Example.TodoApi.Layers (appLayer)
import Example.TodoApi.Middleware
  ( RequestContext
  , defaultAuthConfig
  , requireAuth
  , withRequestContext
  )
import Example.TodoApi.Routes (Route(..), route)
import Example.TodoApi.Services (TodoStore)

type AppEnv =
  { logger :: Logger
  , todoStore :: TodoStore
  , clock :: Clock
  , requestId :: Local String
  , counter :: Ref.Ref Int
  }

main :: Effect Unit
main = launchAff_ do
  built <- buildLayer appLayer
  case built of
    Left _ -> liftEffect (log "todo-api: layer build failed")
    Right base -> liftEffect do
      requestId <- newLocalEffect "<unset>"
      counter <- Ref.new 0
      let
        env =
          { logger: base.logger
          , todoStore: base.todoStore
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
  , todoStore: env.todoStore
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
  }
