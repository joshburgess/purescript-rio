-- | Entry point: build the layer once, install the HTTPurple
-- | router, run each request as an `RIO` action against the
-- | shared environment.
-- |
-- | Run with `npx spago run -p rio-example-todo-api`; the server
-- | listens on `localhost:8080`. A quick smoke test from another
-- | terminal:
-- |
-- | ```
-- | curl -s localhost:8080/todos
-- | curl -s -X POST -d '{"title":"buy milk"}' localhost:8080/todos
-- | curl -s localhost:8080/todos
-- | curl -s localhost:8080/todos/0
-- | curl -s -X DELETE localhost:8080/todos/0
-- | curl -s localhost:8080/todos/0     # 404
-- | curl -s -X POST -d 'not-json' localhost:8080/todos   # 400
-- | ```
module Example.TodoApi.Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Control.Monad.Cont (runContT)
import HTTPurple
  ( JsonEncoder
  , Method(..)
  , Request
  , ResponseM
  , badRequest
  , jsonHeaders
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
import Example.TodoApi.Routes (Route(..), route)
import Example.TodoApi.Services (Logger, TodoStore)

type AppEnv =
  { logger :: Logger
  , todoStore :: TodoStore
  , clock :: Clock
  }

main :: Effect Unit
main = launchAff_ do
  built <- buildLayer appLayer
  case built of
    Left _ -> liftEffect (log "todo-api: layer build failed")
    Right base -> liftEffect do
      let env = { logger: base.logger, todoStore: base.todoStore, clock: liveClock }
      _ <- serve { port: 8080 } { route, router: mkRouter env }
      log "todo-api: listening on http://localhost:8080"

mkRouter :: AppEnv -> Request Route -> ResponseM
mkRouter env req = case req.method, req.route of
  Get, Todos ->
    runHandler env listHandler (renderJsonOk encodeTodos)
  Get, TodoById tid ->
    runHandler env (getHandler tid) (renderJsonOk encodeTodo)
  Post, Todos ->
    runContT
      ( fromJsonE createTodoDecoder
          (\err -> badRequest (decodeError err))
          req.body
      )
      ( \(payload :: CreateTodo) ->
          runHandler env (createHandler payload) (renderJsonOk encodeTodo)
      )
  Delete, TodoById tid ->
    runHandler env (deleteHandler tid) (\_ -> noContent)
  _, _ -> methodNotAllowed

-- | Run an `RIO` handler against the shared environment, then
-- | render its result. `onOk` renders successful payloads;
-- | typed-failure responses are produced by `renderApiError`.
runHandler
  :: forall a
   . AppEnv
  -> RIO Env ApiError a
  -> (a -> ResponseM)
  -> ResponseM
runHandler env action onOk = do
  result <- runRIO (provideAll env action)
  case result of
    Right a -> onOk a
    Left v -> renderApiError v

renderJsonOk :: forall a. JsonEncoder a -> a -> ResponseM
renderJsonOk encoder a = ok' jsonHeaders (toJson encoder a)

renderApiError :: Variant ApiError -> ResponseM
renderApiError = Variant.match
  { notFound: \{ id } -> response Status.notFound ("todo " <> show id <> " not found")
  }
