-- | Request middleware expressed as `RIO` combinators.
-- |
-- | Two combinators:
-- |
-- |   * `withRequestContext` opens a per-request `withFields` block
-- |     stamping `request.id` / `request.method` / `request.path`
-- |     on every log line emitted by the wrapped action. It also
-- |     records the request id in a `Local String` so downstream
-- |     code can reach it without taking it as an argument, and
-- |     logs a "request received" / "request completed" pair
-- |     around the action with elapsed milliseconds and the
-- |     success / failure verdict.
-- |
-- |   * `requireAuth` checks the `Authorization` header against a
-- |     single bearer token. Missing or wrong header raises the
-- |     `unauthorized` typed failure; valid header falls through.
-- |
-- | Both pieces are plain `RIO` combinators. The HTTP-shaped values
-- | (headers, method, path) are captured by the Main router before
-- | crossing into `RIO`.
module Example.TodoApi.Middleware
  ( AuthConfig
  , RequestContext
  , defaultAuthConfig
  , requireAuth
  , withRequestContext
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds)
import Type.Proxy (Proxy(..))

import HTTPurple (Method, RequestHeaders, lookup)

import RIO.Clock (now)
import RIO.Core (RIO, ask, fail)
import RIO.Error (catchAll, rethrow)
import RIO.Local (locally)
import RIO.Logger (logError, logInfo, withFields)

import Example.TodoApi.Handlers (ApiError, Env)

-- | A snapshot of HTTP-shaped values the middleware needs.
type RequestContext =
  { method :: Method
  , path :: String
  , requestId :: String
  , headers :: RequestHeaders
  }

-- | Auth configuration. The example accepts a single fixed bearer
-- | token; production deployments would replace this with a JWT
-- | verifier or a session-store lookup.
type AuthConfig =
  { expected :: String
  }

defaultAuthConfig :: AuthConfig
defaultAuthConfig = { expected: "Bearer example-token" }

-- | Wrap a handler so every log line it emits carries the request
-- | id, method, and path. Stashes the request id in `Local` for
-- | downstream domain code, times the body, and emits
-- | "request received" before / "request completed" or
-- | "request failed" after.
withRequestContext
  :: forall a
   . RequestContext
  -> RIO Env ApiError a
  -> RIO Env ApiError a
withRequestContext ctx action = withFields
  [ Tuple "request.id" ctx.requestId
  , Tuple "request.method" (show ctx.method)
  , Tuple "request.path" ctx.path
  ]
  do
    reqIdLocal <- ask (Proxy :: Proxy "requestId")
    locally reqIdLocal ctx.requestId (timed action)

timed
  :: forall a
   . RIO Env ApiError a
  -> RIO Env ApiError a
timed action = do
  start <- now
  logInfo "request received"
  result <- catchAll
    ( \v -> do
        end <- now
        logFailed (durationMs start end)
        rethrow v
    )
    action
  end <- now
  logSucceeded (durationMs start end)
  pure result

durationMs :: Milliseconds -> Milliseconds -> Number
durationMs start end = unwrap end - unwrap start

logSucceeded :: Number -> RIO Env ApiError Unit
logSucceeded ms = withFields
  [ Tuple "duration_ms" (show ms), Tuple "result" "ok" ]
  (logInfo "request completed")

logFailed :: Number -> RIO Env ApiError Unit
logFailed ms = withFields
  [ Tuple "duration_ms" (show ms), Tuple "result" "error" ]
  (logError "request failed")

-- | Check the `Authorization` header against `cfg.expected`.
-- | Missing or wrong: raise `unauthorized`. Match: fall through.
requireAuth
  :: AuthConfig
  -> RequestHeaders
  -> RIO Env ApiError Unit
requireAuth cfg hdrs =
  case lookup hdrs "Authorization" of
    Just got | got == cfg.expected -> pure unit
    _ -> fail (Proxy :: Proxy "unauthorized") unit
