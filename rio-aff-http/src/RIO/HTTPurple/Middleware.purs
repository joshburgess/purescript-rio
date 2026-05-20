-- | `withRequestContext`: the reusable HTTP request-wrapping
-- | combinator from the todo-api example.
-- |
-- | Wraps any `RIO` action so it runs inside a `RIO.Aff.Logger`
-- | `withFields` block stamping `request.id` / `request.method`
-- | / `request.path` on every emitted log line, writes the
-- | request id into a `Local String` so downstream code can
-- | correlate without taking it as an argument, and emits a
-- | "request received" / "request completed" or "request failed"
-- | pair around the body that times the action and records the
-- | success/failure verdict.
-- |
-- | Required services on the environment row: `logger :: Logger`,
-- | `requestId :: Local String`, `clock :: Clock`. The wrapped
-- | action's error row is preserved; failures are logged on the
-- | failure path via `catchAll` + `rethrow` and propagated
-- | unchanged.
module RIO.Aff.HTTPurple.Middleware
  ( withRequestContext
  ) where

import Prelude

import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds)
import Type.Proxy (Proxy(..))

import RIO.Aff.Clock (Clock, now)
import RIO.Aff.Core (RIO, ask)
import RIO.Aff.Error (catchAll, rethrow)
import RIO.Aff.Local (Local, locally)
import RIO.Aff.Logger (Logger, logError, logInfo, withFields)

import RIO.Aff.HTTPurple.Request (RequestContext)

-- | Wrap a handler so every log line it emits carries
-- | `request.id` / `request.method` / `request.path`, the
-- | request id is visible via the `requestId` `Local`, and a
-- | "received" / "completed" pair frames the body with timing
-- | and verdict.
-- |
-- | The error row `e` is preserved verbatim. Typed failures
-- | from the wrapped action surface as the caller's `e`; the
-- | only failure-row interaction here is the `catchAll` +
-- | `rethrow` pair that lets us log the failure verdict
-- | without absorbing it.
withRequestContext
  :: forall r e a
   . RequestContext
  -> RIO
       ( logger :: Logger
       , requestId :: Local String
       , clock :: Clock
       | r
       )
       e
       a
  -> RIO
       ( logger :: Logger
       , requestId :: Local String
       , clock :: Clock
       | r
       )
       e
       a
withRequestContext ctx action = withFields
  [ Tuple "request.id" ctx.requestId
  , Tuple "request.method" (show ctx.method)
  , Tuple "request.path" ctx.path
  ]
  do
    reqIdLocal <- ask (Proxy :: Proxy "requestId")
    locally reqIdLocal ctx.requestId (timed action)

timed
  :: forall r e a
   . RIO (logger :: Logger, clock :: Clock | r) e a
  -> RIO (logger :: Logger, clock :: Clock | r) e a
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

logSucceeded
  :: forall r e
   . Number
  -> RIO (logger :: Logger | r) e Unit
logSucceeded ms = withFields
  [ Tuple "duration_ms" (show ms), Tuple "result" "ok" ]
  (logInfo "request completed")

logFailed
  :: forall r e
   . Number
  -> RIO (logger :: Logger | r) e Unit
logFailed ms = withFields
  [ Tuple "duration_ms" (show ms), Tuple "result" "error" ]
  (logError "request failed")
