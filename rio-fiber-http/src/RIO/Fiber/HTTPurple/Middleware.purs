-- | `withRequestContext`: the reusable HTTP request-wrapping
-- | combinator for the fiber-backed stack.
-- |
-- | Wraps any `RIO` action so the request id is visible via
-- | `RIO.Fiber.HTTPurple.RequestId.getRequestId` for the duration
-- | of the body, and a "request received" / "request completed"
-- | or "request failed" pair is emitted on the active logger around
-- | the body that times the action and records the success or
-- | failure verdict.
-- |
-- | Because `rio-fiber`'s `Logger` is a single
-- | `LogLevel -> String -> Effect Unit` channel (no structured
-- | fields), the request id, method, path, duration, and verdict
-- | are inlined into the message string. The active logger comes
-- | from the `RIO.Fiber.Logger` `FiberRef`, not the environment
-- | row, so the only requirement on the caller is that a `Logger`
-- | / `Clock` has been installed (the default services suffice).
-- | The wrapped action's error row is preserved; failures are
-- | logged on the failure path via `catchAll` and propagated
-- | unchanged via `fail`.
module RIO.Fiber.HTTPurple.Middleware
  ( withRequestContext
  ) where

import Prelude

import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds)

import RIO.Fiber.Clock (currentEpoch)
import RIO.Fiber.Core (RIO, catchAll, fail)
import RIO.Fiber.Logger (info, error) as Logger

import RIO.Fiber.HTTPurple.Request (RequestContext)
import RIO.Fiber.HTTPurple.RequestId (withRequestId)

-- | Wrap a handler so the request id is visible via
-- | `RIO.Fiber.HTTPurple.RequestId.getRequestId` for the duration
-- | of the body, and a "received" / "completed" pair frames the
-- | body with timing and verdict.
-- |
-- | The error row `e` is preserved verbatim. Typed failures from
-- | the wrapped action surface as the caller's `e`; the only
-- | failure-row interaction here is the `catchAll` + re-raise pair
-- | that lets us log the failure verdict without absorbing it.
withRequestContext
  :: forall r e a
   . RequestContext
  -> RIO r e a
  -> RIO r e a
withRequestContext ctx action =
  withRequestId ctx.requestId (timed ctx action)

timed
  :: forall r e a
   . RequestContext
  -> RIO r e a
  -> RIO r e a
timed ctx action = do
  start <- currentEpoch
  Logger.info (received ctx)
  result <- catchAll
    ( \v -> do
        end <- currentEpoch
        Logger.error (failed ctx (durationMs start end))
        fail v
    )
    action
  end <- currentEpoch
  Logger.info (succeeded ctx (durationMs start end))
  pure result

durationMs :: Milliseconds -> Milliseconds -> Number
durationMs start end = unwrap end - unwrap start

received :: RequestContext -> String
received ctx =
  "request received "
    <> "[id="
    <> ctx.requestId
    <> " method="
    <> show ctx.method
    <> " path="
    <> ctx.path
    <> "]"

succeeded :: RequestContext -> Number -> String
succeeded ctx ms =
  "request completed "
    <> "[id="
    <> ctx.requestId
    <> " duration_ms="
    <> show ms
    <> " result=ok]"

failed :: RequestContext -> Number -> String
failed ctx ms =
  "request failed "
    <> "[id="
    <> ctx.requestId
    <> " duration_ms="
    <> show ms
    <> " result=error]"
