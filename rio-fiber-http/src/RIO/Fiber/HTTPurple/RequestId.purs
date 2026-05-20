-- | A per-fiber request-id slot.
-- |
-- | `RIO.Fiber.HTTPurple.Middleware.withRequestContext` stashes the
-- | per-request id here so downstream `RIO` code can read it
-- | (via `getRequestId`) without taking it as an argument. The
-- | slot is backed by a `FiberRef`, so:
-- |
-- | * Each fiber sees the value set by the nearest enclosing
-- |   `withRequestId` (or `withRequestContext`).
-- | * Children forked from inside the block inherit the value
-- |   at the point of fork.
-- | * Outside any `withRequestId` block, reads return `"<no request>"`.
module RIO.Fiber.HTTPurple.RequestId
  ( getRequestId
  , withRequestId
  ) where

import Prelude

import Effect.Unsafe (unsafePerformEffect)

import RIO.Fiber.Core (RIO, ensuring)
import RIO.Fiber.Internal (FiberRef)
import RIO.Fiber.Ref (getFiberRef, newFiberRef, setFiberRef)

requestIdRef :: FiberRef String
requestIdRef = unsafePerformEffect (newFiberRef "<no request>")

-- | Read the current request id from the per-fiber slot. Returns
-- | `"<no request>"` outside any `withRequestId` block.
getRequestId :: forall r e. RIO r e String
getRequestId = getFiberRef requestIdRef

-- | Run `body` with `rid` installed as the active request id;
-- | the previous value is restored on exit.
withRequestId :: forall r e a. String -> RIO r e a -> RIO r e a
withRequestId rid body = do
  prev <- getRequestId
  setFiberRef requestIdRef rid
  ensuring (setFiberRef requestIdRef prev) body
