-- | Per-fiber state for `rio-fiber`.
-- |
-- | A `FiberRef a` is a mutable cell whose value is private to a
-- | fiber. Forking copies the parent's current value into the child,
-- | so the child starts with whatever the parent held at fork time;
-- | subsequent writes in either fiber are invisible to the other.
-- |
-- | The typical use is contextual state that should flow with the
-- | computation, but not leak across concurrent branches: a request
-- | id, a logging context, a current span, etc. For shared mutable
-- | state across fibers, reach for `Effect.Ref` instead.
module RIO.Fiber.Ref
  ( module Exports
  , newFiberRef
  , getFiberRef
  , setFiberRef
  , modifyFiberRef
  ) where

import Prelude

import Effect (Effect)
import RIO.Fiber.Internal (FiberRef, RIO(..))
import RIO.Fiber.Internal (FiberRef) as Exports
import RIO.Fiber.Internal as Internal

-- | Allocate a fresh `FiberRef` with the given initial value. The
-- | initial value is what every fiber sees until it (or one of its
-- | ancestors) writes a different value.
newFiberRef :: forall a. a -> Effect (FiberRef a)
newFiberRef = Internal._newFiberRef

-- | Read the current fiber's view of the ref.
getFiberRef :: forall r e a. FiberRef a -> RIO r e a
getFiberRef ref = RIO (Internal.opGetFiberRef ref)

-- | Replace the current fiber's view of the ref. Sibling fibers
-- | (forked earlier) are unaffected.
setFiberRef :: forall r e a. FiberRef a -> a -> RIO r e Unit
setFiberRef ref value = RIO (Internal.opSetFiberRef ref value)

-- | Update the current fiber's view of the ref by applying `f`.
modifyFiberRef :: forall r e a. FiberRef a -> (a -> a) -> RIO r e Unit
modifyFiberRef ref f = RIO (Internal.opModifyFiberRef ref f)
