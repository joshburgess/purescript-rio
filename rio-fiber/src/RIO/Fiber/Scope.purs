-- | Scope-based resource management for `rio-fiber`.
-- |
-- | A `Scope` is a value that owns a list of cleanup actions. The
-- | usual pattern is `scoped \scope -> ...`: inside the body you
-- | `acquireRelease scope acquire release` for each resource you
-- | open, and when the body exits (success, typed failure, defect,
-- | or interrupt) every registered release fires in LIFO order.
-- |
-- | The MVP stores releases as `Effect Unit` finalizers. RIO-valued
-- | releases are bridged via `runRIOCallback` and are therefore
-- | fire-and-forget at close time. Synchronous cleanups complete
-- | inline; cleanups that themselves suspend may still be in flight
-- | when `closeScope` returns.
module RIO.Fiber.Scope
  ( module Exports
  , newScope
  , closeScope
  , addFinalizer
  , scoped
  , acquireRelease
  ) where

import Prelude

import Effect (Effect)
import RIO.Fiber.Core (RIO, ask, ensuring, liftEffect, runRIOCallback, uninterruptible)
import RIO.Fiber.Internal (Scope)
import RIO.Fiber.Internal (Scope) as Exports
import RIO.Fiber.Internal as Internal

-- | Allocate a fresh scope. Most users want `scoped` instead, which
-- | both creates the scope and guarantees its closure.
newScope :: forall r e. RIO r e Scope
newScope = liftEffect Internal._newScope

-- | Fire every finalizer currently registered with the scope in
-- | LIFO order. Re-closing a closed scope is a no-op. Individual
-- | finalizers may throw; their throws are swallowed so one bad
-- | finalizer doesn't strand the rest.
closeScope :: forall r e. Scope -> RIO r e Unit
closeScope s = liftEffect (Internal._closeScope s)

-- | Register a synchronous `Effect Unit` cleanup with the scope.
-- | Closing the scope after registration runs this finalizer in
-- | LIFO order with the others.
addFinalizer :: forall r e. Scope -> Effect Unit -> RIO r e Unit
addFinalizer s fin = liftEffect (Internal._addFinalizerEff s fin)

-- | Run `body` against a fresh scope. The scope closes on exit
-- | regardless of how `body` terminates (success, typed failure,
-- | defect, or interrupt).
scoped :: forall r e a. (Scope -> RIO r e a) -> RIO r e a
scoped body = do
  s <- newScope
  ensuring (closeScope s) (body s)

-- | Acquire a resource and register its release with the scope.
-- | The acquire step is uninterruptible so an interrupt between
-- | allocating the resource and installing the release cannot leak.
-- | The release runs when the scope is closed; it is dispatched
-- | through `runRIOCallback` with the env captured at acquire time,
-- | so it observes whatever services were present when the resource
-- | was opened.
acquireRelease
  :: forall r e a
   . Scope
  -> RIO r e a
  -> (a -> RIO r e Unit)
  -> RIO r e a
acquireRelease scope acquire release = uninterruptible do
  resource <- acquire
  env <- ask
  let
    releaseEff :: Effect Unit
    releaseEff = do
      _ <- runRIOCallback (release resource) env (\_ -> pure unit)
      pure unit
  addFinalizer scope releaseEff
  pure resource
