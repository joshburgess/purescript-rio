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
  , forkScoped
  , supervised
  , forkSupervised
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Core (RIO, ask, die, ensuring, fork, liftEffect, runRIOCallback, uninterruptible)
import RIO.Fiber.Internal (Fiber, Scope)
import RIO.Fiber.Internal (Scope) as Exports
import RIO.Fiber.Internal as Internal
import RIO.Fiber.Ref (FiberRef, getFiberRef, newFiberRef, setFiberRef)

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

-- | Fork a child fiber bound to the given scope. When the scope
-- | closes (for any reason), the child fiber is interrupted via a
-- | registered finalizer. If the scope is already closed at the
-- | moment of `forkScoped`, the finalizer fires immediately and the
-- | child is interrupted right after start.
-- |
-- | The fork + finalizer install pair runs in an uninterruptible
-- | region so an interrupt cannot land between starting the child
-- | and registering its cleanup.
forkScoped
  :: forall r e a. Scope -> RIO r e a -> RIO r e (Fiber e a)
forkScoped scope action = uninterruptible do
  fib <- fork action
  addFinalizer scope (Internal.interruptFiber fib)
  pure fib

-- | Per-fiber ambient supervisor scope used by `supervised` and
-- | `forkSupervised`. `Nothing` means no supervisor is currently
-- | active; nested `supervised` blocks restore the previous value
-- | on exit.
supervisorScopeRef :: FiberRef (Maybe Scope)
supervisorScopeRef = unsafePerformEffect (newFiberRef Nothing)

-- | Run `body` under a fresh supervisor scope. Inside the body,
-- | `forkSupervised` forks children bound to this scope; when the
-- | body exits (for any reason) the scope closes and every child
-- | still alive is interrupted.
-- |
-- | `supervised` blocks nest: the body sees a fresh scope, and the
-- | previous ambient scope (if any) is restored when the body
-- | returns.
supervised :: forall r e a. RIO r e a -> RIO r e a
supervised body = scoped \scope -> do
  prev <- getFiberRef supervisorScopeRef
  setFiberRef supervisorScopeRef (Just scope)
  ensuring (setFiberRef supervisorScopeRef prev) body

-- | Fork a child fiber bound to the ambient supervisor scope
-- | installed by `supervised`. Calling `forkSupervised` outside any
-- | `supervised` block is a defect: the child would have no owner
-- | and could leak past its caller.
forkSupervised :: forall r e a. RIO r e a -> RIO r e (Fiber e a)
forkSupervised action = do
  mscope <- getFiberRef supervisorScopeRef
  case mscope of
    Just s -> forkScoped s action
    Nothing ->
      die (error "rio-fiber: forkSupervised called outside `supervised`")
