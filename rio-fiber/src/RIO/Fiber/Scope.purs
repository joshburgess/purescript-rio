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
  , closeScopeExit
  , closeScopeAwait
  , closeScopeExitAwait
  , addFinalizer
  , addFinalizerExit
  , addFinalizerRIO
  , scoped
  , scopedAwait
  , acquireRelease
  , forkScoped
  , supervised
  , forkSupervised
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Exception (error)
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Core (RIO, ask, async, die, ensuringWith, fork, liftEffect, runRIOCallback, uninterruptible)
import RIO.Fiber.Internal (Fiber, Scope)
import RIO.Fiber.Internal (Scope) as Exports
import RIO.Fiber.Internal as Internal
import RIO.Fiber.Ref (FiberRef, getFiberRef, locally, newFiberRef)
import Unsafe.Coerce (unsafeCoerce)

-- | Allocate a fresh scope. Most users want `scoped` instead, which
-- | both creates the scope and guarantees its closure.
newScope :: forall r e. RIO r e Scope
newScope = liftEffect Internal._newScope

-- | Fire every finalizer currently registered with the scope in
-- | LIFO order. Re-closing a closed scope is a no-op. Individual
-- | finalizers may throw; their throws are swallowed so one bad
-- | finalizer doesn't strand the rest.
-- |
-- | This is the exit-unaware variant: finalizers registered with
-- | `addFinalizerExit` will observe `Nothing` (success). Use
-- | `closeScopeExit` to surface a real exit cause.
closeScope :: forall r e. Scope -> RIO r e Unit
closeScope s = liftEffect (Internal._closeScope s)

-- | Close the scope with a known exit cause. `Nothing` means the
-- | enclosing body succeeded; `Just c` means it failed, defected, or
-- | was interrupted. The cause is observable to exit-aware finalizers
-- | (those registered via `addFinalizerExit`) for the duration of the
-- | close. Plain finalizers ignore it. Re-closing a closed scope is a
-- | no-op.
closeScopeExit :: forall r e. Scope -> Maybe (Cause e) -> RIO r e Unit
closeScopeExit s mc = liftEffect do
  n <- Internal.maybeCauseToNullable mc
  Internal._closeScopeExit s n

-- | Register a synchronous `Effect Unit` cleanup with the scope.
-- | Closing the scope after registration runs this finalizer in
-- | LIFO order with the others.
addFinalizer :: forall r e. Scope -> Effect Unit -> RIO r e Unit
addFinalizer s fin = liftEffect (Internal._addFinalizerEff s fin)

-- | Register an exit-aware cleanup with the scope. The handler is
-- | invoked with `Nothing` when the scope closes from a success path
-- | and `Just c` when it closes because the body failed, defected,
-- | or was interrupted. Exit info is only routed when the scope was
-- | closed via `closeScopeExit` (or `scoped`, which now does). Plain
-- | `closeScope` always reports `Nothing`.
-- |
-- | Order with other finalizers is LIFO across registrations,
-- | regardless of which `addFinalizer` variant was used.
addFinalizerExit
  :: forall r e
   . Scope
  -> (Maybe (Cause e) -> Effect Unit)
  -> RIO r e Unit
addFinalizerExit s handler = liftEffect (Internal._addFinalizerEff s eff)
  where
  eff :: Effect Unit
  eff = do
    n <- Internal._scopePendingCause s
    handler (Internal.nullableCauseToMaybe n)

-- | Run `body` against a fresh scope. The scope closes on exit
-- | regardless of how `body` terminates (success, typed failure,
-- | defect, or interrupt). The actual exit cause is threaded through
-- | to exit-aware finalizers via `closeScopeExit`.
scoped :: forall r e a. (Scope -> RIO r e a) -> RIO r e a
scoped body = do
  s <- newScope
  ensuringWith (body s) \result ->
    closeScopeExit s case result of
      Right _ -> Nothing
      Left c -> Just c

-- | Register an RIO-valued cleanup with the scope. The action is
-- | bridged through `runRIOCallback` with the env captured at
-- | registration time, so any services it uses come from the scope's
-- | creator, not the closer. The handler is fire-and-forget under
-- | plain `closeScope`; pair with `closeScopeAwait` (or `scopedAwait`)
-- | when you need to be sure the cleanup has actually finished before
-- | proceeding.
addFinalizerRIO
  :: forall r e. Scope -> RIO r e Unit -> RIO r e Unit
addFinalizerRIO scope action = do
  env <- ask
  let
    fin :: Effect Unit
    fin = do
      f <- Internal.startFiber action env
      Internal._scopeAddJoinable scope (eraseFiber f)
  liftEffect (Internal._addFinalizerEff scope fin)

-- | Suspend until every fiber registered with the scope as a
-- | joinable has resolved. Used internally by `closeScopeAwait` and
-- | `scopedAwait`; safe to call directly if you have a scope handle
-- | and just want to drain its registered children.
awaitScopeJoinables :: forall r e. Scope -> RIO r e Unit
awaitScopeJoinables scope = do
  joinables <- liftEffect (Internal._scopeJoinables scope)
  liftEffect (Internal._scopeClearJoinables scope)
  traverse_ awaitOne joinables
  where
  awaitOne :: forall e' a. Fiber e' a -> RIO r e Unit
  awaitOne fib = async \resume -> do
    Internal.observeFiber fib (\_ -> resume (Right unit))
    pure (pure unit)

-- | Like `closeScope`, but also suspends until every fiber registered
-- | with the scope (via `forkScoped` or `addFinalizerRIO`) has fully
-- | resolved. The finalizer pass dispatches interrupts to those
-- | children, and the await then waits for each to acknowledge it.
closeScopeAwait :: forall r e. Scope -> RIO r e Unit
closeScopeAwait s = do
  closeScope s
  awaitScopeJoinables s

-- | Exit-aware variant of `closeScopeAwait`. Threads the close cause
-- | to exit-aware finalizers like `closeScopeExit`, then awaits the
-- | joinable list like `closeScopeAwait`.
closeScopeExitAwait
  :: forall r e. Scope -> Maybe (Cause e) -> RIO r e Unit
closeScopeExitAwait s mc = do
  closeScopeExit s mc
  awaitScopeJoinables s

-- | Like `scoped`, but the body does not return until every joinable
-- | the scope owns has fully resolved. The body's own result and
-- | failure / interrupt semantics are otherwise identical.
scopedAwait :: forall r e a. (Scope -> RIO r e a) -> RIO r e a
scopedAwait body = do
  s <- newScope
  ensuringWith (body s) \result ->
    closeScopeExitAwait s case result of
      Right _ -> Nothing
      Left c -> Just c

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
  liftEffect (Internal._scopeAddJoinable scope (eraseFiber fib))
  pure fib

-- | The runtime fiber object is the same shape regardless of `e`/`a`;
-- | the joinable list erases both so heterogeneous children can share
-- | one list. The await side never observes the inner result.
eraseFiber :: forall e a. Fiber e a -> Fiber () Unit
eraseFiber = unsafeCoerce

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
supervised body = scoped \scope ->
  locally supervisorScopeRef (Just scope) body

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
