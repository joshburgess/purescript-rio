-- | True per-fiber reference cells, with aff-compatible naming.
-- |
-- | A `FiberRef a` is a typed cell whose value is *private to the
-- | reading fiber*: a child fiber inherits its parent's value at
-- | fork time but subsequent writes from either side do not bleed
-- | into the other. This is the same semantic ZIO's `FiberRef` and
-- | Effect-TS's `FiberRef` ship.
-- |
-- | ## Relationship to `RIO.Fiber.Ref`
-- |
-- | `RIO.Fiber.Ref` is the canonical fiber-native FiberRef module
-- | (`newFiberRef`, `getFiberRef`, `setFiberRef`, `modifyFiberRef`,
-- | `locally`, `locallyWith`). This module re-exposes the same
-- | underlying type with `make` / `get` / `set` / `update` /
-- | `forkFiber` / `forkFiberScoped` names so code ported from
-- | `RIO.Aff.FiberRef` keeps the same call sites. Pick whichever
-- | naming style the surrounding code uses; the underlying cell is
-- | the same `FiberRef` either way.
-- |
-- | ## Inheritance and concurrency
-- |
-- | Unlike `RIO.Aff.FiberRef`, which maintains a per-fiber storage
-- | map carried in the environment row, fiber's `FiberRef` is baked
-- | into the runtime. Forking a child fiber automatically snapshots
-- | the parent's FiberRef view; no `fiberRefs` service is needed in
-- | the environment row. As a consequence:
-- |
-- |   * `make` does not require `(fiberRefs :: FiberRefs | r)` - the
-- |     fiber runtime owns the storage.
-- |   * `forkFiber` is just `RIO.Fiber.Core.fork`; the runtime
-- |     handles the snapshot.
-- |   * `forkFiberScoped` is just `RIO.Fiber.Scope.forkScoped`.
-- |
-- | The fiber-native version is therefore strictly simpler at the
-- | call site than its aff counterpart while delivering the same
-- | semantics.
-- |
-- | ## When to reach for this versus Local
-- |
-- | Use `RIO.Fiber.Local` when the value is set once at entry and
-- | read by every downstream piece of work (typical for a request
-- | id, correlation token, tenant tag). It is a thin newtype over
-- | `FiberRef` with a smaller surface tailored to the
-- | "ambient-context" use case.
-- |
-- | Use `FiberRef` directly when you want explicit access to the
-- | underlying cell and finer-grained operations.
-- |
-- | ```purescript
-- | -- Parent and child each have their own counter.
-- | program = do
-- |   counter <- FiberRef.make 0
-- |   fib <- FiberRef.forkFiber do
-- |     FiberRef.set counter 100
-- |     FiberRef.get counter   -- 100 (child's view)
-- |   childResult <- join fib
-- |   parentValue <- FiberRef.get counter   -- 0 (parent untouched)
-- |   pure { childResult, parentValue }
-- | ```
module RIO.Fiber.FiberRef
  ( module Exports
  , make
  , makeEffect
  , get
  , set
  , update
  , forkFiber
  , forkFiberScoped
  ) where

import Prelude

import Effect (Effect)

import RIO.Fiber.Core (RIO, fork, liftEffect)
import RIO.Fiber.Internal (Fiber)
import RIO.Fiber.Ref (FiberRef)
import RIO.Fiber.Ref (FiberRef) as Exports
import RIO.Fiber.Ref as Ref
import RIO.Fiber.Scope (Scope, forkScoped)

-- | Allocate a fresh `FiberRef` with the given initial value, from
-- | inside `RIO`. The cell is immediately visible to the calling
-- | fiber and to every child fiber forked after this point.
make :: forall r e' a. a -> RIO r e' (FiberRef a)
make initial = liftEffect (Ref.newFiberRef initial)

-- | `Effect`-typed variant for callers that build their environment
-- | record outside an `RIO` action.
makeEffect :: forall a. a -> Effect (FiberRef a)
makeEffect = Ref.newFiberRef

-- | Read the calling fiber's value of the cell.
get :: forall r e a. FiberRef a -> RIO r e a
get = Ref.getFiberRef

-- | Overwrite the calling fiber's value of the cell.
set :: forall r e a. FiberRef a -> a -> RIO r e Unit
set = Ref.setFiberRef

-- | Apply a pure function to the calling fiber's value and store
-- | the result. Equivalent to `get` followed by `set`, but happens
-- | as a single runtime op.
update :: forall r e a. FiberRef a -> (a -> a) -> RIO r e Unit
update = Ref.modifyFiberRef

-- | Fork a child fiber. The child inherits the parent's FiberRef
-- | snapshot at the moment of fork; later writes on either side
-- | stay local to their fiber.
-- |
-- | Aliases `RIO.Fiber.Core.fork`. The snapshot machinery is
-- | handled by the runtime, so no environment-row plumbing is
-- | needed (unlike `RIO.Aff.FiberRef.forkFiber`, which requires
-- | `(fiberRefs :: FiberRefs | r)`).
forkFiber :: forall r e a. RIO r e a -> RIO r e (Fiber e a)
forkFiber = fork

-- | Scope-bounded variant of `forkFiber`: the child's lifetime is
-- | bounded by the supplied `Scope`. Aliases
-- | `RIO.Fiber.Scope.forkScoped`.
forkFiberScoped
  :: forall r e a. Scope -> RIO r e a -> RIO r e (Fiber e a)
forkFiberScoped = forkScoped
