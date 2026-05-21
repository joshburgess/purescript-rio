-- | Ambient state with scoped overrides, backed by `FiberRef`.
-- |
-- | A `Local a` is a typed cell whose value is read by `get`, written
-- | by `set` / `update`, and overridden for the dynamic extent of a
-- | block by `locally`. The intended use is the same as ZIO
-- | `FiberRef` or Effect-TS `Context.Tag` for transient values:
-- | correlation IDs, request-scoped config, the current tenant, the
-- | current span hint.
-- |
-- | ## Inheritance and concurrency
-- |
-- | A `Local a` is a thin newtype over `FiberRef a`, so it has the
-- | structured-concurrency semantics fiber's runtime guarantees:
-- |
-- |   * Forking copies the parent's current value into the child at
-- |     fork time. The child starts with the parent's snapshot;
-- |     subsequent writes in either fiber are invisible to the other.
-- |   * `locally fl value action` snapshots the *current fiber's*
-- |     value, writes `value`, runs `action`, and restores the
-- |     snapshot on every termination path (success, typed failure,
-- |     defect, interrupt). Restores are guaranteed by `ensuring`.
-- |   * Nested `locally` blocks behave correctly: an inner `locally`
-- |     restores to whatever the outer block had set, not to the
-- |     original.
-- |
-- | This is a strict improvement over `RIO.Aff.Local`, which is
-- | backed by a single shared `Effect.Ref` and lacks the per-fiber
-- | snapshot semantics that a true `FiberRef` provides.
-- |
-- | For genuinely shared mutable state across fibers (e.g. a counter
-- | every fiber should bump), reach for `RIO.Fiber.Ref` (which wraps
-- | `Effect.Ref`) or `RIO.Fiber.STM.TRef` for transactional shared
-- | state.
module RIO.Fiber.Local
  ( Local
  , get
  , locally
  , newLocal
  , newLocalEffect
  , set
  , update
  ) where

import Prelude

import Effect (Effect)

import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Ref (FiberRef)
import RIO.Fiber.Ref as Ref

-- | A read/write cell with per-fiber semantics. The constructor is
-- | hidden; use `newLocal` / `newLocalEffect` to create one.
newtype Local a = Local (FiberRef a)

-- | Create a fresh `Local` initialised to `value`. Infallible from
-- | the caller's perspective; the error row is left open so it
-- | composes in any do-block.
-- |
-- | ```purescript
-- | -- create at startup, then place in the env record:
-- | rid <- newLocal "<no request>"
-- | runRIO' (provideAll { requestId: rid } program)
-- | ```
newLocal :: forall r e' a. a -> RIO r e' (Local a)
newLocal value = liftEffect (Local <$> Ref.newFiberRef value)

-- | `Effect`-typed variant for callers that build their environment
-- | record outside an `RIO` action (e.g. at the top of `main`).
-- |
-- | ```purescript
-- | main = do
-- |   rid <- newLocalEffect "<no request>"
-- |   ...
-- | ```
newLocalEffect :: forall a. a -> Effect (Local a)
newLocalEffect value = Local <$> Ref.newFiberRef value

-- | Read the current fiber's value.
get :: forall r e a. Local a -> RIO r e a
get (Local ref) = Ref.getFiberRef ref

-- | Overwrite the current fiber's value. Sibling fibers (forked
-- | earlier) are unaffected.
set :: forall r e a. Local a -> a -> RIO r e Unit
set (Local ref) value = Ref.setFiberRef ref value

-- | Apply a pure function to the current fiber's value and store the
-- | result.
update :: forall r e a. Local a -> (a -> a) -> RIO r e Unit
update (Local ref) f = Ref.modifyFiberRef ref f

-- | Run `action` with the value temporarily set to `value`. The
-- | previous value is restored when `action` returns, regardless of
-- | how it terminates (success, typed failure, defect, or interrupt).
-- | `locally` blocks nest naturally.
-- |
-- | ```purescript
-- | handleRequest req = locally requestId req.id do
-- |   logSomething    -- sees req.id
-- |   callDownstream  -- sees req.id
-- | -- after locally exits, requestId is restored
-- | ```
locally
  :: forall r e a b
   . Local a
  -> a
  -> RIO r e b
  -> RIO r e b
locally (Local ref) value action = Ref.locally ref value action
