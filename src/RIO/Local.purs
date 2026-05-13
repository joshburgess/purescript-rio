-- | Ambient state with scoped overrides.
-- |
-- | A `Local a` is a typed cell whose value is read by `get`,
-- | written by `set` / `update`, and overridden for the dynamic
-- | extent of a block by `locally`. The intended use is the same
-- | as ZIO `FiberRef` or Effect-TS `Context.Tag` for transient
-- | values: correlation IDs, request-scoped config, the current
-- | tenant, the current span hint.
-- |
-- | ## Inheritance and concurrency
-- |
-- | A `Local` is backed by an `Effect.Ref`, so it is *shared* by
-- | every fiber that holds a reference to it (i.e. has the same
-- | environment row). A forked fiber sees whatever value is in
-- | the cell at the time of each read; a write from any fiber is
-- | observable by every other fiber. This is the implicit-context
-- | model `RIO.Tracer` already uses, and it works correctly for
-- | the common case where:
-- |
-- |   - the parent sets a value with `locally` and the child
-- |     fiber, started inside that block, reads it;
-- |   - mutation is rare (set once at the request entry,
-- |     read-only downstream);
-- |   - the child fiber is awaited before the `locally` block
-- |     exits (so the restore happens after the child has read
-- |     what it needed).
-- |
-- | It is *not* equivalent to a true `FiberRef` in a runtime
-- | that snapshots per-fiber state at fork time. If a child
-- | fiber must operate on its own private copy that does not
-- | leak back to the parent, capture the value with `get` at
-- | the fork point and pass it in explicitly. The runtime here
-- | is Aff; we do not have hooks to instrument fork itself.
-- |
-- | ## locally and termination
-- |
-- | `locally fl value action` snapshots the current value,
-- | writes `value`, runs `action`, and restores the snapshot.
-- | The restore is guaranteed by `Aff.finally`, so it runs on
-- | every termination path: success, typed failure, defect, and
-- | fiber interruption mid-action.
module RIO.Local
  ( Local
  , get
  , locally
  , newLocal
  , newLocalEffect
  , set
  , update
  ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (finally)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Internal (RIO(..), unRIO)

-- | A read/write cell that survives across calls within a
-- | program and can be temporarily overridden by `locally`. The
-- | constructor is hidden; use `newLocal` / `newLocalEffect` to
-- | create one.
newtype Local a = Local (Ref a)

-- | Create a fresh `Local` initialised to `value`. Infallible
-- | from the caller's perspective; the error row is left open
-- | so it composes in any do-block.
-- |
-- | ```purescript
-- | -- create at startup, then place in the env record:
-- | rid <- newLocal "<no request>"
-- | runRIO' (provideAll { requestId: rid } program)
-- | ```
newLocal :: forall r e' a. a -> RIO r e' (Local a)
newLocal value = RIO \_ -> do
  ref <- liftEffect (Ref.new value)
  pure (Right (Local ref))

-- | `Effect`-typed variant for callers that build their
-- | environment record outside an `RIO` action (e.g. at the
-- | top of `main` before `launchAff_`).
-- |
-- | ```purescript
-- | main = do
-- |   rid <- newLocalEffect "<no request>"
-- |   launchAff_ (runRIO' (provideAll { requestId: rid } program))
-- | ```
newLocalEffect :: forall a. a -> Effect (Local a)
newLocalEffect value = Local <$> Ref.new value

-- | Read the current value.
get :: forall r e a. Local a -> RIO r e a
get (Local ref) = RIO \_ -> do
  v <- liftEffect (Ref.read ref)
  pure (Right v)

-- | Overwrite the value. Visible to every fiber holding the
-- | same `Local`.
set :: forall r e a. Local a -> a -> RIO r e Unit
set (Local ref) value = RIO \_ -> do
  liftEffect (Ref.write value ref)
  pure (Right unit)

-- | Apply a pure function to the current value and store the
-- | result.
update :: forall r e a. Local a -> (a -> a) -> RIO r e Unit
update (Local ref) f = RIO \_ -> do
  liftEffect (Ref.modify_ f ref)
  pure (Right unit)

-- | Run `action` with the value temporarily set to `value`.
-- | The previous value is restored when `action` returns,
-- | regardless of how it terminates (success, typed failure,
-- | defect, or interrupt). `locally` blocks nest naturally: an
-- | inner `locally` restores to whatever the outer block had
-- | set, not to the original.
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
locally (Local ref) value action = RIO \r -> do
  previous <- liftEffect (Ref.read ref)
  liftEffect (Ref.write value ref)
  finally
    (liftEffect (Ref.write previous ref))
    (unRIO action r)
