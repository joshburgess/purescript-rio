-- | Reference-counted scoped resources.
-- |
-- | An `RcRef e a` lazily acquires its underlying resource the first
-- | time anyone asks for it via `get`, then hands out the same value
-- | to every subsequent caller. Each `get` is tied to a `Scope`:
-- | when that scope closes, the reference count drops by one, and
-- | when it reaches zero the user-supplied `release` runs and the
-- | cell becomes empty again. A later `get` will re-acquire.
-- |
-- | This is the right shape for "expensive shared resource that
-- | multiple components want to use concurrently" such as a database
-- | pool, an HTTP client, or a tracer that needs explicit setup and
-- | teardown.
-- |
-- | The handle is row-polymorphic in the caller's environment:
-- | `make` bakes its build-time `Record r` into the `acquire` /
-- | `release` closures (the same pattern as `Pool`), so the same
-- | `RcRef` can be passed across `scoped` blocks that extend the row
-- | with services like `scope :: Scope`.
module RIO.Aff.RcRef
  ( RcRef
  , make
  , get
  , refCount
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Aff.Internal (RIO, mkRIO, unRIO, unsafeUnRIO)
import RIO.Aff.Error (rethrow)
import RIO.Aff.Resource (Scope, addFinalizer)
import RIO.Aff.Semaphore (Semaphore)
import RIO.Aff.Semaphore as Sem

-- | Internal state of an `RcRef`. The cell is either empty (no
-- | active references, no resource held) or holds `value` with
-- | `count` outstanding references.
data State a = Empty | Held a Int

-- | A reference-counted, lazily-acquired resource handle. The
-- | environment row is hidden behind closures so a single `RcRef`
-- | can be used from any caller row; the typed error row `e` is
-- | preserved so failures from `acquire` re-surface on the caller's
-- | row at `get` time.
newtype RcRef e a = RcRef
  { state :: Ref (State a)
  , gate :: Semaphore
  , acquireAff :: Aff (Either (Variant e) a)
  , releaseAff :: a -> Aff Unit
  }

-- | Build an `RcRef`. Neither `acquire` nor `release` runs at
-- | construction time; `acquire` first fires on the first `get` to
-- | a fully-released cell.
-- |
-- | `release` is typed with an empty error row to match the
-- | `acquireRelease` convention: cleanup must not fail with a typed
-- | error (handle or absorb failures inside `release` if you need
-- | to). Defects still surface as Aff errors during scope teardown.
make
  :: forall r e a
   . { acquire :: RIO r e a
     , release :: a -> RIO r () Unit
     }
  -> RIO r e (RcRef e a)
make { acquire, release } = mkRIO \env -> do
  state <- liftEffect (Ref.new Empty)
  gate <- liftEffect (Sem.make 1)
  pure
    ( RcRef
        { state
        , gate
        , acquireAff: unRIO acquire env
        , releaseAff: \a -> unsafeUnRIO (release a) env
        }
    )

-- | Get a reference to the underlying resource within the lifetime
-- | of `scope`. The first `get` on an empty cell runs `acquire`;
-- | every `get` (first or subsequent) registers a finalizer on
-- | `scope` that decrements the count when the scope closes and
-- | runs `release` once the count reaches zero.
-- |
-- | If `acquire` raises, the cell stays empty so a later `get` can
-- | retry; the failure is re-raised to the caller as usual.
get :: forall r e a. Scope -> RcRef e a -> RIO r e a
get scope ref@(RcRef rc) = Sem.withPermit rc.gate do
  state <- liftEffect (Ref.read rc.state)
  case state of
    Empty -> do
      result <- mkRIO \_ -> rc.acquireAff
      case result of
        Left v -> rethrow v
        Right a -> do
          liftEffect (Ref.write (Held a 1) rc.state)
          registerDecrement scope ref
          pure a
    Held a n -> do
      liftEffect (Ref.write (Held a (n + 1)) rc.state)
      registerDecrement scope ref
      pure a

-- | Current outstanding-reference count. Zero means the cell is
-- | empty (no resource held).
refCount :: forall r e' e a. RcRef e a -> RIO r e' Int
refCount (RcRef rc) = liftEffect do
  state <- Ref.read rc.state
  case state of
    Empty -> pure 0
    Held _ n -> pure n

-- | Register the decrement action as a scope finalizer. Decrement
-- | runs entirely in `Aff` because both the closure stored in the
-- | RcRef and the finalizer interface are Aff-typed.
registerDecrement
  :: forall r e' e a
   . Scope
  -> RcRef e a
  -> RIO r e' Unit
registerDecrement scope ref = addFinalizer scope (decrementAff ref)

-- | Decrement the reference count. When the count reaches zero the
-- | cell is emptied and `release` runs against the held value.
-- | Decrements past zero are silently ignored.
decrementAff :: forall e a. RcRef e a -> Aff Unit
decrementAff ref@(RcRef rc) = do
  -- Use unsafeUnRIO on a withPermit-wrapped RIO so the gate's
  -- canceler semantics still apply on scope-teardown interruption.
  unsafeUnRIO (Sem.withPermit rc.gate (decrementBody ref)) {}

decrementBody :: forall r e a. RcRef e a -> RIO r e Unit
decrementBody (RcRef rc) = do
  state <- liftEffect (Ref.read rc.state)
  case state of
    Empty -> pure unit
    Held a n
      | n <= 1 -> do
          liftEffect (Ref.write Empty rc.state)
          mkRIO \_ -> rc.releaseAff a
      | otherwise ->
          liftEffect (Ref.write (Held a (n - 1)) rc.state)
