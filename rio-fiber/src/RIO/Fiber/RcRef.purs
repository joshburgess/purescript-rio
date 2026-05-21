-- | Reference-counted scoped resources.
-- |
-- | An `RcRef r e a` lazily acquires its underlying resource the
-- | first time anyone asks for it via `get`, then hands out the same
-- | value to every subsequent caller. Each `get` is tied to a
-- | `Scope`: when that scope closes, the reference count drops by
-- | one, and when it reaches zero the user-supplied `release` runs
-- | and the cell becomes empty again. A later `get` will re-acquire.
-- |
-- | This is the right shape for "expensive shared resource that
-- | multiple components want to use concurrently" such as a database
-- | pool, an HTTP client, or a tracer that needs explicit setup and
-- | teardown.
module RIO.Fiber.RcRef
  ( RcRef
  , make
  , get
  , refCount
  ) where

import Prelude

import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Scope (Scope)
import RIO.Fiber.Scope as Scope
import RIO.Fiber.Semaphore (Semaphore)
import RIO.Fiber.Semaphore as Sem

-- | Internal state of an `RcRef`. The cell is either empty (no
-- | active references, no resource held) or holds `value` with
-- | `count` outstanding references.
data State a = Empty | Held a Int

-- | A reference-counted, lazily-acquired resource handle.
newtype RcRef r e a = RcRef
  { state :: Ref (State a)
  , acquire :: RIO r e a
  , release :: a -> RIO r e Unit
  , gate :: Semaphore
  }

-- | Build an `RcRef`. Neither `acquire` nor `release` runs at
-- | construction time; `acquire` first fires on the first `get` to
-- | a fully-released cell.
make
  :: forall r e a
   . { acquire :: RIO r e a
     , release :: a -> RIO r e Unit
     }
  -> RIO r e (RcRef r e a)
make { acquire, release } = do
  state <- liftEffect (Ref.new Empty)
  gate <- liftEffect (Sem.make 1)
  pure (RcRef { state, acquire, release, gate })

-- | Get a reference to the underlying resource within the lifetime
-- | of `scope`. The first `get` on an empty cell runs `acquire`;
-- | every `get` (first or subsequent) registers a finalizer on
-- | `scope` that decrements the count when the scope closes and
-- | runs `release` once the count reaches zero.
-- |
-- | If `acquire` raises, the cell stays empty so a later `get` can
-- | retry; the failure is re-raised to the caller as usual.
get :: forall r e a. Scope -> RcRef r e a -> RIO r e a
get scope ref@(RcRef rc) = Sem.withPermit rc.gate do
  state <- liftEffect (Ref.read rc.state)
  case state of
    Empty -> do
      a <- rc.acquire
      liftEffect (Ref.write (Held a 1) rc.state)
      Scope.addFinalizerRIO scope (decrement ref)
      pure a
    Held a n -> do
      liftEffect (Ref.write (Held a (n + 1)) rc.state)
      Scope.addFinalizerRIO scope (decrement ref)
      pure a

-- | Decrement the reference count. When the count reaches zero the
-- | cell is emptied and `release` runs against the held value.
-- | Decrements past zero are silently ignored (defensive: should not
-- | happen if every increment came from `get`).
decrement :: forall r e a. RcRef r e a -> RIO r e Unit
decrement (RcRef rc) = Sem.withPermit rc.gate do
  state <- liftEffect (Ref.read rc.state)
  case state of
    Empty -> pure unit
    Held a n
      | n <= 1 -> do
          liftEffect (Ref.write Empty rc.state)
          rc.release a
      | otherwise ->
          liftEffect (Ref.write (Held a (n - 1)) rc.state)

-- | Current outstanding-reference count. Zero means the cell is
-- | empty (no resource held).
refCount :: forall r e a. RcRef r e a -> RIO r e Int
refCount (RcRef rc) = do
  state <- liftEffect (Ref.read rc.state)
  case state of
    Empty -> pure 0
    Held _ n -> pure n
