-- | A scope-managed slot that holds at most one running fiber.
-- |
-- | `FiberHandle e a` is a single-slot supervisor: every call to
-- | `run` forks a new fiber, interrupts whichever fiber the slot was
-- | already holding (if any), and stores the fresh fiber in the slot.
-- | When the owning scope closes, the slot's current fiber is
-- | interrupted. When a stored fiber completes on its own, the slot
-- | clears automatically.
-- |
-- | Use this for "last write wins" supervision: a debounce loop, the
-- | latest in-flight request for a UI screen, the single background
-- | poller that should be replaced when configuration changes. It
-- | replaces the manual `Ref (Maybe (Fiber e a))` + interrupt-the-old-
-- | one-on-replace + interrupt-on-scope-close boilerplate.
module RIO.Fiber.FiberHandle
  ( FiberHandle
  , make
  , run
  , get
  , clear
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, fork, liftEffect, uninterruptible)
import RIO.Fiber.Internal (Fiber)
import RIO.Fiber.Internal as Internal
import RIO.Fiber.Scope (Scope, addFinalizer)

type Slot e a = { fiber :: Fiber e a, gen :: Int }

-- | A scope-managed single-fiber slot. Each `run` interrupts the
-- | previous occupant; the slot also clears when its current fiber
-- | finishes on its own.
newtype FiberHandle e a = FiberHandle
  { slot :: Ref (Maybe (Slot e a))
  , nextGen :: Ref Int
  }

-- | Allocate a fresh handle owned by `scope`. When the scope closes,
-- | the current occupant (if any) is interrupted.
make :: forall r e e' a. Scope -> RIO r e' (FiberHandle e a)
make scope = do
  slot <- liftEffect (Ref.new Nothing)
  nextGen <- liftEffect (Ref.new 0)
  addFinalizer scope do
    m <- Ref.read slot
    case m of
      Just s -> Internal.interruptFiber s.fiber
      Nothing -> pure unit
  pure (FiberHandle { slot, nextGen })

-- | Fork `action` and store the fiber in the handle. If the handle
-- | already held a fiber, that fiber is interrupted first. The
-- | returned fiber handle can be observed, joined, or interrupted
-- | manually; it also auto-clears from the slot on its own
-- | completion.
-- |
-- | The interrupt + fork + install runs in an uninterruptible region
-- | so an interrupt cannot land mid-way and leak the new fiber.
run
  :: forall r e a
   . FiberHandle e a
  -> RIO r e a
  -> RIO r e (Fiber e a)
run (FiberHandle { slot, nextGen }) action = uninterruptible do
  mPrev <- liftEffect (Ref.read slot)
  case mPrev of
    Just s -> liftEffect (Internal.interruptFiber s.fiber)
    Nothing -> pure unit
  fib <- fork action
  liftEffect do
    gen <- Ref.modify (_ + 1) nextGen
    Ref.write (Just { fiber: fib, gen }) slot
    -- Auto-clear when this fiber finishes, but only if it's still
    -- the current occupant. A later `run` would bump `gen`; the
    -- observer compares its captured gen with the live one.
    Internal.observeFiber fib \_ -> do
      cur <- Ref.read slot
      case cur of
        Just c | c.gen == gen -> Ref.write Nothing slot
        _ -> pure unit
  pure fib

-- | Read the current occupant. `Nothing` if the slot is empty (no
-- | fiber has run, or the last one already completed).
get
  :: forall r e e' a. FiberHandle e a -> RIO r e' (Maybe (Fiber e a))
get (FiberHandle { slot }) = liftEffect do
  m <- Ref.read slot
  pure (map _.fiber m)

-- | Interrupt the current occupant (if any) and clear the slot.
-- | Returns whether a fiber was actually interrupted.
clear :: forall r e e' a. FiberHandle e a -> RIO r e' Boolean
clear (FiberHandle { slot }) = liftEffect do
  m <- Ref.read slot
  case m of
    Just s -> do
      Internal.interruptFiber s.fiber
      Ref.write Nothing slot
      pure true
    Nothing -> pure false
