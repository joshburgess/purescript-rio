-- | A `Reloadable a` is a slot whose value is re-acquired on a
-- | `Schedule`.
-- |
-- | This is the right shape for values that need to refresh
-- | periodically without bothering their consumers: rotating
-- | credentials, hot-reloaded config, signed URLs with finite
-- | lifetime, etc. The consumer calls `get` and always sees the
-- | latest successfully-acquired value.
-- |
-- | A background fiber bound to a caller-supplied `Scope` drives the
-- | re-acquire loop. Failed acquires are absorbed silently so a
-- | single bad poll does not lose the previously-good value; the
-- | next scheduled tick (or a manual `reload`) gets another chance.
-- | If you want failure to propagate, wrap your acquire with
-- | `Schedule.retry` or a typed `catchAll` before handing it in.
module RIO.Fiber.Reloadable
  ( Reloadable
  , make
  , get
  , reload
  ) where

import Prelude

import Data.Either (Either(..))
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Schedule (Schedule)
import RIO.Fiber.Schedule as Sch
import RIO.Fiber.Scope (Scope)
import RIO.Fiber.Scope as Scope
import RIO.Fiber.STM (TVar)
import RIO.Fiber.STM as STM

-- | A reloadable slot. Carries the typed env and error rows of the
-- | acquire it was built with so `reload` can re-run that same
-- | effect later.
newtype Reloadable r e a = Reloadable
  { current :: TVar a
  , acquire :: RIO r e a
  }

-- | Build a reloadable slot. `acquire` runs immediately to seed the
-- | initial value; a background fiber is then forked into `scope`
-- | and follows `schedule` to re-acquire periodically. Failed
-- | re-acquires are absorbed (the slot keeps its last good value).
-- |
-- | The fiber is interrupted when `scope` closes; the slot remains
-- | readable after that but no further reloads will fire.
make
  :: forall r e a b
   . Scope
  -> Schedule Unit b
  -> RIO r e a
  -> RIO r e (Reloadable r e a)
make scope schedule acquire = do
  seed <- acquire
  current <- STM.atomically (STM.newTVarSTM seed)
  _ <- Scope.forkScoped scope (loop current)
  pure (Reloadable { current, acquire })
  where
  loop current = do
    _ <- Sch.repeat schedule do
      cause <- F.causeOf acquire
      case cause of
        Right a -> STM.atomically (STM.writeTVar current a)
        Left _ -> pure unit
    pure unit

-- | Read the current value. Always observes the latest successful
-- | acquire (initial or scheduled).
get :: forall r e a. Reloadable r e a -> RIO r e a
get (Reloadable { current }) =
  STM.atomically (STM.readTVar current)

-- | Run an immediate re-acquire, in addition to the schedule. If
-- | the acquire fails, the slot keeps its prior value and the
-- | failure is re-raised to the caller (unlike the scheduled loop,
-- | which swallows failures).
reload :: forall r e a. Reloadable r e a -> RIO r e Unit
reload (Reloadable { current, acquire }) = do
  a <- acquire
  STM.atomically (STM.writeTVar current a)
