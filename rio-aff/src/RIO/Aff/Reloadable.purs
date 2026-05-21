-- | A `Reloadable e a` is a slot whose value is re-acquired on a
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
-- |
-- | The handle is row-polymorphic at use sites: `make` bakes its
-- | build-time `Record r` into the acquire closure so the same
-- | `Reloadable` can be passed across `scoped` blocks that extend
-- | the row with services like `scope :: Scope`.
module RIO.Aff.Reloadable
  ( Reloadable
  , make
  , get
  , reload
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import RIO.Aff.Clock (Clock, sleep)
import RIO.Aff.Concurrency (forkScoped)
import RIO.Aff.Error (rethrow)
import RIO.Aff.Internal (RIO, mkRIO, unRIO, unsafeUnRIO)
import RIO.Aff.Resource (Scope)
import RIO.Aff.Schedule (Schedule, Step(..), step)
import RIO.Aff.STM (TVar)
import RIO.Aff.STM as STM
import Unsafe.Coerce (unsafeCoerce)

-- | A reloadable slot. Carries only the typed-error row of its
-- | acquire (the env row is hidden behind a closure baked at
-- | `make` time).
newtype Reloadable e a = Reloadable
  { current :: TVar a
  , acquireAff :: Aff (Either (Variant e) a)
  }

-- | Build a reloadable slot. `acquire` runs immediately to seed the
-- | initial value; a background fiber is then forked into `scope`
-- | and follows `schedule` to re-acquire periodically. Failed
-- | re-acquires are absorbed (the slot keeps its last good value).
-- |
-- | The fiber is interrupted when `scope` closes; the slot remains
-- | readable after that but no further reloads will fire.
-- |
-- | The schedule is driven sleep-first: the seed acquire (run at
-- | `make` time) counts as the initial firing, and the next firing
-- | only happens after `schedule` says `Continue` and the requested
-- | delay has elapsed. `recurs 0` therefore disables scheduled
-- | reloads entirely, leaving only the seed and any manual `reload`
-- | calls.
-- |
-- | `Clock` lives in the result row because the schedule loop
-- | sleeps via the `Clock` service so virtual-time test clocks can
-- | drive the reload cadence deterministically.
make
  :: forall r e a b
   . Scope
  -> Schedule r Unit b
  -> RIO r e a
  -> RIO (clock :: Clock | r) e (Reloadable e a)
make scope schedule acquire = mkRIO \env -> do
  let
    envInner :: Record r
    envInner = unsafeCoerce env

    acquireAff :: Aff (Either (Variant e) a)
    acquireAff = unRIO acquire envInner
  seed <- unsafeUnRIO acquire envInner
  current <- unsafeUnRIO (STM.atomically (STM.newTVar seed)) envInner
  let
    -- Sleep-first schedule loop. The seed acquire above is the
    -- initial firing; from here on we only re-acquire after the
    -- schedule yields `Continue` and we have slept the requested
    -- delay.
    runLoop :: Schedule r Unit b -> Aff Unit
    runLoop sched = do
      stepRes <- unsafeUnRIO (step sched unit) envInner
      case stepRes of
        Done -> pure unit
        Continue _ ms next -> do
          sleepIfPositive ms env
          outcome <- acquireAff
          case outcome of
            Right a ->
              unsafeUnRIO (STM.atomically (STM.writeTVar current a)) envInner
            Left _ -> pure unit
          runLoop next

    loop :: RIO (clock :: Clock | r) e Unit
    loop = mkRIO \_ -> runLoop schedule
  _ <- unsafeUnRIO (forkScoped scope loop) env
  pure (Reloadable { current, acquireAff })
  where
  sleepIfPositive :: Milliseconds -> Record (clock :: Clock | r) -> Aff Unit
  sleepIfPositive ms env =
    if unwrap ms > 0.0 then unsafeUnRIO (sleep ms) env
    else pure unit

-- | Read the current value. Always observes the latest successful
-- | acquire (initial or scheduled).
get :: forall r e' e a. Reloadable e a -> RIO r e' a
get (Reloadable { current }) =
  STM.atomically (STM.readTVar current)

-- | Run an immediate re-acquire, in addition to the schedule. If
-- | the acquire fails, the slot keeps its prior value and the
-- | failure is re-raised to the caller (unlike the scheduled loop,
-- | which swallows failures).
reload :: forall r e a. Reloadable e a -> RIO r e Unit
reload (Reloadable { current, acquireAff }) = do
  result <- mkRIO \_ -> acquireAff
  case result of
    Left v -> rethrow v
    Right a -> STM.atomically (STM.writeTVar current a)
