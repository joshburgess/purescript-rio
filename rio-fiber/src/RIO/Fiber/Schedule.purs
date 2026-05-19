-- | Retry and repeat policies as a small state machine.
-- |
-- | A `Schedule input output` consumes an input each step (a value
-- | for `repeat`, an error for `retry`) and decides whether to
-- | continue, how long to wait before the next attempt, and what
-- | output to expose. Schedules are immutable; each step yields the
-- | next schedule to consult.
-- |
-- | The MVP ships the standard library: `recurs`, `spaced`,
-- | `exponential`, `forever`, plus the obvious combinators
-- | `andThen`, `bothS`, `eitherS`. Apply them with `repeat` (drive a
-- | successful action) or `retry` (re-run a failing action).
module RIO.Fiber.Schedule
  ( Schedule(..)
  , Decision(..)
  , recurs
  , spaced
  , exponential
  , forever
  , once
  , bothS
  , andThen
  , mapOutput
  , repeat
  , repeatN
  , retry
  , retryN
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import RIO.Fiber.Core (RIO, catchAll, fail, liftEffect, sleep)

-- | A schedule: feed it an input and it returns the next decision.
newtype Schedule input output =
  Schedule (input -> Effect (Decision input output))

-- | Output of a single schedule step.
data Decision input output
  = Halt output
  | Step output Milliseconds (Schedule input output)

-- | Run at most `n` steps before halting. The output is the
-- | (1-indexed) attempt count.
recurs :: forall a. Int -> Schedule a Int
recurs maxIter = go 1
  where
  go n = Schedule \_ -> pure
    if n > maxIter then Halt (n - 1)
    else Step n (Milliseconds 0.0) (go (n + 1))

-- | A single step.
once :: forall a. Schedule a Int
once = recurs 1

-- | Step forever, waiting `delay` between attempts.
spaced :: forall a. Milliseconds -> Schedule a Int
spaced delay = go 1
  where
  go n = Schedule \_ -> pure (Step n delay (go (n + 1)))

-- | Step forever with no delay between attempts. Useful as a base
-- | for combinators that add their own bound (e.g. `forever && recurs n`).
forever :: forall a. Schedule a Int
forever = spaced (Milliseconds 0.0)

-- | Step forever with an exponentially-growing delay. The first
-- | delay is `base`; each subsequent delay is multiplied by `factor`.
exponential :: forall a. Milliseconds -> Number -> Schedule a Int
exponential (Milliseconds base) factor = go 1 base
  where
  go n d = Schedule \_ -> pure (Step n (Milliseconds d) (go (n + 1) (d * factor)))

-- | Intersection: both schedules must agree to continue. The
-- | combined delay is the maximum of the two.
bothS
  :: forall a b c
   . Schedule a b
  -> Schedule a c
  -> Schedule a (Tuple b c)
bothS (Schedule sa) (Schedule sb) = Schedule \input -> do
  da <- sa input
  db <- sb input
  pure case da, db of
    Halt b, Halt c -> Halt (Tuple b c)
    Halt b, Step c _ _ -> Halt (Tuple b c)
    Step b _ _, Halt c -> Halt (Tuple b c)
    Step b delA sa', Step c delB sb' ->
      Step (Tuple b c) (maxDelay delA delB) (bothS sa' sb')

-- | Run the first schedule until it halts, then switch to the second.
andThen
  :: forall a b c
   . Schedule a b
  -> Schedule a c
  -> Schedule a (Either b c)
andThen (Schedule sa) sb = Schedule \input -> do
  d <- sa input
  case d of
    Halt _ -> do
      let Schedule step = mapOutput Right sb
      step input
    Step b delay sa' ->
      pure (Step (Left b) delay (andThen sa' sb))

-- | Map the output of a schedule. Useful for tagging.
mapOutput :: forall a b c. (b -> c) -> Schedule a b -> Schedule a c
mapOutput f (Schedule step) = Schedule \input -> do
  d <- step input
  pure case d of
    Halt b -> Halt (f b)
    Step b delay next -> Step (f b) delay (mapOutput f next)

-- | Run `action` once, then repeatedly while `schedule` decides to
-- | continue. Returns the schedule's last output.
repeat
  :: forall r e a b
   . Schedule a b
  -> RIO r e a
  -> RIO r e b
repeat schedule action = do
  a <- action
  loop schedule a
  where
  loop (Schedule step) a = do
    d <- liftEffect (step a)
    case d of
      Halt b -> pure b
      Step _ delay next -> do
        sleep delay
        a' <- action
        loop next a'

-- | `repeat (recurs n)` — convenience for "run the action `n + 1`
-- | times total (initial run plus `n` repeats)".
repeatN :: forall r e a. Int -> RIO r e a -> RIO r e Int
repeatN n = repeat (recurs n)

-- | Retry `action` while it raises a typed failure, consulting the
-- | schedule with each failure. If the schedule halts the action's
-- | last failure is re-raised.
retry
  :: forall r e a b
   . Schedule (Variant e) b
  -> RIO r e a
  -> RIO r e a
retry schedule action = go schedule
  where
  go (Schedule step) = catchAll
    ( \v -> do
        d <- liftEffect (step v)
        case d of
          Halt _ -> fail v
          Step _ delay next -> do
            sleep delay
            go next
    )
    action

-- | `retry (recurs n)` — up to `n` retries.
retryN :: forall r e a. Int -> RIO r e a -> RIO r e a
retryN n = retry (recurs n)

maxDelay :: Milliseconds -> Milliseconds -> Milliseconds
maxDelay (Milliseconds a) (Milliseconds b) = Milliseconds (max a b)
