-- | `Schedule r i o`: a description of when (and how often) to fire,
-- | suitable for driving `retry` (retry a failing action under a
-- | policy) and `repeat` (run a successful action on a cadence).
-- |
-- | The encoding is the "next schedule" style:
-- |
-- | ```purescript
-- | newtype Schedule r i o = Schedule (i -> RIO r () (Step r i o))
-- |
-- | data Step r i o
-- |   = Done
-- |   | Continue o Milliseconds (Schedule r i o)
-- | ```
-- |
-- | A schedule's step looks at the input (the action's last result
-- | for `repeat`, or its typed failure for `retry`) and either says
-- | "stop" (`Done`) or "continue with this output, after this delay,
-- | using this next schedule". The runner threads the next schedule
-- | through iterations, so policies are pure values composed by
-- | combinators rather than mutable cursors.
-- |
-- | Schedules cannot themselves fail with a typed error; the error
-- | row is fixed to `()`. They can still read services from `r`
-- | (the wall clock, a config value) when their decisions need that.
-- |
-- | The runners (`repeat`, `retry`, `retryOrElse`) sleep via the
-- | `Clock` service rather than `Aff.delay` directly, so test suites
-- | using `RIO.Test.Clock` can drive scheduled programs in virtual
-- | time.
module RIO.Schedule
  ( Schedule
  , Step(..)
  , andThen
  , exponential
  , forever
  , intersect
  , jittered
  , mapSchedule
  , once
  , recurs
  , repeat
  , retry
  , retryOrElse
  , spaced
  , step
  , whileInput
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Random as Random
import Unsafe.Coerce (unsafeCoerce)

import RIO.Clock (Clock, sleep)
import RIO.Internal (RIO(..), unRIO)

-- | A scheduling policy: given an input `i`, fire `Step r i o`.
-- |
-- | Construct one with `recurs`, `spaced`, `exponential`, `forever`,
-- | or `once`; combine with `andThen`, `intersect`, `whileInput`,
-- | `jittered`, or `mapSchedule`; drive with `repeat`, `retry`, or
-- | `retryOrElse`.
newtype Schedule :: Row Type -> Type -> Type -> Type
newtype Schedule r i o = Schedule (i -> RIO r () (Step r i o))

-- | One step of a schedule. `Done` ends the run; `Continue o ms
-- | next` produces an output `o`, asks the runner to sleep `ms`,
-- | and hands back the next schedule for the iteration after that.
data Step :: Row Type -> Type -> Type -> Type
data Step r i o
  = Done
  | Continue o Milliseconds (Schedule r i o)

-- | Fire up to `n` times, regardless of input. Output is the
-- | one-based iteration count. `recurs 3` paired with `repeat`
-- | runs the action 4 times (one initial run plus 3 repeats).
-- |
-- | ```purescript
-- | -- run the action four times in a row, no delay between
-- | _ <- repeat (recurs 3) action
-- | ```
recurs :: forall r i. Int -> Schedule r i Int
recurs n = go 0
  where
  go k = Schedule \_ -> RIO \_ ->
    if k >= n then pure (Right Done)
    else
      let next = k + 1
      in pure (Right (Continue next (Milliseconds 0.0) (go next)))

-- | Fixed delay between firings, forever.
-- |
-- | ```purescript
-- | -- poll the endpoint every 30 seconds
-- | _ <- repeat (spaced (Milliseconds 30000.0)) (poll endpoint)
-- | ```
spaced :: forall r i. Milliseconds -> Schedule r i Int
spaced ms = go 0
  where
  go k = Schedule \_ -> RIO \_ ->
    let next = k + 1
    in pure (Right (Continue next ms (go next)))

-- | Exponential backoff: delay grows by `factor` each step,
-- | starting from `base`. Output is the current delay (useful for
-- | logging or for `jittered`).
-- |
-- | ```purescript
-- | -- 100ms, 200ms, 400ms, 800ms, ...
-- | exponential (Milliseconds 100.0) 2.0
-- | ```
exponential :: forall r i. Milliseconds -> Number -> Schedule r i Milliseconds
exponential (Milliseconds base) factor = go base
  where
  go ms = Schedule \_ -> RIO \_ ->
    let delay = Milliseconds ms
    in pure (Right (Continue delay delay (go (ms * factor))))

-- | Never stops; output is the iteration count. Equivalent to
-- | `spaced (Milliseconds 0.0)`.
forever :: forall r i. Schedule r i Int
forever = spaced (Milliseconds 0.0)

-- | Fire exactly once (in `repeat` terms: the action runs twice,
-- | once initially and once on the single recurrence).
once :: forall r i. Schedule r i Unit
once = mapSchedule (\_ -> unit) (recurs 1)

-- | Run `sa` to completion, then `sb`. Outputs are tagged: while
-- | `sa` is producing, the output is `Left`; once `sa` is `Done`
-- | and `sb` takes over, the output is `Right`.
-- |
-- | ```purescript
-- | -- 3 fast retries, then back off forever
-- | andThen (recurs 3) (exponential (Milliseconds 100.0) 2.0)
-- | ```
andThen
  :: forall r i o o'
   . Schedule r i o
  -> Schedule r i o'
  -> Schedule r i (Either o o')
andThen sa sb = Schedule \i -> RIO \env -> do
  let Schedule fa = sa
  res <- unRIO (fa i) env
  case res of
    Left v -> Variant.case_ v
    Right Done -> stepRight sb i env
    Right (Continue o d next) ->
      pure (Right (Continue (Left o) d (andThen next sb)))
  where
  stepRight (Schedule fb) i env = do
    res <- unRIO (fb i) env
    case res of
      Left v -> Variant.case_ v
      Right Done -> pure (Right Done)
      Right (Continue o d next) ->
        pure (Right (Continue (Right o) d (mapSchedule Right next)))

-- | Both schedules must continue: as soon as either says `Done`,
-- | the intersection says `Done`. The delay is the larger of the
-- | two (so both can keep up), and the output is the tuple of
-- | per-schedule outputs.
-- |
-- | ```purescript
-- | -- at most 5 retries, with exponential backoff between them
-- | intersect (recurs 5) (exponential (Milliseconds 100.0) 2.0)
-- | ```
intersect
  :: forall r i o o'
   . Schedule r i o
  -> Schedule r i o'
  -> Schedule r i (Tuple o o')
intersect sa sb = Schedule \i -> RIO \env -> do
  let
    Schedule fa = sa
    Schedule fb = sb
  resA <- unRIO (fa i) env
  resB <- unRIO (fb i) env
  case resA, resB of
    Left v, _ -> Variant.case_ v
    _, Left v -> Variant.case_ v
    Right (Continue oa da nextA), Right (Continue ob db nextB) ->
      pure
        ( Right
            ( Continue (Tuple oa ob) (maxMs da db) (intersect nextA nextB)
            )
        )
    _, _ -> pure (Right Done)
  where
  maxMs (Milliseconds a) (Milliseconds b) =
    Milliseconds (if a >= b then a else b)

-- | Continue only while the input satisfies the predicate. The
-- | underlying schedule's decision is consulted only when the
-- | predicate holds; if it doesn't, the result is `Done` immediately.
-- |
-- | Pair with `retry` to bail out on a specific failure tag without
-- | exhausting the retry budget.
whileInput
  :: forall r i o
   . (i -> Boolean)
  -> Schedule r i o
  -> Schedule r i o
whileInput pred (Schedule s) = Schedule \i -> RIO \env ->
  if not (pred i) then pure (Right Done)
  else do
    res <- unRIO (s i) env
    case res of
      Left v -> Variant.case_ v
      Right Done -> pure (Right Done)
      Right (Continue o d next) ->
        pure (Right (Continue o d (whileInput pred next)))

-- | Multiply every delay by a uniform random factor in `[lo, hi]`,
-- | sampled per step. Use a tight band like `0.8`/`1.2` to soften a
-- | thundering herd without changing the shape of the policy.
-- |
-- | ```purescript
-- | -- exponential backoff with +/- 20% jitter
-- | jittered 0.8 1.2 (exponential (Milliseconds 100.0) 2.0)
-- | ```
jittered
  :: forall r i o
   . Number
  -> Number
  -> Schedule r i o
  -> Schedule r i o
jittered lo hi (Schedule s) = Schedule \i -> RIO \env -> do
  res <- unRIO (s i) env
  case res of
    Left v -> Variant.case_ v
    Right Done -> pure (Right Done)
    Right (Continue o (Milliseconds ms) next) -> do
      r <- unRIO randomNumber env
      case r of
        Left v -> Variant.case_ v
        Right factorBase -> do
          let factor = lo + (hi - lo) * factorBase
          pure
            ( Right
                ( Continue o (Milliseconds (ms * factor)) (jittered lo hi next)
                )
            )
  where
  randomNumber :: RIO r () Number
  randomNumber = RIO \_ -> do
    n <- liftEffect Random.random
    pure (Right n)

-- | Transform a schedule's output. The cadence (number of steps and
-- | per-step delay) is preserved; only the output side changes.
mapSchedule
  :: forall r i o o'
   . (o -> o')
  -> Schedule r i o
  -> Schedule r i o'
mapSchedule f (Schedule s) = Schedule \i -> RIO \env -> do
  res <- unRIO (s i) env
  case res of
    Left v -> Variant.case_ v
    Right Done -> pure (Right Done)
    Right (Continue o d next) ->
      pure (Right (Continue (f o) d (mapSchedule f next)))

-- | Run `action`, then repeat under `schedule`. The schedule sees
-- | each successful result as input; while it says `Continue`, the
-- | runner sleeps the requested delay and runs the action again.
-- | Returns the action's last value.
-- |
-- | Stops on the first typed failure (the failure surfaces on the
-- | parent's row unchanged); `retry` is the dual that keeps going
-- | on failure.
-- |
-- | Sleeps via the `Clock` service: a virtual-time test clock can
-- | drive a `repeat` deterministically.
-- |
-- | ```purescript
-- | -- poll the endpoint every 30s, forever
-- | _ <- repeat (spaced (Milliseconds 30000.0)) (poll endpoint)
-- | ```
repeat
  :: forall r e a o
   . Schedule r a o
  -> RIO r e a
  -> RIO (clock :: Clock | r) e a
repeat sched action = loop sched
  where
  loop (Schedule s) = RIO \env -> do
    let envInner = (unsafeCoerce env :: Record r)
    res <- unRIO action envInner
    case res of
      Left v -> pure (Left v)
      Right a -> do
        stepRes <- unRIO (s a) envInner
        case stepRes of
          Left v -> Variant.case_ v
          Right Done -> pure (Right a)
          Right (Continue _ ms next) -> do
            _ <- unRIO (sleepIfPositive ms) env
            unRIO (loop next) env

-- | Run `action`; on typed failure, consult `schedule` with the
-- | failure as input. While the schedule says `Continue`, sleep
-- | and retry. When the schedule says `Done`, surface the most
-- | recent failure on the parent's row.
-- |
-- | Defects (from `die` or any uncaught `Aff` exception) skip retry
-- | and propagate immediately; sandbox the action if you want a
-- | defect to feed back into the schedule.
-- |
-- | ```purescript
-- | -- retry up to 5 times with exponential backoff
-- | retry
-- |   (intersect (recurs 5) (exponential (Milliseconds 100.0) 2.0))
-- |   (fetch url)
-- | ```
retry
  :: forall r e a o
   . Schedule r (Variant e) o
  -> RIO r e a
  -> RIO (clock :: Clock | r) e a
retry sched action = loop sched
  where
  loop (Schedule s) = RIO \env -> do
    let envInner = (unsafeCoerce env :: Record r)
    res <- unRIO action envInner
    case res of
      Right a -> pure (Right a)
      Left v -> do
        stepRes <- unRIO (s v) envInner
        case stepRes of
          Left v' -> Variant.case_ v'
          Right Done -> pure (Left v)
          Right (Continue _ ms next) -> do
            _ <- unRIO (sleepIfPositive ms) env
            unRIO (loop next) env

-- | Like `retry`, but on exhaustion the fallback runs with the
-- | final failure. The fallback's error row replaces the action's,
-- | so a recovered run can change the surfaced error shape.
-- |
-- | If the schedule's first step is `Done` (no retry allowed), the
-- | fallback runs immediately on the first failure.
retryOrElse
  :: forall r e e' a o
   . Schedule r (Variant e) o
  -> RIO r e a
  -> (Variant e -> RIO r e' a)
  -> RIO (clock :: Clock | r) e' a
retryOrElse sched action fallback = loop sched
  where
  loop (Schedule s) = RIO \env -> do
    let envInner = (unsafeCoerce env :: Record r)
    res <- unRIO action envInner
    case res of
      Right a -> pure (Right a)
      Left v -> do
        stepRes <- unRIO (s v) envInner
        case stepRes of
          Left v' -> Variant.case_ v'
          Right Done -> unRIO (fallback v) envInner
          Right (Continue _ ms next) -> do
            _ <- unRIO (sleepIfPositive ms) env
            unRIO (loop next) env

-- | Apply a schedule to one input, returning its next `Step`.
-- |
-- | Most code uses `repeat` / `retry` / `retryOrElse` to drive a
-- | schedule. `step` is for the rare case where you need to drive a
-- | schedule by hand (for example, in a test that samples a
-- | schedule's per-step delay distribution).
step
  :: forall r i o
   . Schedule r i o
  -> i
  -> RIO r () (Step r i o)
step (Schedule f) i = f i

-- | Internal: sleep only when the requested delay is positive.
-- | Avoids an unnecessary `delay 0` round-trip.
sleepIfPositive :: forall r. Milliseconds -> RIO (clock :: Clock | r) () Unit
sleepIfPositive ms =
  if unwrap ms > 0.0 then sleep ms
  else RIO \_ -> pure (Right unit)
