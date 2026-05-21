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
  , fibonacci
  , fixed
  , forever
  , jittered
  , once
  , bothS
  , intersect
  , andThen
  , compose
  , mapInput
  , mapOutput
  , dimap
  , passthrough
  , elapsed
  , delays
  , delayed
  , collectAll
  , asUnit
  , mapDelay
  , modifyDelayM
  , addDelayM
  , whileInput
  , whileOutput
  , untilInput
  , untilOutput
  , recursWhile
  , recursUntil
  , repetitions
  , tapOutput
  , windowed
  , fromFunction
  , unfold
  , repeat
  , repeatN
  , retry
  , retryN
  , retryOrElse
  , eventually
  ) where

import Prelude hiding (compose)

import Data.Array (snoc) as Array
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Now (now) as Now
import Effect.Random (random)
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

-- | Step forever with a delay following the Fibonacci sequence,
-- | seeded with `unit`. The first delay is `unit`, the second is
-- | `unit`, then each subsequent delay is the sum of the previous
-- | two. Useful as a gentler alternative to `exponential`.
fibonacci :: forall a. Milliseconds -> Schedule a Int
fibonacci (Milliseconds unitMs) = go 1 unitMs unitMs
  where
  go n a b = Schedule \_ -> pure
    (Step n (Milliseconds a) (go (n + 1) b (a + b)))

-- | Step forever at a fixed interval. Unlike `spaced`, the delay
-- | between iteration *starts* is held constant: the schedule
-- | tracks wall time elapsed since the last step and shortens the
-- | next sleep accordingly. (In this MVP we approximate it as
-- | `spaced delay`; tightening to true fixed-rate scheduling is a
-- | follow-up once we expose monotonic time.)
fixed :: forall a. Milliseconds -> Schedule a Int
fixed = spaced

-- | Add jitter to every delay produced by the inner schedule. The
-- | new delay is uniformly distributed in `[delay * lo, delay * hi]`.
-- | Useful with `exponential` to avoid thundering-herd retries.
jittered
  :: forall a b. Number -> Number -> Schedule a b -> Schedule a b
jittered lo hi (Schedule step) = Schedule \input -> do
  d <- step input
  case d of
    Halt b -> pure (Halt b)
    Step b (Milliseconds ms) next -> do
      r <- random
      let
        factor = lo + (hi - lo) * r
      pure (Step b (Milliseconds (ms * factor)) (jittered lo hi next))

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

-- | Alias for `bothS` matching rio-aff's `intersect` naming. The
-- | combined schedule continues only while both inputs continue;
-- | per-step delay is the maximum of the two. Provided for symmetry
-- | with rio-aff.
intersect
  :: forall a b c
   . Schedule a b
  -> Schedule a c
  -> Schedule a (Tuple b c)
intersect = bothS

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

-- | Pre-process the input before feeding it to the inner schedule.
-- | Mirror of `mapOutput`. Useful for adapting a schedule that wants
-- | one input shape (e.g. a typed failure variant) to a callsite
-- | that has another (e.g. a wrapped error).
mapInput :: forall a a' b. (a' -> a) -> Schedule a b -> Schedule a' b
mapInput f (Schedule step) = Schedule \input -> do
  d <- step (f input)
  pure case d of
    Halt b -> Halt b
    Step b delay next -> Step b delay (mapInput f next)

-- | Transform both the input and the output of a schedule in a
-- | single step. `dimap pre post s` is `mapOutput post (mapInput
-- | pre s)`; it exists as a named entry point so the Profunctor
-- | shape of schedules is discoverable.
dimap
  :: forall a a' b b'
   . (a' -> a)
  -> (b -> b')
  -> Schedule a b
  -> Schedule a' b'
dimap pre post (Schedule step) = Schedule \input -> do
  d <- step (pre input)
  pure case d of
    Halt b -> Halt (post b)
    Step b delay next -> Step (post b) delay (dimap pre post next)

-- | Sequential composition: thread the output of the first schedule
-- | into the input of the second. Both schedules must continue for
-- | the composition to continue; the combined delay is the maximum
-- | of the two so neither schedule's pacing is violated.
compose
  :: forall a b c
   . Schedule a b
  -> Schedule b c
  -> Schedule a c
compose (Schedule sa) (Schedule sb) = Schedule \input -> do
  da <- sa input
  case da of
    Halt b -> do
      db <- sb b
      pure case db of
        Halt c -> Halt c
        Step c _ _ -> Halt c
    Step b delA sa' -> do
      db <- sb b
      pure case db of
        Halt c -> Halt c
        Step c delB sb' -> Step c (maxDelay delA delB) (compose sa' sb')

-- | The identity schedule: recurs forever, echoing the input as the
-- | output with zero delay between recurrences. Useful as the left
-- | argument to `compose` to inspect or tag inputs while delegating
-- | pacing to another schedule.
passthrough :: forall a. Schedule a a
passthrough = Schedule \input ->
  pure (Step input (Milliseconds 0.0) passthrough)

-- | Recurs forever, outputting the wall-clock milliseconds elapsed
-- | since the first step. The first emitted output is `0.0`; each
-- | subsequent output is the delta from the schedule's start time
-- | (captured on the first step). Emits no delay of its own; use
-- | with `compose` against a paced schedule to expose elapsed
-- | timing.
elapsed :: forall a. Schedule a Milliseconds
elapsed = Schedule \_ -> do
  start <- nowMs
  pure (Step (Milliseconds 0.0) (Milliseconds 0.0) (go start))
  where
  go startMs = Schedule \_ -> do
    nowMs' <- nowMs
    pure
      ( Step
          (Milliseconds (nowMs' - startMs))
          (Milliseconds 0.0)
          (go startMs)
      )

-- | Replace the inner schedule's output with the delay it emitted.
-- | The inner schedule's pacing is preserved (each step still sleeps
-- | for the same amount); only the output changes. Useful for
-- | observing the timing sequence of a retry/backoff policy.
delays :: forall a b. Schedule a b -> Schedule a Milliseconds
delays (Schedule step) = Schedule \input -> do
  d <- step input
  pure case d of
    Halt _ -> Halt (Milliseconds 0.0)
    Step _ delay next -> Step delay delay (delays next)

-- | Add a one-time delay to the first emitted step. Subsequent
-- | steps run on the underlying schedule's normal cadence with no
-- | adjustment. Useful to phase-offset a periodic schedule (so two
-- | pollers don't tick in lockstep) or give a warm-up window before
-- | the first action fires.
delayed :: forall a b. Milliseconds -> Schedule a b -> Schedule a b
delayed (Milliseconds offset) (Schedule step) = Schedule \input -> do
  d <- step input
  pure case d of
    Halt b -> Halt b
    Step b (Milliseconds delay) next ->
      Step b (Milliseconds (delay + offset)) next

-- | Each step emits the array of every output the schedule has
-- | produced so far (inclusive of the current one). Cadence and
-- | termination are unchanged; only the output type widens.
-- |
-- | The accumulator grows without bound, so pair with `recurs`,
-- | `untilOutput`, or similar to stop early on unbounded schedules.
collectAll :: forall a b. Schedule a b -> Schedule a (Array b)
collectAll = go []
  where
  go acc (Schedule step) = Schedule \input -> do
    d <- step input
    pure case d of
      Halt b -> Halt (Array.snoc acc b)
      Step b delay next ->
        let acc' = Array.snoc acc b
        in Step acc' delay (go acc' next)

-- | Discard the inner schedule's output type by replacing every
-- | emission with `unit`. Cadence and termination are preserved;
-- | only the output type changes. Mirrors ZIO `Schedule.unit`
-- | (renamed to avoid shadowing `unit :: Unit`).
asUnit :: forall a b. Schedule a b -> Schedule a Unit
asUnit = mapOutput (\_ -> unit)

-- | Transform every per-step delay through a pure function. The
-- | decision shape and output are preserved; only the sleep
-- | duration changes.
mapDelay
  :: forall a b
   . (Milliseconds -> Milliseconds)
  -> Schedule a b
  -> Schedule a b
mapDelay f (Schedule step) = Schedule \input -> do
  d <- step input
  pure case d of
    Halt b -> Halt b
    Step b delay next -> Step b (f delay) (mapDelay f next)

-- | Effectful sibling of `mapDelay`. The transform runs in `Effect`
-- | so it can read refs, hit a random source, or sample any other
-- | Effect-level state.
modifyDelayM
  :: forall a b
   . (Milliseconds -> Effect Milliseconds)
  -> Schedule a b
  -> Schedule a b
modifyDelayM f (Schedule step) = Schedule \input -> do
  d <- step input
  case d of
    Halt b -> pure (Halt b)
    Step b delay next -> do
      delay' <- f delay
      pure (Step b delay' (modifyDelayM f next))

-- | Add an effectful delta to each step's delay, computed from the
-- | step's output. The decision and underlying delay are preserved;
-- | only an additive adjustment is layered on top.
addDelayM
  :: forall a b
   . (b -> Effect Milliseconds)
  -> Schedule a b
  -> Schedule a b
addDelayM f (Schedule step) = Schedule \input -> do
  d <- step input
  case d of
    Halt b -> pure (Halt b)
    Step b (Milliseconds delay) next -> do
      Milliseconds add <- f b
      pure (Step b (Milliseconds (delay + add)) (addDelayM f next))

-- | Continue only while the input satisfies `p`. An input that
-- | fails the predicate halts immediately; the schedule's last
-- | step output is reused as the halt value.
whileInput :: forall a b. (a -> Boolean) -> Schedule a b -> Schedule a b
whileInput p sched = go sched Nothing
  where
  go (Schedule step) lastB = Schedule \input -> do
    d <- step input
    case d of
      Halt b -> pure (Halt b)
      Step b delay next
        | p input -> pure (Step b delay (go next (Just b)))
        | otherwise -> case lastB of
            Just prev -> pure (Halt prev)
            Nothing -> pure (Halt b)

-- | Continue only while the inner schedule's output satisfies `p`.
whileOutput :: forall a b. (b -> Boolean) -> Schedule a b -> Schedule a b
whileOutput p (Schedule step) = Schedule \input -> do
  d <- step input
  pure case d of
    Halt b -> Halt b
    Step b delay next
      | p b -> Step b delay (whileOutput p next)
      | otherwise -> Halt b

-- | Continue until the input satisfies `p` (negation of `whileInput`).
untilInput :: forall a b. (a -> Boolean) -> Schedule a b -> Schedule a b
untilInput p = whileInput (not <<< p)

-- | Continue until the inner schedule's output satisfies `p`.
untilOutput :: forall a b. (b -> Boolean) -> Schedule a b -> Schedule a b
untilOutput p = whileOutput (not <<< p)

-- | `forever` but only while the input matches the predicate.
-- | Equivalent to `whileInput pred forever`, exposed as a named
-- | constructor for discoverability when reaching for a
-- | "retry while this condition holds" policy.
recursWhile :: forall a. (a -> Boolean) -> Schedule a Int
recursWhile pred = whileInput pred forever

-- | `forever` but only until the input matches the predicate.
-- | Equivalent to `untilInput pred forever`.
recursUntil :: forall a. (a -> Boolean) -> Schedule a Int
recursUntil pred = untilInput pred forever

-- | Replace the schedule's output with the (1-indexed) step count.
-- | Cadence and termination are preserved.
repetitions :: forall a b. Schedule a b -> Schedule a Int
repetitions = go 1
  where
  go n (Schedule s) = Schedule \i -> do
    d <- s i
    case d of
      Halt _ -> pure (Halt n)
      Step _ delay next -> pure (Step n delay (go (n + 1) next))

-- | Run an `Effect` side-effect with each emitted output, passing
-- | the output through unchanged. Cadence is unaffected.
-- |
-- | The handler is `Effect`-typed (rather than `RIO`) because
-- | rio-fiber's `Schedule` step is itself `Effect`-typed; reach for
-- | a higher-level wrapper if you need full `RIO` semantics inside
-- | the tap.
tapOutput
  :: forall a b
   . (b -> Effect Unit)
  -> Schedule a b
  -> Schedule a b
tapOutput f (Schedule s) = Schedule \i -> do
  d <- s i
  case d of
    Halt b -> do
      f b
      pure (Halt b)
    Step b delay next -> do
      f b
      pure (Step b delay (tapOutput f next))

-- | Cap every per-step delay at `cap`. Delays already at or below
-- | the cap pass through unchanged. The decision (number of steps,
-- | output values, termination) is preserved; only the sleep time
-- | is clamped.
-- |
-- | Useful for ensuring a runaway `exponential` or `fibonacci`
-- | backoff never sleeps for an unreasonable amount of time. Pair
-- | with `jittered` to keep the cap soft.
windowed
  :: forall a b
   . Milliseconds
  -> Schedule a b
  -> Schedule a b
windowed (Milliseconds cap) = mapDelay
  (\(Milliseconds ms) -> Milliseconds (if ms > cap then cap else ms))

-- | Build a stateless schedule from a pure function on the input.
-- | The schedule never stops; each step emits the function's
-- | result and the requested delay.
fromFunction
  :: forall a b
   . (a -> { output :: b, delay :: Milliseconds })
  -> Schedule a b
fromFunction f = Schedule \i ->
  let
    r = f i
  in
    pure (Step r.output r.delay (fromFunction f))

-- | Build a schedule from a state-threading function. Starts at
-- | `seed` and at each step calls `f state input`, which returns
-- | the per-step output, delay, and the next state. The schedule
-- | never stops.
unfold
  :: forall s a b
   . s
  -> (s -> a -> { output :: b, delay :: Milliseconds, state :: s })
  -> Schedule a b
unfold seed f = go seed
  where
  go s = Schedule \i ->
    let
      r = f s i
    in
      pure (Step r.output r.delay (go r.state))

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

-- | Like `retry`, but when the schedule halts (gives up), run a
-- | fallback action instead of re-raising the action's last typed
-- | failure. The fallback's error row replaces the action's, so a
-- | recovered run can change the surfaced error shape.
-- |
-- | If the schedule's first step is `Halt` (no retry allowed), the
-- | fallback runs immediately on the first failure.
retryOrElse
  :: forall r e e' a b
   . Schedule (Variant e) b
  -> RIO r e a
  -> (Variant e -> RIO r e' a)
  -> RIO r e' a
retryOrElse schedule action fallback = go schedule
  where
  go (Schedule step) = catchAll
    ( \v -> do
        d <- liftEffect (step v)
        case d of
          Halt _ -> fallback v
          Step _ delay next -> do
            sleep delay
            go next
    )
    action

-- | Run `action`; on any typed failure, immediately try again.
-- | Repeats forever until a success, so the result type loses the
-- | error row entirely. Defects (uncaught exceptions, `die`) skip
-- | the retry loop and propagate.
-- |
-- | No clock service is required because there is no inter-attempt
-- | delay. For backed-off retries with a cap, reach for `retry`
-- | with an explicit schedule instead.
eventually :: forall r e e' a. RIO r e a -> RIO r e' a
eventually action = go
  where
  go = catchAll (\_ -> go) action

maxDelay :: Milliseconds -> Milliseconds -> Milliseconds
maxDelay (Milliseconds a) (Milliseconds b) = Milliseconds (max a b)

-- | Read wall-clock time as milliseconds since the Unix epoch.
-- | Used by `elapsed` to track schedule start time without bringing
-- | the `Clock` service into the Effect-level Schedule API.
nowMs :: Effect Number
nowMs = do
  instant <- Now.now
  pure (unwrap (unInstant instant))
