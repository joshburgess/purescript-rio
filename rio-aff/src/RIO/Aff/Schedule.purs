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
-- | using `RIO.Aff.Test.Clock` can drive scheduled programs in virtual
-- | time.
module RIO.Aff.Schedule
  ( Schedule
  , Step(..)
  , andThen
  , asUnit
  , bothS
  , collectAll
  , compose
  , addDelayM
  , dayOfWeek
  , delayed
  , delays
  , dimap
  , elapsed
  , eventually
  , exponential
  , fibonacci
  , fixed
  , forever
  , fromFunction
  , hourOfDay
  , intersect
  , minuteOfHour
  , jittered
  , mapDelay
  , mapInput
  , mapOutput
  , modifyDelayM
  , mapSchedule
  , once
  , passthrough
  , recurs
  , recursUntil
  , recursWhile
  , repeat
  , repeatN
  , repetitions
  , retry
  , retryN
  , retryOrElse
  , spaced
  , step
  , tapOutput
  , unfold
  , untilInput
  , untilOutput
  , whileInput
  , whileOutput
  , windowed
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (ceil, toNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number (floor) as Number
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Random as Random
import Unsafe.Coerce (unsafeCoerce)

import RIO.Aff.Clock (Clock, now, partsFromMs, sleep)
import RIO.Aff.Internal (RIO(..), mkEffectRIO, mkRIO, rioFail, unRIO, unsafeUnRIO)

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
  go k = Schedule \_ -> mkRIO \_ ->
    if k >= n then pure Done
    else
      let
        next = k + 1
      in
        pure (Continue next (Milliseconds 0.0) (go next))

-- | Fixed delay between firings, forever.
-- |
-- | ```purescript
-- | -- poll the endpoint every 30 seconds
-- | _ <- repeat (spaced (Milliseconds 30000.0)) (poll endpoint)
-- | ```
spaced :: forall r i. Milliseconds -> Schedule r i Int
spaced ms = go 0
  where
  go k = Schedule \_ -> mkRIO \_ ->
    let
      next = k + 1
    in
      pure (Continue next ms (go next))

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
  go ms = Schedule \_ -> mkRIO \_ ->
    let
      delay = Milliseconds ms
    in
      pure (Continue delay delay (go (ms * factor)))

-- | Fibonacci backoff: each delay is the sum of the previous two,
-- | starting from `base` and `base`. Output is the current delay.
-- |
-- | Useful when you want growth between linear (`spaced`) and
-- | aggressive (`exponential`). Pair with `jittered` to soften the
-- | thundering-herd risk.
-- |
-- | ```purescript
-- | -- 100ms, 100ms, 200ms, 300ms, 500ms, 800ms, 1300ms, ...
-- | fibonacci (Milliseconds 100.0)
-- | ```
fibonacci :: forall r i. Milliseconds -> Schedule r i Milliseconds
fibonacci (Milliseconds base) = go base base
  where
  go prev curr = Schedule \_ -> mkRIO \_ ->
    let
      delay = Milliseconds curr
    in
      pure (Continue delay delay (go curr (prev + curr)))

-- | Fire on a fixed cadence regardless of how long the work takes.
-- |
-- | Unlike `spaced ms`, which waits `ms` *after* each completion (so a
-- | slow run drifts the cadence), `fixed ms` targets every `ms`
-- | milliseconds on the wall clock. If a run overshoots the next
-- | scheduled fire time, the runner fires immediately and re-aligns
-- | to the next future multiple of the period; no firings are
-- | bunched up to "catch up".
-- |
-- | The output is the actual sleep duration the runner used for that
-- | step (so observers can see whether the cadence is being kept or
-- | the work is overshooting).
-- |
-- | Reads the `Clock` service to compute the remaining delay on each
-- | step, so the schedule's environment row carries `clock :: Clock`.
-- | Under `RIO.Aff.Test.Clock` this is fully deterministic.
-- |
-- | ```purescript
-- | -- poll on a 30s cadence; one slow poll won't shift later firings
-- | _ <- repeat (fixed (Milliseconds 30000.0)) pollOnce
-- | ```
fixed
  :: forall r i
   . Milliseconds
  -> Schedule (clock :: Clock | r) i Milliseconds
fixed (Milliseconds period) = start
  where
  start = Schedule \_ -> do
    Milliseconds t0 <- now
    pure (Continue (Milliseconds period) (Milliseconds period) (go (t0 + period)))

  go target = Schedule \_ -> do
    Milliseconds tNow <- now
    let
      -- If the action overshot the target, jump ahead to the next
      -- future multiple of `period`; otherwise sleep the remaining
      -- distance to the target.
      raw = target - tNow
      delay = if raw < 0.0 then 0.0 else raw
      -- Advance to the next future fire time. When we've overshot by
      -- one or more periods, this catches us up without re-firing the
      -- missed targets.
      nextTarget =
        if raw >= 0.0 then target + period
        else
          let
            overshoots = Int.toNumber (Int.ceil ((-raw) / period))
          in
            target + period * (overshoots + 1.0)
    pure (Continue (Milliseconds delay) (Milliseconds delay) (go nextTarget))

-- | Never stops; output is the iteration count. Equivalent to
-- | `spaced (Milliseconds 0.0)`.
forever :: forall r i. Schedule r i Int
forever = spaced (Milliseconds 0.0)

-- | Emit the total elapsed wall-clock time since the schedule started,
-- | with no inter-step delay. Pair with `intersect` to gate another
-- | schedule on a deadline ("retry forever, but stop after 30s").
-- |
-- | The first step emits `Milliseconds 0.0` (the schedule has just
-- | begun) and never sleeps. Subsequent steps emit the difference
-- | between the current wall-clock reading and the start time.
-- |
-- | Reads the `Clock` service, so this schedule is fully deterministic
-- | under `RIO.Aff.Test.Clock`.
-- |
-- | ```purescript
-- | -- retry forever, but give up once 30 seconds have elapsed
-- | retry
-- |   (untilOutput (\(Tuple _ ms) -> ms >= Milliseconds 30000.0)
-- |     (intersect forever elapsed))
-- |   fetch
-- | ```
elapsed
  :: forall r i. Schedule (clock :: Clock | r) i Milliseconds
elapsed = start
  where
  start = Schedule \_ -> do
    Milliseconds t0 <- now
    pure
      ( Continue (Milliseconds 0.0) (Milliseconds 0.0) (go t0)
      )

  go t0 = Schedule \_ -> do
    Milliseconds tNow <- now
    let diff = tNow - t0
    pure
      ( Continue (Milliseconds diff) (Milliseconds 0.0) (go t0)
      )

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
andThen sa sb = Schedule \i -> mkRIO \env -> do
  let Schedule fa = sa
  res <- unsafeUnRIO (fa i) env
  case res of
    Done -> stepRight sb i env
    Continue o d next ->
      pure (Continue (Left o) d (andThen next sb))
  where
  stepRight (Schedule fb) i env = do
    res <- unsafeUnRIO (fb i) env
    case res of
      Done -> pure Done
      Continue o d next ->
        pure (Continue (Right o) d (mapSchedule Right next))

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
intersect sa sb = Schedule \i -> mkRIO \env -> do
  let
    Schedule fa = sa
    Schedule fb = sb
  resA <- unsafeUnRIO (fa i) env
  resB <- unsafeUnRIO (fb i) env
  case resA, resB of
    Continue oa da nextA, Continue ob db nextB ->
      pure (Continue (Tuple oa ob) (maxMs da db) (intersect nextA nextB))
    _, _ -> pure Done
  where
  maxMs (Milliseconds a) (Milliseconds b) =
    Milliseconds (if a >= b then a else b)

-- | Alias for `intersect`, named to match the rio-fiber and ZIO
-- | spelling.
bothS
  :: forall r i o o'
   . Schedule r i o
  -> Schedule r i o'
  -> Schedule r i (Tuple o o')
bothS = intersect

-- | Sequential composition: thread the output of the first schedule
-- | into the input of the second. Both schedules must continue for
-- | the composition to continue; the combined delay is the maximum
-- | of the two so neither schedule's pacing is violated.
-- |
-- | When either inner schedule says `Done`, the composition is
-- | `Done`. Useful for layering "stop after N" (via `recurs`) over
-- | a paced policy like `exponential`, or for piping `elapsed` into
-- | a `whileOutput` cutoff.
-- |
-- | ```purescript
-- | -- exponential backoff, but stop once the total elapsed wall
-- | -- time crosses 10 seconds
-- | compose
-- |   (exponential (Milliseconds 100.0) 2.0)
-- |   (whileInput (\(Milliseconds t) -> t < 10000.0) elapsed)
-- | ```
compose
  :: forall r a b c
   . Schedule r a b
  -> Schedule r b c
  -> Schedule r a c
compose sa sb = Schedule \input -> mkRIO \env -> do
  let
    Schedule fa = sa
    Schedule fb = sb
  resA <- unsafeUnRIO (fa input) env
  case resA of
    Done -> pure Done
    Continue b delA sa' -> do
      resB <- unsafeUnRIO (fb b) env
      case resB of
        Done -> pure Done
        Continue c delB sb' ->
          pure (Continue c (maxMs delA delB) (compose sa' sb'))
  where
  maxMs (Milliseconds a) (Milliseconds b) =
    Milliseconds (if a >= b then a else b)

-- | The identity schedule: continues forever, echoing the input as
-- | the output with zero delay between recurrences. Useful as the
-- | left argument to `compose` to inspect or tag inputs while
-- | delegating pacing to another schedule.
-- |
-- | ```purescript
-- | -- expose every retry's failure variant alongside an exponential
-- | -- backoff policy
-- | retry (compose passthrough (exponential (Milliseconds 100.0) 2.0))
-- |   fetch
-- | ```
passthrough :: forall r a. Schedule r a a
passthrough = Schedule \input -> mkRIO \_ ->
  pure (Continue input (Milliseconds 0.0) passthrough)

-- | Replace the inner schedule's output with the delay it emitted.
-- | The inner schedule's pacing is preserved (each step still
-- | sleeps for the same amount); only the output changes. Useful
-- | for observing the timing sequence of a retry/backoff policy.
-- |
-- | ```purescript
-- | -- collect the delay sequence an exponential schedule produces
-- | collectAll (delays (exponential (Milliseconds 100.0) 2.0))
-- | ```
delays :: forall r a b. Schedule r a b -> Schedule r a Milliseconds
delays (Schedule s) = Schedule \input -> mkRIO \env -> do
  d <- unsafeUnRIO (s input) env
  case d of
    Done -> pure Done
    Continue _ delay next ->
      pure (Continue delay delay (delays next))

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
whileInput pred (Schedule s) = Schedule \i -> mkRIO \env ->
  if not (pred i) then pure Done
  else do
    res <- unsafeUnRIO (s i) env
    case res of
      Done -> pure Done
      Continue o d next ->
        pure (Continue o d (whileInput pred next))

-- | The dual of `whileInput`: stop the moment the input satisfies the
-- | predicate. Equivalent to `whileInput (not <<< pred)`, exposed as
-- | a named entry point so retry-until-condition policies are
-- | discoverable.
untilInput
  :: forall r i o
   . (i -> Boolean)
  -> Schedule r i o
  -> Schedule r i o
untilInput pred = whileInput (not <<< pred)

-- | Continue only while the underlying schedule's own output
-- | satisfies the predicate. Pair with `repeat` to stop on a sentinel
-- | value (a counter reaching a threshold, an `exponential` delay
-- | exceeding a budget, etc.).
whileOutput
  :: forall r i o
   . (o -> Boolean)
  -> Schedule r i o
  -> Schedule r i o
whileOutput pred (Schedule s) = Schedule \i -> mkRIO \env -> do
  res <- unsafeUnRIO (s i) env
  case res of
    Done -> pure Done
    Continue o d next ->
      if pred o then pure (Continue o d (whileOutput pred next))
      else pure Done

-- | The dual of `whileOutput`: stop the moment the underlying
-- | schedule's output satisfies the predicate.
untilOutput
  :: forall r i o
   . (o -> Boolean)
  -> Schedule r i o
  -> Schedule r i o
untilOutput pred = whileOutput (not <<< pred)

-- | `forever` but only while the input matches the predicate.
-- | Equivalent to `whileInput pred forever`, exposed as a named
-- | constructor for discoverability when reaching for a
-- | "retry while this condition holds" policy.
recursWhile :: forall r i. (i -> Boolean) -> Schedule r i Int
recursWhile pred = whileInput pred forever

-- | `forever` but only until the input matches the predicate.
-- | Equivalent to `untilInput pred forever`.
recursUntil :: forall r i. (i -> Boolean) -> Schedule r i Int
recursUntil pred = untilInput pred forever

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
jittered lo hi (Schedule s) = Schedule \i -> mkRIO \env -> do
  res <- unsafeUnRIO (s i) env
  case res of
    Done -> pure Done
    Continue o (Milliseconds ms) next -> do
      factorBase <- unsafeUnRIO randomNumber env
      let factor = lo + (hi - lo) * factorBase
      pure (Continue o (Milliseconds (ms * factor)) (jittered lo hi next))
  where
  randomNumber :: RIO r () Number
  randomNumber = mkEffectRIO \_ -> Random.random

-- | Transform a schedule's output. The cadence (number of steps and
-- | per-step delay) is preserved; only the output side changes.
mapSchedule
  :: forall r i o o'
   . (o -> o')
  -> Schedule r i o
  -> Schedule r i o'
mapSchedule f (Schedule s) = Schedule \i -> mkRIO \env -> do
  res <- unsafeUnRIO (s i) env
  case res of
    Done -> pure Done
    Continue o d next ->
      pure (Continue (f o) d (mapSchedule f next))

-- | Alias for `mapSchedule`, named to match the rio-fiber spelling.
mapOutput
  :: forall r i o o'
   . (o -> o')
  -> Schedule r i o
  -> Schedule r i o'
mapOutput = mapSchedule

-- | Transform a schedule's input. Pre-processes each input with `f`
-- | before passing it to the underlying schedule. Cadence, output,
-- | and termination are unchanged; only what the schedule "sees"
-- | for its decision changes.
-- |
-- | Contravariant on the input position: makes a `Schedule r i o`
-- | usable in a context that produces `i'` by mapping `i' -> i`.
mapInput
  :: forall r i i' o
   . (i' -> i)
  -> Schedule r i o
  -> Schedule r i' o
mapInput f (Schedule s) = Schedule \i' -> mkRIO \env -> do
  res <- unsafeUnRIO (s (f i')) env
  case res of
    Done -> pure Done
    Continue o d next ->
      pure (Continue o d (mapInput f next))

-- | Transform a schedule's per-step delay. The decision (number of
-- | steps, output values) is preserved; only the sleep time between
-- | steps changes.
-- |
-- | ```purescript
-- | -- exponential backoff, capped at 30 seconds per step
-- | mapDelay
-- |   (\(Milliseconds ms) -> Milliseconds (min ms 30000.0))
-- |   (exponential (Milliseconds 100.0) 2.0)
-- | ```
mapDelay
  :: forall r i o
   . (Milliseconds -> Milliseconds)
  -> Schedule r i o
  -> Schedule r i o
mapDelay f (Schedule s) = Schedule \i -> mkRIO \env -> do
  res <- unsafeUnRIO (s i) env
  case res of
    Done -> pure Done
    Continue o d next ->
      pure (Continue o (f d) (mapDelay f next))

-- | Effectful sibling of `mapDelay`. The transform runs in
-- | `RIO r ()`, so it can read services from `r` (a config
-- | service that exposes the current cap, a feature flag, a
-- | clock-aware jitter source). Schedules cannot fail, so the
-- | transform's error row is fixed to `()`.
-- |
-- | ```purescript
-- | -- cap each delay at the value read from a runtime config
-- | capByConfig
-- |   :: Schedule (config :: Config) Int Int
-- |   -> Schedule (config :: Config) Int Int
-- | capByConfig =
-- |   modifyDelayM \(Milliseconds ms) -> do
-- |     cfg <- ask (Proxy :: Proxy "config")
-- |     pure (Milliseconds (min ms cfg.maxBackoffMs))
-- | ```
modifyDelayM
  :: forall r i o
   . (Milliseconds -> RIO r () Milliseconds)
  -> Schedule r i o
  -> Schedule r i o
modifyDelayM f (Schedule s) = Schedule \i -> mkRIO \env -> do
  res <- unsafeUnRIO (s i) env
  case res of
    Done -> pure Done
    Continue o d next -> do
      delay <- unsafeUnRIO (f d) env
      pure (Continue o delay (modifyDelayM f next))

-- | Add an effectful delta to each step's delay, where the
-- | delta is computed from the step's output. The decision and
-- | the underlying delay are preserved; only an additive
-- | adjustment is layered on top.
-- |
-- | Like `modifyDelayM`, the per-step computation runs in
-- | `RIO r ()` so it can read services.
-- |
-- | ```purescript
-- | -- extend each retry by a randomly sampled jitter
-- | withJitter =
-- |   addDelayM \_ -> do
-- |     ms <- liftEffect (Random.randomRange 0.0 50.0)
-- |     pure (Milliseconds ms)
-- | ```
addDelayM
  :: forall r i o
   . (o -> RIO r () Milliseconds)
  -> Schedule r i o
  -> Schedule r i o
addDelayM f (Schedule s) = Schedule \i -> mkRIO \env -> do
  res <- unsafeUnRIO (s i) env
  case res of
    Done -> pure Done
    Continue o (Milliseconds d) next -> do
      Milliseconds add <- unsafeUnRIO (f o) env
      pure (Continue o (Milliseconds (d + add)) (addDelayM f next))

-- | Transform both the input and the output of a schedule in a
-- | single step. `dimap pre post s` is `mapSchedule post (mapInput
-- | pre s)`; it exists as a named entry point so the Profunctor
-- | shape of schedules is discoverable.
dimap
  :: forall r i i' o o'
   . (i' -> i)
  -> (o -> o')
  -> Schedule r i o
  -> Schedule r i' o'
dimap pre post (Schedule s) = Schedule \i' -> mkRIO \env -> do
  res <- unsafeUnRIO (s (pre i')) env
  case res of
    Done -> pure Done
    Continue o d next ->
      pure (Continue (post o) d (dimap pre post next))

-- | Each step emits the array of every output the schedule has
-- | produced so far (inclusive of the current one). Cadence and
-- | termination are unchanged; only the output type widens.
-- |
-- | Useful for collecting retry delays, recurrence counts, or
-- | observed timestamps as the schedule runs.
-- |
-- | Note: the accumulator grows without bound, so this is not
-- | appropriate for unbounded schedules unless you're going to
-- | stop early. Pair with `recurs`, `untilOutput`, etc.
collectAll
  :: forall r i o
   . Schedule r i o
  -> Schedule r i (Array o)
collectAll = go []
  where
  go acc (Schedule s) = Schedule \i -> mkRIO \env -> do
    res <- unsafeUnRIO (s i) env
    case res of
      Done -> pure Done
      Continue o d next ->
        let
          acc' = Array.snoc acc o
        in
          pure (Continue acc' d (go acc' next))

-- | Replace each output with the 1-based iteration count. Cadence
-- | is unchanged. Equivalent to `mapSchedule` ing the underlying
-- | output away and counting steps.
-- |
-- | ```purescript
-- | -- a deadline schedule that emits "1, 2, 3, ..." through repeat
-- | repetitions (spaced (Milliseconds 100.0))
-- | ```
repetitions
  :: forall r i o
   . Schedule r i o
  -> Schedule r i Int
repetitions = go 1
  where
  go n (Schedule s) = Schedule \i -> mkRIO \env -> do
    res <- unsafeUnRIO (s i) env
    case res of
      Done -> pure Done
      Continue _ d next ->
        pure (Continue n d (go (n + 1) next))

-- | Run a side-effecting handler with each emitted output, then
-- | pass the output through unchanged. Cadence is unaffected.
-- |
-- | Mirrors `RIO.Aff.Stream.tap` and ZIO's `Schedule.tapOutput`: drop
-- | this between schedule construction and `repeat` / `retry` to
-- | trace, count, or log every step without changing the program.
tapOutput
  :: forall r i o
   . (o -> RIO r () Unit)
  -> Schedule r i o
  -> Schedule r i o
tapOutput f (Schedule s) = Schedule \i -> mkRIO \env -> do
  res <- unsafeUnRIO (s i) env
  case res of
    Done -> pure Done
    Continue o d next -> do
      _ <- unsafeUnRIO (f o) env
      pure (Continue o d (tapOutput f next))

-- | Discard a schedule's output, collapsing it to `Unit`. Cadence
-- | and termination are preserved; only the output type changes.
-- |
-- | Equivalent to `mapSchedule (\_ -> unit)`, exposed as a named
-- | combinator so a "I don't care about the output" call site is
-- | self-documenting. Mirrors ZIO `Schedule.unit` (renamed here to
-- | avoid shadowing the prelude's `unit :: Unit`).
asUnit :: forall r i o. Schedule r i o -> Schedule r i Unit
asUnit = mapSchedule (\_ -> unit)

-- | Cap every per-step delay at `maxDelay`. Delays already at or
-- | below the cap pass through unchanged. The decision (number of
-- | steps, output values, termination) is preserved; only the sleep
-- | time is clamped.
-- |
-- | Useful for ensuring a runaway `exponential` or `fibonacci`
-- | backoff never sleeps for an unreasonable amount of time. Pair
-- | with `jittered` to keep the cap soft.
-- |
-- | ```purescript
-- | -- exponential backoff, but never sleep more than 30 seconds
-- | windowed (Milliseconds 30000.0)
-- |   (exponential (Milliseconds 100.0) 2.0)
-- | ```
windowed
  :: forall r i o
   . Milliseconds
  -> Schedule r i o
  -> Schedule r i o
windowed (Milliseconds cap) = mapDelay
  (\(Milliseconds ms) -> Milliseconds (if ms > cap then cap else ms))

-- | Add a one-time delay to the first emitted step. Subsequent
-- | steps run on the underlying schedule's normal cadence with no
-- | adjustment.
-- |
-- | Use this to phase-offset a periodic schedule (so two pollers
-- | sharing a backend don't tick in lockstep), or to give a process
-- | a warm-up window before its first action fires.
-- |
-- | A `Done` first step is passed through unchanged: there is no
-- | delay to apply if the schedule never recurs.
-- |
-- | ```purescript
-- | -- wait 1s after launch, then poll every 30s on the normal cadence
-- | delayed (Milliseconds 1000.0) (spaced (Milliseconds 30000.0))
-- | ```
delayed
  :: forall r i o
   . Milliseconds
  -> Schedule r i o
  -> Schedule r i o
delayed (Milliseconds offset) (Schedule s) = Schedule \i -> mkRIO \env -> do
  res <- unsafeUnRIO (s i) env
  case res of
    Done -> pure Done
    Continue o (Milliseconds d) next ->
      pure (Continue o (Milliseconds (d + offset)) next)

-- | Fire on each minute boundary (UTC). The emitted delay is the
-- | distance from the current wall-clock time to the next minute
-- | boundary; subsequent steps emit one full minute apart. The
-- | output is the minute-of-hour (`0..59`) at the firing target.
-- |
-- | Use with `intersect` and `whileOutput` to build "every minute
-- | at second 0" or "only during minute 30" predicates.
-- |
-- | ```purescript
-- | -- run an action exactly on each minute boundary
-- | _ <- repeat minuteOfHour action
-- | ```
minuteOfHour :: forall r i. Schedule (clock :: Clock | r) i Int
minuteOfHour = boundarySchedule minuteMs (\p -> p.minute)

-- | Fire on each hour boundary (UTC). The output is the
-- | hour-of-day (`0..23`) at the firing target.
-- |
-- | ```purescript
-- | -- run an hourly sync on the hour
-- | _ <- repeat (whileOutput (_ == 9) hourOfDay) runDailyJob
-- | ```
hourOfDay :: forall r i. Schedule (clock :: Clock | r) i Int
hourOfDay = boundarySchedule hourMs (\p -> p.hour)

-- | Fire on each UTC midnight boundary. The output is the ISO
-- | day-of-week (`1 = Monday`, `7 = Sunday`) of the firing target.
-- |
-- | ```purescript
-- | -- "every Monday at midnight"
-- | _ <- repeat (whileOutput (_ == 1) dayOfWeek) weeklyReport
-- | ```
dayOfWeek :: forall r i. Schedule (clock :: Clock | r) i Int
dayOfWeek = boundarySchedule dayMs (\p -> p.dayOfWeek)

-- | Internal helper: a schedule that fires on each `unit`
-- | boundary (a UTC multiple of `unit` ms since the epoch) and
-- | reads the requested field from the wall-clock parts at the
-- | firing target.
boundarySchedule
  :: forall r i
   . Number
  -> ( { year :: Int
       , month :: Int
       , day :: Int
       , hour :: Int
       , minute :: Int
       , second :: Int
       , millisecond :: Int
       , dayOfWeek :: Int
       }
       -> Int
     )
  -> Schedule (clock :: Clock | r) i Int
boundarySchedule unit_ readField = start
  where
  start = Schedule \_ -> do
    Milliseconds tNow <- now
    let
      target = Number.floor (tNow / unit_) * unit_ + unit_
      delay = Milliseconds (target - tNow)
      output = case partsFromMs (Milliseconds target) of
        Just p -> readField p
        _ -> -1
    pure (Continue output delay (go target))

  go target = Schedule \_ -> do
    Milliseconds tNow <- now
    let
      nextTarget = target + unit_
      raw = nextTarget - tNow
      delay = Milliseconds (if raw < 0.0 then 0.0 else raw)
      output = case partsFromMs (Milliseconds nextTarget) of
        Just p -> readField p
        _ -> -1
    pure (Continue output delay (go nextTarget))

minuteMs :: Number
minuteMs = 60000.0

hourMs :: Number
hourMs = 3600000.0

dayMs :: Number
dayMs = 86400000.0

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
  loop (Schedule s) = mkRIO \env -> do
    let envInner = (unsafeCoerce env :: Record r)
    a <- unsafeUnRIO action envInner
    stepRes <- unsafeUnRIO (s a) envInner
    case stepRes of
      Done -> pure a
      Continue _ ms next -> do
        _ <- unsafeUnRIO (sleepIfPositive ms) env
        unsafeUnRIO (loop next) env

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
  loop (Schedule s) = mkRIO \env -> do
    let envInner = (unsafeCoerce env :: Record r)
    res <- unRIO action envInner
    case res of
      Right a -> pure a
      Left v -> do
        stepRes <- unsafeUnRIO (s v) envInner
        case stepRes of
          Done -> rioFail v
          Continue _ ms next -> do
            _ <- unsafeUnRIO (sleepIfPositive ms) env
            unsafeUnRIO (loop next) env

-- | `repeat (recurs n)` - convenience for "run the action `n + 1`
-- | times total (initial run plus `n` repeats)". Returns the
-- | action's last value.
repeatN
  :: forall r e a
   . Int
  -> RIO r e a
  -> RIO (clock :: Clock | r) e a
repeatN n = repeat (recurs n)

-- | `retry (recurs n)` - convenience for "up to `n` retries on
-- | typed failure, no delay between attempts". The most recent
-- | failure is re-raised when the budget runs out.
retryN
  :: forall r e a
   . Int
  -> RIO r e a
  -> RIO (clock :: Clock | r) e a
retryN n = retry (recurs n)

-- | Retry forever (no delay between attempts) until the action
-- | succeeds. The caller's error row is discharged because no typed
-- | failure can ever surface; defects still propagate and abort the
-- | loop.
-- |
-- | Mirrors ZIO `ZIO.eventually` / Effect-TS `Effect.eventually`.
-- | Use this for "this will succeed eventually if I keep retrying"
-- | situations: an idempotent network call against a flapping
-- | dependency, a Ref-CAS loop that must commit at least once.
-- |
-- | No clock service is required because there is no inter-attempt
-- | delay. For backed-off retries with a cap, reach for `retry` or
-- | `retryOrElse` with an explicit schedule instead.
-- |
-- | ```purescript
-- | -- keep pinging the dependency until it answers; defects abort
-- | result <- eventually pingDependency
-- | ```
eventually
  :: forall r e e' a
   . RIO r e a
  -> RIO r e' a
eventually action = mkRIO \r ->
  let
    loop = do
      res <- unRIO action r
      case res of
        Right a -> pure a
        Left _ -> loop
  in
    loop

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
  loop (Schedule s) = mkRIO \env -> do
    let envInner = (unsafeCoerce env :: Record r)
    res <- unRIO action envInner
    case res of
      Right a -> pure a
      Left v -> do
        stepRes <- unsafeUnRIO (s v) envInner
        case stepRes of
          Done -> unsafeUnRIO (fallback v) envInner
          Continue _ ms next -> do
            _ <- unsafeUnRIO (sleepIfPositive ms) env
            unsafeUnRIO (loop next) env

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

-- | Build a schedule from a state-threading function. Starts at
-- | `seed` and at each step calls `f state input`, which returns
-- | the per-step output, delay, and the next state. The schedule
-- | never stops.
-- |
-- | This is the lowest-level constructor for custom schedules:
-- | when none of the named combinators fit, you can describe an
-- | arbitrary policy by carrying explicit state across iterations.
-- | For schedules that need to read services or fail, drop down to
-- | the `Schedule` newtype directly.
-- |
-- | ```purescript
-- | -- a schedule that yields the running sum of inputs
-- | sumSchedule :: Schedule () Int Int
-- | sumSchedule = unfold 0 \acc i ->
-- |   let next = acc + i
-- |   in { output: next, delay: Milliseconds 0.0, state: next }
-- | ```
unfold
  :: forall r s i o
   . s
  -> (s -> i -> { output :: o, delay :: Milliseconds, state :: s })
  -> Schedule r i o
unfold seed f = go seed
  where
  go s = Schedule \i -> mkRIO \_ ->
    let
      r = f s i
    in
      pure (Continue r.output r.delay (go r.state))

-- | Build a stateless schedule from a pure function on the input.
-- | The schedule never stops; each step emits the function's
-- | result and the requested delay.
-- |
-- | Useful as an escape hatch when the only thing you need is a
-- | reactive per-input decision (the policy itself has no memory).
-- |
-- | ```purescript
-- | -- delay scaled by the magnitude of the input
-- | proportional :: Schedule () Int Int
-- | proportional = fromFunction \i ->
-- |   { output: i
-- |   , delay: Milliseconds (50.0 * toNumber i)
-- |   }
-- | ```
fromFunction
  :: forall r i o
   . (i -> { output :: o, delay :: Milliseconds })
  -> Schedule r i o
fromFunction f = Schedule \i -> mkRIO \_ ->
  let
    r = f i
  in
    pure (Continue r.output r.delay (fromFunction f))

-- | Internal: sleep only when the requested delay is positive.
-- | Avoids an unnecessary `delay 0` round-trip.
sleepIfPositive :: forall r. Milliseconds -> RIO (clock :: Clock | r) () Unit
sleepIfPositive ms =
  if unwrap ms > 0.0 then sleep ms
  else mkRIO \_ -> pure unit
