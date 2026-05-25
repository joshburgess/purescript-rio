# Scheduling: retry and repeat

> **Naming convention.** This guide uses `RIO.Aff.*` module
> names in code samples. The same combinators exist under
> `RIO.Fiber.*` for the premier rio-fiber package
> (`recurs`, `spaced`, `exponential`, `jittered`, `intersect`,
> `andThen`, `whileInput`, `repeat`, `retry`, `retryOrElse`,
> `once`, `forever`, etc.), so the walkthrough body is usable
> with a mechanical prefix swap. A few divergences worth knowing:
> `step` is only exported from `RIO.Aff.Schedule`. In
> `RIO.Fiber.Schedule`, `Step` is the constructor of `Decision`
> and there is no public `step` runner; the test-helper pattern
> at the end of this guide is therefore rio-aff-only.
> rio-aff exports `mapSchedule` (with `mapOutput` as an alias);
> rio-fiber exports only `mapOutput`. `once` returns
> `Schedule r i Unit` in rio-aff but `Schedule a Int` in
> rio-fiber (the integer is the step count). `exponential` and
> `fibonacci` have the same output-channel split: rio-aff
> yields the current delay as `Milliseconds`, rio-fiber yields
> the 1-indexed step count as `Int`. Two type-level
> divergences: the rio-fiber `Schedule` is
> `Schedule input output = Schedule (input -> Effect (Decision
> input output))` (no env row; uses `Effect` rather than
> `RIO r ()`), and the step ADT is `Decision` (`Halt` / `Step`)
> rather than `Step` (`Done` / `Continue`).
> Unqualified shorthand like `RIO.Schedule` in the prose
> refers to whichever variant your package is using.

`RIO.Aff.Schedule` (rio-fiber: `RIO.Fiber.Schedule`) is the
policy layer for retrying failing actions and repeating
successful ones. A `Schedule` is a pure description of
*when* (delay) and *how many times* to fire, plus an output value at
each step. You drive it with one of the runners:

- `repeat`: run an action, then keep running it while the schedule
  says `Continue`. Stops on the first typed failure.
- `retry`: run an action, then on typed failure consult the schedule
  with the failure as input. While the schedule says `Continue`,
  sleep and retry. When the schedule says `Done`, surface the most
  recent failure.
- `retryOrElse`: like `retry`, but on exhaustion a fallback runs
  with the final failure.

All three sleep through the `Clock` service, so a virtual-time test
clock (`RIO.Aff.Test.Clock.newTestClock`, rio-fiber:
`RIO.Fiber.TestClock.newTestClock`) drives scheduled programs
deterministically.

## The type

```purescript
newtype Schedule r i o = Schedule (i -> RIO r () (Step r i o))

data Step r i o
  = Done
  | Continue o Milliseconds (Schedule r i o)
```

A schedule sees an input (a successful result for `repeat`, a typed
failure for `retry`) and decides one of two things: stop (`Done`)
or continue with this output, after this delay, using this next
schedule for the iteration after that. The "next schedule" lives
inside `Continue` so policies are pure values: combinators compose
them like data.

The error row is fixed to `()`. A schedule cannot itself fail with
a typed error; it can still read services from `r` if its decisions
depend on them.

## Building blocks

```purescript
recurs      :: Int -> Schedule r i Int               -- up to n times
spaced      :: Milliseconds -> Schedule r i Int      -- fixed gap, forever
exponential :: Milliseconds -> Number -> Schedule r i Milliseconds
                                                     -- rio-fiber: Schedule i Int
forever     :: Schedule r i Int                      -- spaced 0
once        :: Schedule r i Unit                     -- rio-fiber: Schedule i Int
```

A few examples:

```purescript
-- run an action 4 times: the initial run plus 3 repeats
repeat (recurs 3) action

-- poll every 30 seconds, forever
repeat (spaced (Milliseconds 30000.0)) (poll endpoint)

-- 100ms, 200ms, 400ms, 800ms, ...
exponential (Milliseconds 100.0) 2.0
```

## Combinators

`andThen` runs the first schedule to completion, then the second.
Outputs are tagged so callers know which side fired:

```purescript
-- 3 fast retries, then back off forever
andThen (recurs 3) (exponential (Milliseconds 100.0) 2.0)
```

`intersect` stops as soon as either side stops. The delay is the
larger of the two, and the output is the tuple of per-schedule
outputs:

```purescript
-- at most 5 retries with exponential backoff between them
intersect (recurs 5) (exponential (Milliseconds 100.0) 2.0)
```

`whileInput` is a guard: continue only while the input satisfies a
predicate. Useful for bailing out of `retry` on a specific failure
tag without exhausting the budget:

```purescript
-- retry up to 5 times, but only while the failure is `transient`
retry
  (whileInput isTransient (recurs 5))
  action
```

`jittered lo hi` multiplies every delay by a uniform random factor
in `[lo, hi]`. A tight band (`0.8`/`1.2`) softens a thundering herd
without changing the shape of the policy:

```purescript
jittered 0.8 1.2 (exponential (Milliseconds 100.0) 2.0)
```

`mapSchedule` transforms the output value. The cadence is
preserved; only the output side changes.

## Putting it together

A typical "retry with capped exponential backoff and jitter":

```purescript
import Data.Time.Duration (Milliseconds(..))
import RIO.Aff.Schedule
  ( exponential
  , intersect
  , jittered
  , recurs
  , retry
  )

policy =
  intersect
    (recurs 5)
    (jittered 0.8 1.2 (exponential (Milliseconds 100.0) 2.0))

robustFetch url = retry policy (fetch url)
```

That schedule says: at most 5 retries, with 100ms / 200ms / 400ms /
800ms / 1600ms base delays each multiplied by a random factor in
`[0.8, 1.2]`.

## `repeat` vs. `retry`

The two runners differ in *which channel feeds the schedule*:

- `repeat` feeds the action's success value as input to the
  schedule. The schedule stops on `Done`; the runner returns the
  action's last value. Typed failures from the action short-circuit
  the loop and surface on the parent's row.
- `retry` feeds the action's *failure* as input. The runner keeps
  retrying until the action succeeds or the schedule says `Done`.
  On exhaustion, the most recent failure surfaces; on success, the
  successful value surfaces.

`retryOrElse` is `retry` with a fallback: when the schedule says
`Done`, the fallback runs with the final failure as input. The
fallback's error row replaces the action's, so a recovered run can
change the surfaced error shape:

```purescript
retryOrElse
  (recurs 3)
  (fetch url)
  \_failure -> pure cachedDefault
```

## Driving schedules in tests

The runners sleep via the `Clock` service. Provide a `TestClock`
and call `tc.advance` from the test thread to drive a scheduled
program through any number of iterations without waiting on real
time:

```purescript
itRIO "exponential drives one step per matching advance" do
  counter <- liftEffect (Ref.new 0)
  tc <- liftAff newTestClock
  let
    action = liftEffect (Ref.modify (_ + 1) counter)
    program =
      repeat
        (intersect (recurs 3) (exponential (Milliseconds 100.0) 2.0))
        action

  _ <- liftAff (Aff.forkAff (runRIO' (provideAll { clock: tc.clock } program)))

  liftAff (Aff.delay (Milliseconds 0.0))
  -- 1: the initial run has happened, no advance yet
  liftAff (tc.advance (Milliseconds 100.0))
  -- 2: 100ms covered the first scheduled delay
  liftAff (tc.advance (Milliseconds 200.0))
  -- 3
  liftAff (tc.advance (Milliseconds 400.0))
  -- 4: schedule says Done after recurs 3
```

For schedules that don't fire (a `jittered` band sample, say),
`RIO.Schedule.step` exposes one decision so a test can inspect the
delay distribution directly without running the action:

```purescript
import RIO.Aff.Schedule (Schedule, Step(..), step)

collectDelays
  :: forall o
   . Int
  -> Schedule () Unit o
  -> RIO () () (Array Milliseconds)
collectDelays n0 sched0 = go n0 sched0 []
  where
  go 0 _ acc = pure acc
  go k s acc =
    step s unit >>= case _ of
      Done -> pure acc
      Continue _ ms next -> go (k - 1) next (Array.snoc acc ms)
```

That helper is what the test suite uses to verify `jittered` keeps
its delays in band over 100 samples.

## What `RIO.Schedule` does not do

- **Decision functions.** ZIO schedules expose a richer
  `(in, state) -> Decision[out, state]` interface that lets a
  schedule react to per-step time and metadata. `RIO.Schedule` does
  not. If you need a decision that depends on absolute wall-clock
  time, read the `Clock` inside the action and pass the timestamp
  as the schedule's input.
- **Choice / `||`.** There is no "first to fire" combinator; use
  `intersect` (both must continue) or `andThen` (sequence). A
  `choice`-style combinator is a reasonable later addition.
- **Reset by output.** Schedules see one input per step; there is
  no way for the schedule to declare "reset my counter because the
  last failure looked different from the previous ones." Wrap the
  action in a function that resets its own retry budget if you
  need that behaviour.

## Pointers

- `rio-aff/src/RIO/Aff/Schedule.purs` and
  `rio-fiber/src/RIO/Fiber/Schedule.purs`: the type,
  constructors, combinators, and runners.
- `rio-aff/test/Test/RIO/Aff/ScheduleSpec.purs`: tests covering
  `repeat`,
  `retry`, `retryOrElse`, `intersect`, exponential under the test
  clock, and the `jittered` band sample.
- `docs/06-concurrency.md`: how cancellation interacts with the
  same `Clock`-based sleep.
- `docs/07-testing.md`: more on the virtual-time test clock.
- Worked example:
  [`examples/worker-pool/`](../examples/worker-pool/) wires a
  `retry` policy onto each worker using
  `intersect (recurs maxRetries) (exponential base 2.0)` so the
  retry budget bounds the count while the delay grows
  exponentially.
