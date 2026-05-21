-- | User-facing entry point for `rio-fiber`.
-- |
-- | The fiber-backed `RIO r e a` lives here, along with the
-- | combinators that programs typically reach for: `pure` /
-- | `liftEffect`, `ask` / `asks`, `fail` / `failCause` /
-- | `catchAll`, structured concurrency (`fork` / `join` /
-- | `interrupt` / `race*` / `parTraverse` / `validatePar`),
-- | resources (`bracket` / `ensuring` / `acquireRelease`), and
-- | runners (`runRIO'`, callback-style `runAsync`, and Aff bridges
-- | exposed via `RIO.Fiber.Aff`).
module RIO.Fiber.Core
  ( module Exports
  , ask
  , asks
  , async
  , awaitOutcome
  , awaitAllOutcomes
  , bracket
  , catchAll
  , causeOf
  , checkInterruptible
  , die
  , ensuring
  , ensuringWith
  , fail
  , failCause
  , filterOrDie
  , filterOrFail
  , firstSuccessOf
  , forEach
  , forever
  , fork
  , forkAll
  , forkAllInline
  , forkInline
  , ignore
  , interrupt
  , iterate
  , join
  , joinAll
  , liftEffect
  , loop
  , never
  , onExit
  , parTraverse
  , partition
  , poll
  , race
  , raceAll
  , runRIO
  , runRIO'
  , runRIOCallback
  , timed
  , timeout
  , uninterruptible
  , uninterruptibleMask
  , unlessRIO
  , validatePar
  , whenRIO
  , yieldNow
  , zipFiber
  , zipPar
  , zipWithFiber
  , zipWithPar
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Exception (Error, throwException, error)
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Clock (currentEpoch, sleep)
import RIO.Fiber.Clock (sleep) as Exports
import RIO.Fiber.Internal (Fiber, Outcome, RIO(..))
import RIO.Fiber.Internal (Fiber, Outcome(..), RIO, observeFiber, runFiber, startFiber) as Exports
import RIO.Fiber.Internal as Internal
import Unsafe.Coerce (unsafeCoerce)

-- | Lift a synchronous `Effect` into `RIO`.
liftEffect :: forall r e a. Effect a -> RIO r e a
liftEffect e = RIO (Internal.opLiftEffect e)

-- | Read the entire environment record.
ask :: forall r e. RIO r e (Record r)
ask = RIO Internal.opAsk

-- | Read a projection of the environment.
asks :: forall r e a. (Record r -> a) -> RIO r e a
asks f = map f ask

-- | Raise a typed failure on the chosen tag.
fail :: forall r e a. Variant e -> RIO r e a
fail v = RIO (Internal.opFail v)

-- | Raise a structured `Cause` directly. Useful when re-raising a
-- | cause captured via `causeOf`, or when reporting two independent
-- | leaf failures together.
failCause :: forall r e a. Cause e -> RIO r e a
failCause c = RIO (Internal.opFailCause (Internal.causeToJS c))

-- | Crash the fiber with a JS defect. Goes through the normal
-- | unwind path (finalizers run) and surfaces as `Die err` at the
-- | runner. Use this for "should never happen" invariants, not for
-- | expected business errors (those belong in the typed row).
die :: forall r e a. Error -> RIO r e a
die err = RIO (Internal.opLiftEffect (throwException err))

-- | Handle every typed failure with a recovery action. The handler
-- | sees the original error row `e`; the recovered program runs in
-- | a new (possibly empty) row `e'`.
catchAll
  :: forall r e e' a
   . (Variant e -> RIO r e' a)
  -> RIO r e a
  -> RIO r e' a
catchAll handler (RIO m) =
  RIO (Internal.opCatchAll (\v -> case handler v of RIO m' -> m') m)

-- | Suspend the fiber on a register-callback primitive. The register
-- | function receives a single resume callback that takes an
-- | `Either (Variant e) a`. The returned `Effect Unit` is the
-- | best-effort canceller invoked if the fiber is interrupted.
async
  :: forall r e a
   . ((Either (Variant e) a -> Effect Unit) -> Effect (Effect Unit))
  -> RIO r e a
async register = RIO
  ( Internal.opAsync \onOk onFail ->
      register \result -> case result of
        Right a -> onOk a
        Left v -> onFail v
  )

-- | A fiber that never completes on its own. Useful as the "do
-- | nothing" branch of a `race` or as a parked fiber waiting to be
-- | interrupted externally. The fiber stays suspended until it is
-- | interrupted; the interrupt fires the registered no-op canceller
-- | and propagates through the normal unwind path.
never :: forall r e a. RIO r e a
never = async \_ -> pure (pure unit)

-- | Repeat `action` indefinitely. The result type is fully
-- | polymorphic because the loop never terminates on its own: it
-- | runs until interrupted, until a typed failure is raised, or
-- | until a defect aborts the fiber.
-- |
-- | Useful for daemon-style workers that consume from a queue or
-- | tick from a schedule:
-- |
-- |     forkScoped (forever (Q.take queue >>= handle))
forever :: forall r e a b. RIO r e a -> RIO r e b
forever action = action >>= \_ -> forever action

-- | Record how long `action` took, alongside its result. The
-- | duration is measured against the active `Clock`, so a virtual
-- | clock makes the measurement deterministic in tests.
-- |
-- | The duration covers only the success path. A typed failure,
-- | defect, or interrupt short-circuits before the second clock
-- | read, so callers see the original outcome unchanged. Use
-- | `Metrics.withTimer` if you need duration on every outcome.
timed :: forall r e a. RIO r e a -> RIO r e (Tuple Milliseconds a)
timed action = do
  Milliseconds startMs <- currentEpoch
  a <- action
  Milliseconds endMs <- currentEpoch
  pure (Tuple (Milliseconds (endMs - startMs)) a)

-- | Run `action`; if its success value fails `predicate`, raise the
-- | typed failure produced by `onFalse`. The check happens at the
-- | success boundary and behaves like a guarded `fail`: defects
-- | and interrupts inside `action` are untouched.
filterOrFail
  :: forall r e a
   . (a -> Boolean)
  -> (a -> Variant e)
  -> RIO r e a
  -> RIO r e a
filterOrFail predicate onFalse action = do
  a <- action
  if predicate a then pure a else fail (onFalse a)

-- | Like `filterOrFail`, but turn a predicate violation into a
-- | defect via `onFalse`. The error row is preserved unchanged
-- | because the failure surfaces as `Die` rather than a typed
-- | error.
filterOrDie
  :: forall r e a
   . (a -> Boolean)
  -> (a -> Error)
  -> RIO r e a
  -> RIO r e a
filterOrDie predicate onFalse action = do
  a <- action
  if predicate a then pure a else die (onFalse a)

-- | Cooperative yield. The fiber records `unit` as its current
-- | success value, re-enqueues itself on the scheduler, and breaks
-- | out of the inner step loop. The next fiber in the run queue
-- | gets to make progress before this one resumes.
-- |
-- | Equivalent to the natural break that fires when the per-fiber
-- | tick budget runs out, but caller-driven: long pure loops that
-- | never hit a real suspension can sprinkle `yieldNow` between
-- | passes to keep the scheduler responsive without changing the
-- | program's structure.
yieldNow :: forall r e. RIO r e Unit
yieldNow = RIO Internal.opYieldNow

-- | Reflect the current fiber's interrupt-mask state into the
-- | success channel. Returns `true` when the fiber is in the normal
-- | (interruptible) state; returns `false` while the fiber is
-- | inside an `uninterruptible` region.
-- |
-- | This is a window into the runtime, not a setter: it does not
-- | change the mask. Use it to decide, inside a finalizer or a
-- | long-running loop, whether a pending interrupt would actually
-- | take effect at this point.
checkInterruptible :: forall r e. RIO r e Boolean
checkInterruptible = RIO Internal.opCheckInterruptible

-- | # Fork family
-- |
-- | Four ops cover the fan-out cases. They differ on two axes:
-- |
-- |   * **Shape**: one child (`fork` / `forkInline`) vs a batch
-- |     (`forkAll` / `forkAllInline`).
-- |   * **Scheduling**: queued (`fork` / `forkAll`) vs inline
-- |     (`forkInline` / `forkAllInline`).
-- |
-- | The matrix:
-- |
-- |     |          | queued     | inline           |
-- |     | -------- | ---------- | ---------------- |
-- |     | one      | fork       | forkInline       |
-- |     | batch    | forkAll    | forkAllInline    |
-- |
-- | **Queued vs inline.** A queued fork enqueues the child to start at
-- | the next scheduler tick; the parent runs its next op first and the
-- | child's first instruction lands on a fresh microtask. An inline
-- | fork drives the child synchronously to its first suspension (or to
-- | completion) before the parent's next op runs. For sync-bodied
-- | children (the body is `pure` / `liftEffect` / pure binds with no
-- | `async` or `join`), inline fork finishes the child in place, and
-- | the matching `join` resolves without going through
-- | `queueMicrotask`. For children that genuinely suspend, the two
-- | scheduling modes converge after the first await.
-- |
-- | **One vs batch.** The batch variants take an `Array (RIO r e a)`
-- | and walk it in a single JS loop, so a fan-out of N fibers costs
-- | one op dispatch instead of N nested binds. `forkAll xs` is
-- | semantically `traverse fork xs` but skips the ~2N BIND nodes that
-- | `traverseArrayImpl` would build; same for `forkAllInline xs` vs
-- | `traverse forkInline xs`.
-- |
-- | **Picking one.**
-- |
-- |   * One sync-bodied child you're about to `join`: `forkInline`.
-- |     Saves the microtask hop on both sides.
-- |   * One genuinely concurrent child (does I/O, sleeps, awaits a
-- |     ref): `fork`. The microtask hop costs nothing when the child
-- |     was going to suspend anyway.
-- |   * Batch fan-out of mostly-sync children: `forkAllInline`. Skips
-- |     the per-element bind chain AND the per-child microtask hop.
-- |   * Batch fan-out of genuinely concurrent children: `forkAll`.
-- |     Skips the per-element bind chain; keeps the queued scheduling
-- |     that lets siblings interleave naturally.
-- |
-- | Inline scheduling does NOT change observable semantics: an
-- | `interrupt` issued after the parent observes the handle still
-- | wins, parent / child ordering after the first suspension is the
-- | same as `fork`, and the child sees the same environment. The only
-- | observable difference is microbenchmark wall time when the child
-- | is sync-bodied.
-- |
-- | All four ops return the fiber handle(s); pair them with `join`,
-- | `joinAll`, or `interrupt` to consume the result.

-- | Fork a child fiber that runs concurrently. Returns the fiber
-- | handle so callers can `join` or `interrupt` it. The child
-- | inherits the parent's environment at the point of fork.
-- |
-- | The child is queued: it starts at the next scheduler tick, after
-- | the parent has run its next op. For sync-bodied children you mean
-- | to `join` immediately, `forkInline` skips the microtask hop on
-- | both sides; see the fork-family doc block above for the full
-- | matrix.
fork :: forall r e a. RIO r e a -> RIO r e (Fiber e a)
fork (RIO op) = RIO (Internal.opFork op)

-- | Inline variant of `fork`. Drives the child synchronously to its
-- | first suspension (or to completion) before returning the handle.
-- | For sync-bodied children this means the child has already
-- | finished by the time the parent observes the handle, and the
-- | subsequent `join` resolves without going through the microtask
-- | scheduler.
-- |
-- | Use this when both sides of a fork would otherwise spend their
-- | budget bouncing through `queueMicrotask`. The semantic difference
-- | from `fork` shows up in ordering: with `fork` the parent runs its
-- | next op first, then yields; with `forkInline` the child runs to
-- | its first await before the parent continues. Past the first
-- | await, the two variants behave identically.
forkInline :: forall r e a. RIO r e a -> RIO r e (Fiber e a)
forkInline (RIO op) = RIO (Internal.opForkInline op)

-- | Suspend the current fiber until the target completes; propagate
-- | its outcome (success / typed failure / defect / interrupt).
join :: forall r e a. Fiber e a -> RIO r e a
join f = RIO (Internal.opJoin f)

-- | Batch variant of `fork`. Forks one fiber per element of the
-- | array, returning the handles in order. Equivalent to
-- | `traverse fork xs` but goes through a single specialized op that
-- | walks the array in JS, so a fan-out of N fibers costs one op
-- | dispatch instead of N nested binds.
-- |
-- | Each child is queued (same scheduling as `fork`). For batches
-- | where the bodies are sync-bodied, prefer `forkAllInline`.
forkAll :: forall r e a. Array (RIO r e a) -> RIO r e (Array (Fiber e a))
forkAll xs = RIO (Internal.opForkAll (coerceOps xs))
  where
  coerceOps :: Array (RIO r e a) -> Array (Internal.Op r e a)
  coerceOps = unsafeCoerce

-- | Inline batch variant. Forks one fiber per element of the array,
-- | driving each child synchronously to its first suspension (or to
-- | completion) before moving on. Equivalent to
-- | `traverse forkInline xs` but goes through a single specialized op
-- | so a fan-out of N fibers costs one op dispatch instead of N
-- | nested binds.
-- |
-- | This is the right pick when you have a batch of mostly-sync
-- | children you intend to `joinAll` immediately: it eliminates both
-- | the per-element bind chain (saved by being a batch op) and the
-- | per-child microtask hop (saved by being inline). For batches of
-- | genuinely concurrent children, `forkAll` is the right pick; the
-- | inline savings collapse to nothing once each child suspends.
forkAllInline
  :: forall r e a. Array (RIO r e a) -> RIO r e (Array (Fiber e a))
forkAllInline xs = RIO (Internal.opForkAllInline (coerceOps xs))
  where
  coerceOps :: Array (RIO r e a) -> Array (Internal.Op r e a)
  coerceOps = unsafeCoerce

-- | Wait on a batch of pre-forked fibers and collect their results
-- | in order. Suspends until every fiber completes; the first non-
-- | success outcome propagates to the caller. Sibling fibers are not
-- | interrupted on failure (they were forked outside this call's
-- | scope; use `interrupt` explicitly to cancel them).
joinAll :: forall r e a. Array (Fiber e a) -> RIO r e (Array a)
joinAll fs = RIO (Internal.opJoinAll fs)

-- | Wait on a fiber and reflect its full outcome into the success
-- | channel. Unlike `join`, this never propagates the fiber's
-- | failure: success, typed failure, defect, and interrupt each
-- | surface as the corresponding `Outcome` constructor on the caller.
-- |
-- | Useful when you need to react to each branch's individual fate
-- | (logging both succeeded-and-failed children, partitioning a
-- | fan-out by exit reason) rather than fail-fast on the first one
-- | that didn't succeed.
awaitOutcome
  :: forall r e e' a. Fiber e a -> RIO r e' (Outcome e a)
awaitOutcome fib = async \resume -> do
  Internal.observeFiber fib (\o -> resume (Right o))
  pure (pure unit)

-- | Wait on a batch of fibers and collect each one's `Outcome` in
-- | order. Like `joinAll` but every fiber's individual fate is
-- | reported instead of failing fast on the first non-success.
-- |
-- | Backed by `parTraverse awaitOutcome`, so each await happens
-- | concurrently; the call returns once every fiber has settled.
awaitAllOutcomes
  :: forall r e e' a. Array (Fiber e a) -> RIO r e' (Array (Outcome e a))
awaitAllOutcomes = parTraverse awaitOutcome

-- | Combine two fiber handles into a new fiber whose result is the
-- | tuple of the two source results. The new fiber awaits both
-- | source fibers concurrently; it succeeds with `Tuple a b` when
-- | both succeed, fails fast on the first failure, and is
-- | independently interruptible.
-- |
-- | Interrupting the combined fiber does *not* propagate to the
-- | source fibers; they keep running. If you want their lifecycles
-- | tied, bind them to a `Scope` via `forkScoped` instead.
zipFiber
  :: forall r e a b
   . Fiber e a
  -> Fiber e b
  -> RIO r e (Fiber e (Tuple a b))
zipFiber = zipWithFiber Tuple

-- | Like `zipFiber` but combine the two results with the given
-- | function. Both source fibers are awaited concurrently in the
-- | derived fiber.
zipWithFiber
  :: forall r e a b c
   . (a -> b -> c)
  -> Fiber e a
  -> Fiber e b
  -> RIO r e (Fiber e c)
zipWithFiber f fa fb = fork (zipWithPar f (join fa) (join fb))

-- | Sequential traverse over an array. Runs `f item` for each item
-- | in order and collects the results. The first non-success outcome
-- | propagates and discards any partial results.
-- |
-- | Equivalent to `traverse f xs` but goes through a specialized op
-- | that holds a single interpreter frame for the whole walk, so a
-- | traverse of N elements pays one frame allocation instead of the
-- | ~2N bind nodes that `traverseArrayImpl` would build.
forEach :: forall r e a b. (a -> RIO r e b) -> Array a -> RIO r e (Array b)
forEach f xs = RIO (Internal.opForEach (\a -> case f a of RIO m -> m) xs)

-- | Request interruption of the target fiber. Best-effort: the
-- | target completes with `Interrupted` at its next safe point.
interrupt :: forall r e a. Fiber e a -> RIO r e Unit
interrupt f = RIO (Internal.opInterrupt f)

-- | Synchronous runner for an `RIO` with an empty environment row.
-- | Returns the typed failure on `Left`, the value on `Right`. If
-- | the program suspends (e.g. on `async` or `join`), this runner
-- | raises a JS exception; use `runRIOCallback` for async programs.
-- | Defects are re-raised as exceptions.
-- |
-- | Goes through the fused `_runFiberSyncEither` FFI: the OK and Fail
-- | paths build `Right` / `Left` in JS and return them directly; the
-- | Die / Interrupt / suspended paths throw from JS. Skips the layered
-- | `runFiberSync` -> `resultToOutcome` -> Outcome -> Maybe -> Either
-- | pipeline that the previous implementation walked through.
runRIO :: forall e a. RIO () e a -> Effect (Either (Variant e) a)
runRIO (RIO op) = Internal._runFiberSyncEither op {}

-- | Run an `RIO` with both rows discharged. The error row is
-- | uninhabited so the result is returned unwrapped. Same sync
-- | constraints as `runRIO`.
-- |
-- | Goes straight through the fused `_runFiberSyncOrThrow` FFI rather
-- | than `runFiberSync` + Maybe / Outcome / Either pattern matches.
-- | The OK path is one Fiber-status check + one field read on the JS
-- | side; the failure paths throw directly. There is no Outcome
-- | constructor alloc, no Maybe wrap, and no Either wrap per call.
runRIO' :: forall a. RIO () () a -> Effect a
runRIO' (RIO op) = Internal._runFiberSyncOrThrow op {}

-- | Callback-style runner. The callback receives the full outcome
-- | (including `Interrupted` as a dedicated case); the returned
-- | `Effect Unit` requests interruption of the running fiber.
runRIOCallback
  :: forall r e a
   . RIO r e a
  -> Record r
  -> (Outcome e a -> Effect Unit)
  -> Effect (Effect Unit)
runRIOCallback = Internal.runFiber

-- | Race an action against a timeout. Returns `Just a` if the action
-- | completes before the duration, `Nothing` if the timeout wins. The
-- | losing branch is interrupted. The timer goes through the active
-- | `Clock`, so a virtual clock makes timeout deterministic.
timeout :: forall r e a. Milliseconds -> RIO r e a -> RIO r e (Maybe a)
timeout dur action = race (Just <$> action) (sleep dur *> pure Nothing)

-- | Run a finalizer after the action regardless of how it terminates
-- | (success, typed failure, defect, or interrupt). The finalizer
-- | runs inside an uninterruptible region so a late interrupt cannot
-- | abandon it midway.
-- |
-- | If the finalizer itself raises a typed failure or defect, that
-- | replaces the action's outcome.
ensuring :: forall r e a. RIO r e Unit -> RIO r e a -> RIO r e a
ensuring (RIO fin) (RIO action) = RIO (Internal.opEnsuring fin action)

-- | Run a finalizer that observes the action's outcome. The handler
-- | receives `Right a` on success and `Left cause` on any non-success
-- | exit (typed failure, defect, or interrupt). The handler runs
-- | inside an uninterruptible region so a late interrupt cannot
-- | abandon it midway; if it raises a typed failure or defect, that
-- | replaces the action's outcome.
-- |
-- | This is the exit-aware variant of `ensuring`. Use it when the
-- | finalizer needs to log success and failure differently, record
-- | the cause to a metric, or skip cleanup work that's only needed
-- | on one branch.
ensuringWith
  :: forall r e a
   . RIO r e a
  -> (Either (Cause e) a -> RIO r e Unit)
  -> RIO r e a
ensuringWith action k = do
  result <- causeOf action
  uninterruptible (k result)
  case result of
    Right a -> pure a
    Left c -> failCause c

-- | Run a finalizer only on non-success exit. The handler receives
-- | the `Cause` describing why the action stopped (typed failure,
-- | defect, or interrupt) and must have a discharged error row.
-- | The handler runs inside an uninterruptible region.
-- |
-- | Equivalent to `ensuringWith` with the success branch discarded.
-- | Useful for "log if and only if this failed" patterns where the
-- | handler shouldn't be able to introduce a new failure mode.
onExit
  :: forall r e a
   . RIO r e a
  -> (Cause e -> RIO r () Unit)
  -> RIO r e a
onExit action k = ensuringWith action handler
  where
  handler (Right _) = pure unit
  handler (Left c) = catchAll (\v -> Variant.case_ v) (k c)

-- | Defer interruption for the duration of the wrapped action. The
-- | interrupt flag is preserved; the action just doesn't observe it
-- | until the mask is released.
uninterruptible :: forall r e a. RIO r e a -> RIO r e a
uninterruptible (RIO op) = RIO (Internal.opUninterruptible op)

-- | Run `body` uninterruptibly, but give it a `restore` function that
-- | temporarily lifts the mask back to its surrounding value for the
-- | wrapped action. This is the building block for resource-safe
-- | acquire / use / release patterns where the *use* phase should be
-- | interruptible while the surrounding bookkeeping should not.
-- |
-- | If the surrounding context was already masked when
-- | `uninterruptibleMask` was called, `restore` is the identity (there
-- | is no outer interruptibility to restore to). Otherwise `restore x`
-- | makes `x` interruptible for the duration of `x`'s execution; once
-- | `x` finishes (success, failure, or interrupt), the mask snaps
-- | back into place automatically.
-- |
-- | Multiple `restore` calls within the same `uninterruptibleMask`
-- | block are independent: each opens and closes its own
-- | interruptible window.
uninterruptibleMask
  :: forall r e a
   . ((forall b. RIO r e b -> RIO r e b) -> RIO r e a)
  -> RIO r e a
uninterruptibleMask body = RIO
  ( Internal.opMaskWithRestore \restoreOp ->
      case body (unsafeCoerce restoreOp) of
        RIO op -> op
  )

-- | Acquire / use / release with finalizer semantics. The release
-- | runs whether `use` succeeds, fails, defects, or is interrupted.
-- | The acquire itself is not interruptible (otherwise an interrupt
-- | between allocating the resource and installing the release would
-- | leak).
bracket
  :: forall r e a b
   . RIO r e a
  -> (a -> RIO r e Unit)
  -> (a -> RIO r e b)
  -> RIO r e b
bracket acquire release use = uninterruptible acquire >>=
  \resource -> ensuring (release resource) (use resource)

-- | Run two actions concurrently; the first success wins and
-- | interrupts the loser. A single failure waits for the other side
-- | to settle: if the other side succeeds, that success still wins;
-- | if the other side also fails, the two causes are composed with
-- | `Cause.both`. If both branches are interrupted externally the
-- | result is `Interrupted`.
race :: forall r e a. RIO r e a -> RIO r e a -> RIO r e a
race (RIO l) (RIO r) = RIO (Internal.opRace l r)

-- | Race a non-empty array of actions; resume with the first success
-- | and interrupt the rest. If every branch is interrupted the parent
-- | inherits the interrupt; if every branch fails the failures are
-- | composed with `Cause.both`. An empty array raises a defect: a race
-- | with nothing to race has no defined winner.
-- |
-- | Backed by a native runtime op that fans out in one step rather than
-- | building a `race` tree, so an N-way race spawns N children directly
-- | without intermediate coordinating fibers.
raceAll :: forall r e a. Array (RIO r e a) -> RIO r e a
raceAll xs = RIO (Internal.opRaceAll (coerceOps xs))
  where
  coerceOps :: Array (RIO r e a) -> Array (Internal.Op r e a)
  coerceOps = unsafeCoerce

-- | Sequential fallback. Tries each action in order; the first one
-- | to succeed wins and the rest are not started. If every action
-- | fails with a typed error, the last failure propagates. An empty
-- | array raises a defect: a chain with nothing to try has no
-- | defined fallback.
-- |
-- | This is the sequential analogue of `raceAll`: same "first
-- | success wins" semantics, but actions are tried one at a time
-- | rather than running concurrently. Useful when each branch
-- | itself does work (DNS lookups, retry strategies with different
-- | backoffs) and you'd rather pay for the cheaper branches first.
firstSuccessOf :: forall r e a. Array (RIO r e a) -> RIO r e a
firstSuccessOf actions = case Array.uncons actions of
  Nothing -> die (error "rio-fiber: firstSuccessOf with empty array")
  Just { head, tail } ->
    foldl (\acc next -> catchAll (\_ -> next) acc) head tail

-- | Run the wrapped action and capture its leaf cause on failure.
-- | A success becomes `Right a`; a typed failure / defect / interrupt
-- | becomes `Left` of the corresponding `Cause`. The outer error
-- | row is independent so callers can discharge it.
-- |
-- | This is the gateway to inspecting causes from user code. Note
-- | that `causeOf` swallows interrupts: the wrapped action runs to
-- | completion and the captured `Cause.Interrupt` is the only
-- | trace. Callers who want the interrupt to propagate must re-raise.
causeOf :: forall r e e' a. RIO r e a -> RIO r e' (Either (Cause e) a)
causeOf (RIO m) = RIO (Internal.opBind (Internal.opPeel m) goPure)
  where
  goPure r = Internal.opPure (Internal.peelToCauseEither r)

-- | Run an action and discard both its result and any typed failure
-- | or defect it raises. The returned action always succeeds with
-- | `unit`. Interrupts are still respected and propagate.
-- |
-- | Useful for fire-and-forget effects whose outcome the caller
-- | doesn't care about: log a metric, ping a webhook, etc.
ignore :: forall r e e' a. RIO r e a -> RIO r e' Unit
ignore rio = do
  _ <- causeOf rio
  pure unit

-- | Run one fiber per element and collect the results in order.
-- | Fail-fast: the first non-success outcome interrupts the
-- | siblings and propagates to the caller.
parTraverse
  :: forall r e a b. (a -> RIO r e b) -> Array a -> RIO r e (Array b)
parTraverse f xs = RIO
  (Internal.opParTraverse (\a -> case f a of RIO m -> m) xs)

-- | Run `f` on every element concurrently and partition the
-- | outcomes into typed failures and successes. Every branch runs to
-- | completion; sibling branches are not interrupted on a typed
-- | failure. The outer error row is discharged because failures are
-- | reflected into the success channel as the `failures` array.
-- |
-- | Defects and interrupts are *not* caught by this combinator: they
-- | escape just like they would from any other concurrent walk. Use
-- | this when each element's failure is independently meaningful and
-- | you need both halves of the result (e.g., to report a per-item
-- | status to the caller).
partition
  :: forall r e e' a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e' { failures :: Array (Variant e), successes :: Array b }
partition f xs = do
  results <- parTraverse (\a -> catchAll (\v -> pure (Left v)) (Right <$> f a)) xs
  pure (foldl step { failures: [], successes: [] } results)
  where
  step acc (Left v) = acc { failures = acc.failures <> [ v ] }
  step acc (Right b) = acc { successes = acc.successes <> [ b ] }

-- | Run every branch to completion (not fail-fast). On all-success,
-- | yields the array of results. If any branch failed, the resulting
-- | typed-failure / defect / interrupt causes are merged left-to-right
-- | with `Cause.both`, and the combined `Cause` is raised via
-- | `failCause`. Unlike `parTraverse`, sibling branches are not
-- | interrupted on the first failure: every branch contributes its
-- | outcome to the composed cause.
validatePar
  :: forall r e a b. (a -> RIO r e b) -> Array a -> RIO r e (Array b)
validatePar f xs = do
  results <- parTraverse (\a -> causeOf (f a)) xs
  case splitResults results of
    { failures: [], successes } -> pure successes
    { failures } -> failCause (foldl Cause.both Cause.empty failures)
  where
  splitResults rs = foldl step { failures: [], successes: [] } rs

  step acc (Left c) = acc { failures = acc.failures <> [ c ] }
  step acc (Right b) = acc { successes = acc.successes <> [ b ] }

-- | Run two actions concurrently; succeed with both results when
-- | both complete. Fail-fast on the first failure or interrupt.
zipPar :: forall r e a b. RIO r e a -> RIO r e b -> RIO r e (Tuple a b)
zipPar = zipWithPar Tuple

-- | Like `zipPar` but combine the two results with the given
-- | function. Both branches run concurrently.
zipWithPar
  :: forall r e a b c
   . (a -> b -> c)
  -> RIO r e a
  -> RIO r e b
  -> RIO r e c
zipWithPar f ra rb = do
  pair <- parTraverse identity [ map Left ra, map Right rb ]
  case pair of
    [ Left x, Right y ] -> pure (f x y)
    _ -> RIO
      ( Internal.opLiftEffect
          (throwException (error "rio-fiber: zipWithPar invariant violated"))
      )

-- | Non-blocking outcome check. Returns `Just outcome` if the fiber
-- | has completed (with any of success, typed failure, defect, or
-- | interrupt), `Nothing` while it is still running.
poll :: forall r e a. Fiber e a -> RIO r e (Maybe (Outcome e a))
poll fib = liftEffect $ pure
  if Internal.fiberIsDone fib then Just (Internal.fiberOutcome fib)
  else Nothing

-- | Run `body` only when the condition action returns `true`.
whenRIO :: forall r e. RIO r e Boolean -> RIO r e Unit -> RIO r e Unit
whenRIO cond body = do
  c <- cond
  when c body

-- | Run `body` only when the condition action returns `false`.
unlessRIO :: forall r e. RIO r e Boolean -> RIO r e Unit -> RIO r e Unit
unlessRIO cond body = do
  c <- cond
  unless c body

-- | Iterate `f` starting at `seed` until the predicate `cont` is
-- | `false`. Returns the final value (the first one for which `cont`
-- | returned `false`). `iterate seed cont f` is equivalent to a
-- | while-style loop in imperative code.
iterate
  :: forall r e a
   . a
  -> (a -> Boolean)
  -> (a -> RIO r e a)
  -> RIO r e a
iterate seed cont f =
  if cont seed then do
    next <- f seed
    iterate next cont f
  else pure seed

-- | Loop with explicit state advancement and collection. While
-- | `cont s` holds, run `body s` and collect the result, then
-- | advance state with `step s`.
loop
  :: forall r e s a
   . s
  -> (s -> Boolean)
  -> (s -> s)
  -> (s -> RIO r e a)
  -> RIO r e (Array a)
loop seed cont step body = go seed []
  where
  go s acc =
    if cont s then do
      a <- body s
      go (step s) (acc <> [ a ])
    else pure acc

