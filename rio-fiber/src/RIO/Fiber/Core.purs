-- | User-facing entry point for `rio-fiber`.
-- |
-- | This is the MVP surface for the fiber-backed `RIO`. The
-- | combinators here are a deliberately small slice of what
-- | `rio`'s `RIO.Core` exposes: pure / liftEffect, ask / asks,
-- | fail / catchAll, plus async, fork / join / interrupt, and
-- | runners (synchronous and callback-style). Layers, resources,
-- | and the rest land in later phases.
module RIO.Fiber.Core
  ( module Exports
  , ask
  , asks
  , async
  , bracket
  , catchAll
  , causeOf
  , die
  , ensuring
  , fail
  , failCause
  , forEach
  , fork
  , forkAll
  , forkAllInline
  , forkInline
  , interrupt
  , join
  , joinAll
  , liftEffect
  , parTraverse
  , race
  , raceAll
  , runRIO
  , runRIO'
  , runRIOCallback
  , timeout
  , uninterruptible
  , validatePar
  , zipPar
  , zipWithPar
  ) where

import Prelude

import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (Error, throwException, error)
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Clock (sleep)
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
-- | replaces the action's outcome. (Composing both outcomes is what
-- | `Cause` is for; it lands in a later phase.)
ensuring :: forall r e a. RIO r e Unit -> RIO r e a -> RIO r e a
ensuring (RIO fin) (RIO action) = RIO (Internal.opEnsuring fin action)

-- | Defer interruption for the duration of the wrapped action. The
-- | interrupt flag is preserved; the action just doesn't observe it
-- | until the mask is released.
uninterruptible :: forall r e a. RIO r e a -> RIO r e a
uninterruptible (RIO op) = RIO (Internal.opUninterruptible op)

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

-- | Race a non-empty array of actions; resume with the first
-- | outcome and interrupt the rest. An empty array raises a defect:
-- | a race with nothing to race has no defined winner.
raceAll :: forall r e a. Array (RIO r e a) -> RIO r e a
raceAll xs = case uncons xs of
  Nothing -> die (error "rio-fiber: raceAll on an empty array")
  Just { head, tail } -> foldl race head tail

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

-- | Run one fiber per element and collect the results in order.
-- | Fail-fast: the first non-success outcome interrupts the
-- | siblings and propagates to the caller.
parTraverse
  :: forall r e a b. (a -> RIO r e b) -> Array a -> RIO r e (Array b)
parTraverse f xs = RIO
  (Internal.opParTraverse (\a -> case f a of RIO m -> m) xs)

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
  case partition results of
    { failures: [], successes } -> pure successes
    { failures } -> failCause (foldl Cause.both Cause.empty failures)
  where
  partition rs = foldl step { failures: [], successes: [] } rs

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

