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
  , fork
  , interrupt
  , join
  , liftEffect
  , parTraverse
  , race
  , raceAll
  , runRIO
  , runRIO'
  , runRIOCallback
  , timeout
  , uninterruptible
  , zipPar
  , zipWithPar
  ) where

import Prelude

import Data.Array (uncons)
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
import RIO.Fiber.Clock (sleep)
import RIO.Fiber.Clock (sleep) as Exports
import RIO.Fiber.Internal (Fiber, Outcome(..), RIO(..))
import RIO.Fiber.Internal (Fiber, Outcome(..), RIO, observeFiber, runFiber, startFiber) as Exports
import RIO.Fiber.Internal as Internal

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

-- | Fork a child fiber that runs concurrently. Returns the fiber
-- | handle so callers can `join` or `interrupt` it. The child
-- | inherits the parent's environment at the point of fork.
fork :: forall r e a. RIO r e a -> RIO r e (Fiber e a)
fork (RIO op) = RIO (Internal.opFork op)

-- | Suspend the current fiber until the target completes; propagate
-- | its outcome (success / typed failure / defect / interrupt).
join :: forall r e a. Fiber e a -> RIO r e a
join f = RIO (Internal.opJoin f)

-- | Request interruption of the target fiber. Best-effort: the
-- | target completes with `Interrupted` at its next safe point.
interrupt :: forall r e a. Fiber e a -> RIO r e Unit
interrupt f = RIO (Internal.opInterrupt f)

-- | Synchronous runner for an `RIO` with an empty environment row.
-- | Returns the typed failure on `Left`, the value on `Right`. If
-- | the program suspends (e.g. on `async` or `join`), this runner
-- | raises a JS exception; use `runRIOCallback` for async programs.
-- | Defects are re-raised as exceptions.
runRIO :: forall e a. RIO () e a -> Effect (Either (Variant e) a)
runRIO m = do
  res <- Internal.runFiberSync m {}
  case res of
    Just (Success a) -> pure (Right a)
    Just (Fail v) -> pure (Left v)
    Just (Die err) -> throwException err
    Just Interrupted -> throwException (error "rio-fiber: program was interrupted")
    Nothing -> throwException (error "rio-fiber: program suspended; use runRIOCallback")

-- | Run an `RIO` with both rows discharged. The error row is
-- | uninhabited so the result is returned unwrapped. Same sync
-- | constraints as `runRIO`.
runRIO' :: forall a. RIO () () a -> Effect a
runRIO' m = do
  res <- runRIO m
  case res of
    Right a -> pure a
    Left v -> Variant.case_ v

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

-- | Run two actions concurrently; resume with whichever finishes
-- | first and interrupt the loser. A typed failure or defect from
-- | the winner short-circuits the race. If both are interrupted
-- | externally the result is `Interrupted`.
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

