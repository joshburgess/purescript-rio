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
  , catchAll
  , fail
  , fork
  , interrupt
  , join
  , liftEffect
  , runRIO
  , runRIO'
  , runRIOCallback
  , sleep
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Exception (throwException, error)
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

-- | Suspend for the given duration. Interrupting the fiber while
-- | it sleeps fires the underlying `clearTimeout` so no callback
-- | runs after the interrupt point.
sleep :: forall r e. Milliseconds -> RIO r e Unit
sleep (Milliseconds ms) = async \cb -> do
  id <- _setTimeout ms (cb (Right unit))
  pure (_clearTimeout id)

foreign import data TimeoutId :: Type
foreign import _setTimeout :: Number -> Effect Unit -> Effect TimeoutId
foreign import _clearTimeout :: TimeoutId -> Effect Unit
