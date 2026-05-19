-- | User-facing entry point for `rio-fiber`.
-- |
-- | This is the MVP surface for the fiber-backed `RIO`. The
-- | combinators here are deliberately a small slice of what
-- | `rio`'s `RIO.Core` exposes: pure / liftEffect, ask / asks,
-- | fail / catchAll, plus the synchronous runner. Async, fork,
-- | resources, layers, and the rest land in later phases once
-- | the runtime grows past synchronous step-and-resume.
module RIO.Fiber.Core
  ( module Exports
  , ask
  , asks
  , catchAll
  , fail
  , liftEffect
  , runRIO
  , runRIO'
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Exception (throwException)
import RIO.Fiber.Internal (RIO(..))
import RIO.Fiber.Internal (RIO, runFiber) as Exports
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

-- | Run an `RIO` whose environment row is empty, surfacing the typed
-- | failure as the `Left` branch of an `Either`. Defects bubble up as
-- | thrown JS exceptions inside the resulting `Effect`.
runRIO
  :: forall e a
   . RIO () e a
  -> Effect (Either (Variant e) a)
runRIO m = do
  res <- Internal.runFiber m {}
  case res of
    Right a -> pure (Right a)
    Left (Right v) -> pure (Left v)
    Left (Left err) -> throwException err

-- | Run an `RIO` with both rows discharged. The error row is
-- | uninhabited so the result is returned unwrapped.
runRIO' :: forall a. RIO () () a -> Effect a
runRIO' m = do
  res <- runRIO m
  case res of
    Right a -> pure a
    Left v -> Variant.case_ v
