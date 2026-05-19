-- | One-shot synchronization cell.
-- |
-- | A `Deferred e a` starts empty. Any fiber can await it (suspending
-- | until it is completed); any fiber can complete it once with either
-- | a success or a typed failure. Subsequent completions are no-ops
-- | and return `false` so callers can detect the race loser.
-- |
-- | Use it to wire one fiber's result into another, or as the building
-- | block for higher-level coordination primitives like `Semaphore`
-- | and `Queue`.
module RIO.Fiber.Deferred
  ( Deferred
  , make
  , await
  , awaitPure
  , succeed
  , _succeed
  , fail
  , poll
  , isDone
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import RIO.Fiber.Core (RIO, async, liftEffect)

-- | A one-shot cell parameterised by its typed-failure row and
-- | success type.
foreign import data Deferred :: Row Type -> Type -> Type

foreign import _make :: forall e a. Effect (Deferred e a)
foreign import _await
  :: forall e a
   . Deferred e a
  -> (a -> Effect Unit)
  -> (Variant e -> Effect Unit)
  -> Effect Int
foreign import _unsubscribe :: forall e a. Deferred e a -> Int -> Effect Unit
foreign import _succeed :: forall e a. Deferred e a -> a -> Effect Boolean
foreign import _fail :: forall e a. Deferred e a -> Variant e -> Effect Boolean
foreign import _pollIsDone :: forall e a. Deferred e a -> Effect Boolean
foreign import _pollIsOk :: forall e a. Deferred e a -> Effect Boolean
foreign import _pollOk :: forall e a. Deferred e a -> Effect a
foreign import _pollFail :: forall e a. Deferred e a -> Effect (Variant e)

-- | Allocate a fresh empty Deferred.
make :: forall e a. Effect (Deferred e a)
make = _make

-- | Suspend until the cell is completed; resume with the value or
-- | typed failure that was set. If the awaiting fiber is interrupted
-- | the waiter is removed so a later completion is not delivered
-- | into a stale fiber.
await :: forall r e a. Deferred e a -> RIO r e a
await d = async \cb -> do
  id <- _await d (\a -> cb (Right a)) (\v -> cb (Left v))
  pure (_unsubscribe d id)

-- | Await a Deferred whose failure row is `()`. Because no
-- | `Variant ()` value can be constructed, the failure callback is
-- | unreachable, so the await can be embedded in any outer row.
awaitPure :: forall r e a. Deferred () a -> RIO r e a
awaitPure d = async \cb -> do
  id <- _await d (\a -> cb (Right a)) (\_ -> pure unit)
  pure (_unsubscribe d id)

-- | Complete the cell with a success. Returns `true` if this call
-- | won the race, `false` if some prior call already completed it.
succeed :: forall r e e' a. Deferred e a -> a -> RIO r e' Boolean
succeed d a = liftEffect (_succeed d a)

-- | Complete the cell with a typed failure. Returns `true` if this
-- | call won the race.
fail :: forall r e e' a. Deferred e a -> Variant e -> RIO r e' Boolean
fail d v = liftEffect (_fail d v)

-- | Non-blocking peek. `Nothing` while empty; `Just (Right a)` after
-- | success; `Just (Left v)` after typed failure.
poll
  :: forall r e e' a
   . Deferred e a
  -> RIO r e' (Maybe (Either (Variant e) a))
poll d = liftEffect do
  done <- _pollIsDone d
  if not done then pure Nothing
  else do
    ok <- _pollIsOk d
    if ok then do
      a <- _pollOk d
      pure (Just (Right a))
    else do
      v <- _pollFail d
      pure (Just (Left v))

-- | Non-blocking completion check.
isDone :: forall r e e' a. Deferred e a -> RIO r e' Boolean
isDone d = liftEffect (_pollIsDone d)
