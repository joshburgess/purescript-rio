-- | A write-once cell that lives inside STM, the transactional
-- | counterpart to `RIO.Aff.Deferred`.
-- |
-- | `RIO.Aff.Deferred` is built on `AVar`: it composes with `RIO` and
-- | wakes pending fibers when filled, but it cannot participate in
-- | a transaction alongside other shared state. `TDeferred` is the
-- | STM-flavoured version: `awaitTDeferred` retries the transaction
-- | until the cell is filled, and the fill primitives are STM
-- | actions that commit atomically with whatever other writes the
-- | transaction is making.
-- |
-- | The headline use is "handshake on filled cell *and* drain a
-- | queue in one atomically block":
-- |
-- | ```purescript
-- | -- wait until both the worker is ready and an item is queued
-- | item <- atomically do
-- |   _ <- awaitTDeferred ready
-- |   takeTQueue queue
-- | ```
-- |
-- | A `TDeferred e a` carries the same `Either (Variant e) a` shape
-- | as the `RIO` result channel, so `failTDeferred` raises a typed
-- | failure on the awaiter's row and `succeedTDeferred` produces a
-- | value on the success channel.
module RIO.Aff.STM.TDeferred
  ( TDeferred
  , awaitTDeferred
  , failTDeferred
  , makeTDeferred
  , pollTDeferred
  , succeedTDeferred
  , tryAwaitTDeferred
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)

import RIO.Aff.STM (STM, TRef, newTRef, readTRef, retry, throwSTM, writeTRef)

-- | A transactional write-once cell. Constructor hidden.
newtype TDeferred :: Row Type -> Type -> Type
newtype TDeferred e a = TDeferred (TRef (Maybe (Either (Variant e) a)))

-- | Create an empty `TDeferred`.
makeTDeferred :: forall e' e a. STM e' (TDeferred e a)
makeTDeferred = TDeferred <$> newTRef Nothing

-- | Fill the cell with a success value. Returns `True` if this
-- | call filled the cell; `False` if it was already filled (the
-- | existing value is preserved).
succeedTDeferred
  :: forall e' e a
   . TDeferred e a
  -> a
  -> STM e' Boolean
succeedTDeferred (TDeferred ref) value = do
  current <- readTRef ref
  case current of
    Just _ -> pure false
    Nothing -> do
      writeTRef ref (Just (Right value))
      pure true

-- | Fill the cell with a typed failure. Returns `True` on success,
-- | `False` if the cell was already filled.
failTDeferred
  :: forall e' e a
   . TDeferred e a
  -> Variant e
  -> STM e' Boolean
failTDeferred (TDeferred ref) v = do
  current <- readTRef ref
  case current of
    Just _ -> pure false
    Nothing -> do
      writeTRef ref (Just (Left v))
      pure true

-- | Wait until the cell is filled. If the fill was a success,
-- | return the value; if it was a failure, raise it on the STM
-- | error row.
-- |
-- | This is the STM-native handshake: combine with `readTRef` /
-- | `takeTQueue` / `awaitKey` / etc. inside one `atomically` block
-- | for atomic multi-source coordination.
awaitTDeferred :: forall e a. TDeferred e a -> STM e a
awaitTDeferred (TDeferred ref) = do
  current <- readTRef ref
  case current of
    Nothing -> retry
    Just (Right a) -> pure a
    Just (Left v) -> throwSTM v

-- | Non-blocking poll. Returns `Nothing` if empty, `Just (Left v)`
-- | if filled with a failure, `Just (Right a)` if filled with a
-- | success.
pollTDeferred
  :: forall e' e a
   . TDeferred e a
  -> STM e' (Maybe (Either (Variant e) a))
pollTDeferred (TDeferred ref) = readTRef ref

-- | Non-retrying variant of `awaitTDeferred`. Returns `Nothing`
-- | when the cell is empty (the transaction proceeds rather than
-- | retrying), `Just a` on success, and raises the typed failure
-- | when the cell holds one.
tryAwaitTDeferred :: forall e a. TDeferred e a -> STM e (Maybe a)
tryAwaitTDeferred (TDeferred ref) = do
  current <- readTRef ref
  case current of
    Nothing -> pure Nothing
    Just (Right a) -> pure (Just a)
    Just (Left v) -> throwSTM v
