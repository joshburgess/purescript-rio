-- | A transactional publish/subscribe hub built on top of `TRef`.
-- |
-- | A `THub a` distributes each published value to every active
-- | subscriber. Each subscriber receives values in publish order
-- | and consumes them at its own pace from a private per-subscriber
-- | buffer; subscribers are independent.
-- |
-- | The four constructors choose the back-pressure strategy:
-- |
-- |   - `newBoundedTHub n`: every subscriber buffer is capped at
-- |     `n` items. `publishTHub` retries (blocks) while *any*
-- |     subscriber is full. The slowest subscriber dictates
-- |     producer throughput.
-- |   - `newSlidingTHub n`: subscriber buffers are capped at `n`;
-- |     when a buffer is full the oldest entry is dropped to make
-- |     room. `publishTHub` never retries and always delivers (the
-- |     subscriber loses the oldest pending message, not the new
-- |     one).
-- |   - `newDroppingTHub n`: subscriber buffers are capped at `n`;
-- |     when a buffer is full the *new* message is dropped for
-- |     that subscriber. `publishTHub` never retries.
-- |     `publishTHub` returns `false` when any subscriber dropped
-- |     the message.
-- |   - `newUnboundedTHub`: no cap. Producer never blocks and
-- |     nothing is dropped. Susceptible to memory growth if a
-- |     subscriber stops consuming.
-- |
-- | `subscribeTHub` registers a fresh subscriber and returns a
-- | `Subscription` paired with the integer id used internally.
-- | `takeSubscription` retries when the subscription's private
-- | buffer is empty. `unsubscribeTHub` removes the subscription;
-- | values still buffered for it are dropped.
-- |
-- | For most code, `withSubscription` is preferable: it brackets
-- | subscribe / unsubscribe against an `RIO` action so the
-- | subscription is released on every termination path.
module RIO.STM.THub
  ( Strategy(..)
  , Subscription
  , THub
  , isEmptySubscription
  , lengthSubscription
  , newBoundedTHub
  , newDroppingTHub
  , newSlidingTHub
  , newTHub
  , newUnboundedTHub
  , publishTHub
  , subscribeTHub
  , subscriberCount
  , takeSubscription
  , tryTakeSubscription
  , unsubscribeTHub
  , withSubscription
  ) where

import Prelude

import Data.Array (drop, length, snoc, uncons) as Array
import Data.Foldable (foldM, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))

import RIO.Core (RIO, acquireRelease)
import RIO.STM (STM, TRef, atomically, modifyTRef, newTRef, readTRef, retry, writeTRef)

-- | Back-pressure policy. Set at construction time and immutable
-- | thereafter.
data Strategy
  = Bounded Int
  | Sliding Int
  | Dropping Int
  | Unbounded

derive instance eqStrategy :: Eq Strategy

instance showStrategy :: Show Strategy where
  show = case _ of
    Bounded n -> "Bounded " <> show n
    Sliding n -> "Sliding " <> show n
    Dropping n -> "Dropping " <> show n
    Unbounded -> "Unbounded"

-- | A pub/sub hub. Constructor hidden; identity is the underlying
-- | refs.
newtype THub a = THub
  { strategy :: Strategy
  , subscribers :: TRef (Map Int (TRef (Array a)))
  , nextId :: TRef Int
  }

-- | One subscriber's view of the hub. Carries the integer id used
-- | by `unsubscribeTHub` and a reference to the private buffer
-- | from which `takeSubscription` reads.
newtype Subscription a = Subscription
  { hub :: THub a
  , id :: Int
  , buffer :: TRef (Array a)
  }

-- | Allocate a hub with the given strategy.
newTHub :: forall e a. Strategy -> STM e (THub a)
newTHub strategy = do
  subscribers <- newTRef Map.empty
  nextId <- newTRef 0
  pure (THub { strategy, subscribers, nextId })

-- | `newTHub (Bounded n)`. Producer retries while any subscriber
-- | buffer holds `n` items.
newBoundedTHub :: forall e a. Int -> STM e (THub a)
newBoundedTHub n = newTHub (Bounded n)

-- | `newTHub (Sliding n)`. Producer never retries; the oldest
-- | buffered item is dropped to make room when a subscriber's
-- | buffer is full.
newSlidingTHub :: forall e a. Int -> STM e (THub a)
newSlidingTHub n = newTHub (Sliding n)

-- | `newTHub (Dropping n)`. Producer never retries; the new item
-- | is dropped for any subscriber whose buffer is full.
newDroppingTHub :: forall e a. Int -> STM e (THub a)
newDroppingTHub n = newTHub (Dropping n)

-- | `newTHub Unbounded`. Producer never retries and nothing is
-- | dropped. A slow subscriber will grow its buffer without bound.
newUnboundedTHub :: forall e a. STM e (THub a)
newUnboundedTHub = newTHub Unbounded

-- | Number of currently-active subscribers.
subscriberCount :: forall e a. THub a -> STM e Int
subscriberCount (THub h) = Map.size <$> readTRef h.subscribers

-- | Publish a value. Returns `true` if every active subscriber
-- | received the value; `false` if at least one dropped it.
-- |
-- | Behaviour by strategy:
-- |
-- |   - `Bounded n`: retries while any subscriber buffer is full;
-- |     returns `true` once every buffer accepted the value.
-- |   - `Sliding n`: always returns `true`; a subscriber whose
-- |     buffer was full loses its oldest entry.
-- |   - `Dropping n`: returns `true` only when every subscriber
-- |     accepted the value; subscribers whose buffer was full
-- |     drop the new value.
-- |   - `Unbounded`: always returns `true`.
publishTHub :: forall e a. THub a -> a -> STM e Boolean
publishTHub (THub h) a = do
  subs <- readTRef h.subscribers
  let buffers = Map.values subs
  case h.strategy of
    Bounded n -> do
      -- Atomicity is per-transaction, so checking each buffer
      -- and then writing is safe; if any retries, all writes
      -- in this transaction are rolled back.
      for_ buffers \buf -> do
        xs <- readTRef buf
        if Array.length xs >= n then retry
        else pure unit
      for_ buffers \buf -> modifyTRef buf (\xs -> Array.snoc xs a)
      pure true
    Sliding n -> do
      for_ buffers \buf -> do
        xs <- readTRef buf
        let xs' = if Array.length xs >= n then Array.drop 1 xs else xs
        writeTRef buf (Array.snoc xs' a)
      pure true
    Dropping n ->
      foldM
        ( \acc buf -> do
            xs <- readTRef buf
            if Array.length xs >= n then pure false
            else do
              writeTRef buf (Array.snoc xs a)
              pure acc
        )
        true
        buffers
    Unbounded -> do
      for_ buffers \buf -> modifyTRef buf (\xs -> Array.snoc xs a)
      pure true

-- | Register a new subscriber. The subscriber sees only values
-- | published *after* it registers; values that were already in
-- | flight are not delivered to it.
subscribeTHub :: forall e a. THub a -> STM e (Subscription a)
subscribeTHub hub@(THub h) = do
  buffer <- newTRef []
  rid <- readTRef h.nextId
  writeTRef h.nextId (rid + 1)
  modifyTRef h.subscribers (Map.insert rid buffer)
  pure (Subscription { hub, id: rid, buffer })

-- | Remove a subscription. Subsequent `publishTHub` calls do not
-- | deliver to it; any values still buffered for the subscriber
-- | are dropped.
unsubscribeTHub :: forall e a. Subscription a -> STM e Unit
unsubscribeTHub (Subscription s) =
  case s.hub of
    THub h -> modifyTRef h.subscribers (Map.delete s.id)

-- | Take the next value from this subscriber's buffer. Retries
-- | (waits) until a value is available.
takeSubscription :: forall e a. Subscription a -> STM e a
takeSubscription (Subscription s) = do
  xs <- readTRef s.buffer
  case Array.uncons xs of
    Nothing -> retry
    Just { head, tail } -> do
      writeTRef s.buffer tail
      pure head

-- | Non-blocking take. `Nothing` when the subscriber's buffer is
-- | empty.
tryTakeSubscription :: forall e a. Subscription a -> STM e (Maybe a)
tryTakeSubscription (Subscription s) = do
  xs <- readTRef s.buffer
  case Array.uncons xs of
    Nothing -> pure Nothing
    Just { head, tail } -> do
      writeTRef s.buffer tail
      pure (Just head)

-- | True when the subscriber's buffer is empty.
isEmptySubscription :: forall e a. Subscription a -> STM e Boolean
isEmptySubscription (Subscription s) = do
  xs <- readTRef s.buffer
  pure (Array.length xs == 0)

-- | Current number of values buffered for this subscriber.
lengthSubscription :: forall e a. Subscription a -> STM e Int
lengthSubscription (Subscription s) = Array.length <$> readTRef s.buffer

-- | Scoped subscribe / unsubscribe. The subscription is released
-- | on every termination path of `use` (success, typed failure,
-- | defect, interrupt). This is the preferred way to consume from
-- | a hub.
withSubscription
  :: forall r e a b
   . THub a
  -> (Subscription a -> RIO r e b)
  -> RIO r e b
withSubscription hub use =
  acquireRelease
    (atomically (subscribeTHub hub))
    (\sub -> atomically (unsubscribeTHub sub))
    use
