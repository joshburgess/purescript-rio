-- | A bounded asynchronous FIFO queue.
-- |
-- | `offer` enqueues an element, suspending if the queue is at
-- | capacity. `take` dequeues, suspending if empty. Both fairly
-- | unblock the longest-waiting fiber when room (or an item)
-- | appears.
-- |
-- | Cancelling an awaiting offer or take drops the request from the
-- | corresponding queue cleanly so no element is delivered into a
-- | stale fiber and no slot is silently consumed.
module RIO.Fiber.Queue
  ( Queue
  , make
  , offer
  , take
  , tryOffer
  , tryTake
  , size
  , capacity
  ) where

import Prelude

import Data.Array (filter, length, snoc, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, async, liftEffect)

type Offerer a = { id :: Int, value :: a, fire :: Effect Unit }
type Taker a = { id :: Int, deliver :: a -> Effect Unit }

type State a =
  { capacity :: Int
  , items :: Array a
  , offerers :: Array (Offerer a)
  , takers :: Array (Taker a)
  , nextId :: Int
  }

newtype Queue a = Queue (Ref (State a))

-- | Allocate a queue with the given positive capacity. A non-positive
-- | input is clamped to 1.
make :: forall a. Int -> Effect (Queue a)
make cap = Queue <$> Ref.new
  { capacity: max 1 cap
  , items: []
  , offerers: []
  , takers: []
  , nextId: 0
  }

-- | Enqueue an element. If a taker is already waiting it receives
-- | the element directly; if the queue has room it is appended; else
-- | the offering fiber suspends until a taker arrives.
offer :: forall r e a. Queue a -> a -> RIO r e Unit
offer (Queue ref) a = async \cb -> do
  st <- Ref.read ref
  case uncons st.takers of
    Just { head: t, tail: trest } -> do
      Ref.write (st { takers = trest }) ref
      t.deliver a
      cb (Right unit)
      pure (pure unit)
    Nothing
      | length st.items < st.capacity -> do
          Ref.write (st { items = snoc st.items a }) ref
          cb (Right unit)
          pure (pure unit)
      | otherwise -> do
          let
            id = st.nextId
            offerer = { id, value: a, fire: cb (Right unit) }
          Ref.write
            ( st
                { offerers = snoc st.offerers offerer
                , nextId = id + 1
                }
            )
            ref
          pure
            ( Ref.modify_
                ( \s -> s
                    { offerers = filter (\o -> o.id /= id) s.offerers
                    }
                )
                ref
            )

-- | Dequeue the next element. If the queue is empty the fiber
-- | suspends until an `offer` arrives.
take :: forall r e a. Queue a -> RIO r e a
take (Queue ref) = async \cb -> do
  st <- Ref.read ref
  case uncons st.items of
    Just { head: x, tail: rest } -> do
      let
        promoted = uncons st.offerers
        newItems = case promoted of
          Just { head: o } -> snoc rest o.value
          Nothing -> rest
        newOfferers = case promoted of
          Just { tail: orest } -> orest
          Nothing -> st.offerers
      Ref.write (st { items = newItems, offerers = newOfferers }) ref
      case promoted of
        Just { head: o } -> o.fire
        Nothing -> pure unit
      cb (Right x)
      pure (pure unit)
    Nothing -> do
      let
        id = st.nextId
        taker = { id, deliver: \v -> cb (Right v) }
      Ref.write
        ( st
            { takers = snoc st.takers taker
            , nextId = id + 1
            }
        )
        ref
      pure
        ( Ref.modify_
            (\s -> s { takers = filter (\t -> t.id /= id) s.takers })
            ref
        )

-- | Non-blocking enqueue. Returns `true` if the element was accepted
-- | (handed to a taker or appended), `false` if the queue was full
-- | with no taker waiting.
tryOffer :: forall r e a. Queue a -> a -> RIO r e Boolean
tryOffer (Queue ref) a = liftEffect do
  st <- Ref.read ref
  case uncons st.takers of
    Just { head: t, tail: trest } -> do
      Ref.write (st { takers = trest }) ref
      t.deliver a
      pure true
    Nothing
      | length st.items < st.capacity -> do
          Ref.write (st { items = snoc st.items a }) ref
          pure true
      | otherwise -> pure false

-- | Non-blocking dequeue. Returns `Nothing` if the queue is empty.
tryTake :: forall r e a. Queue a -> RIO r e (Maybe a)
tryTake (Queue ref) = liftEffect do
  st <- Ref.read ref
  case uncons st.items of
    Just { head: x, tail: rest } -> do
      let
        promoted = uncons st.offerers
        newItems = case promoted of
          Just { head: o } -> snoc rest o.value
          Nothing -> rest
        newOfferers = case promoted of
          Just { tail: orest } -> orest
          Nothing -> st.offerers
      Ref.write (st { items = newItems, offerers = newOfferers }) ref
      case promoted of
        Just { head: o } -> o.fire
        Nothing -> pure unit
      pure (Just x)
    Nothing -> pure Nothing

-- | Current item count.
size :: forall r e a. Queue a -> RIO r e Int
size (Queue ref) = liftEffect (length <<< _.items <$> Ref.read ref)

-- | Configured capacity.
capacity :: forall r e a. Queue a -> RIO r e Int
capacity (Queue ref) = liftEffect (_.capacity <$> Ref.read ref)
