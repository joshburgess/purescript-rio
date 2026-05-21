-- | A bounded asynchronous FIFO queue with four offer strategies.
-- |
-- | `make` is the default: bounded with backpressure. Offering to a
-- | full queue suspends the offering fiber until a slot frees up.
-- | The other three constructors trade backpressure for different
-- | full-queue behaviour:
-- |
-- |   * `unbounded` accepts every offer without a capacity check;
-- |     offers never suspend.
-- |   * `dropping cap` drops the offered element when the queue is
-- |     full; offers never suspend, but the new element is silently
-- |     discarded. Use `tryOffer` to learn whether an offer was kept.
-- |   * `sliding cap` makes room by dropping the oldest stored
-- |     element, then appends the new one; offers never suspend and
-- |     are always accepted.
-- |
-- | `take` is the same across all four: dequeue the next element,
-- | suspending if empty. Cancelling an awaiting offer or take drops
-- | the request from the corresponding queue cleanly.
module RIO.Fiber.Queue
  ( Queue
  , Strategy(..)
  , make
  , unbounded
  , dropping
  , sliding
  , offer
  , offerAll
  , take
  , takeAll
  , takeUpTo
  , tryOffer
  , tryTake
  , size
  , capacity
  , strategy
  ) where

import Prelude

import Data.Array (filter, length, snoc, uncons)
import Data.Array (snoc) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, async, liftEffect)

-- | How a queue behaves when an offer arrives at a full queue.
data Strategy
  = BackPressure
  | Dropping
  | Sliding
  | Unbounded

derive instance eqStrategy :: Eq Strategy

type Offerer a = { id :: Int, value :: a, fire :: Effect Unit }
type Taker a = { id :: Int, deliver :: a -> Effect Unit }

type State a =
  { capacity :: Int
  , strategy :: Strategy
  , items :: Array a
  , offerers :: Array (Offerer a)
  , takers :: Array (Taker a)
  , nextId :: Int
  }

newtype Queue a = Queue (Ref (State a))

mkQueue :: forall a. Strategy -> Int -> Effect (Queue a)
mkQueue strat cap = Queue <$> Ref.new
  { capacity: max 1 cap
  , strategy: strat
  , items: []
  , offerers: []
  , takers: []
  , nextId: 0
  }

-- | Allocate a bounded queue with backpressure. Offers to a full
-- | queue suspend until a slot opens. A non-positive capacity is
-- | clamped to 1.
make :: forall a. Int -> Effect (Queue a)
make = mkQueue BackPressure

-- | Allocate an unbounded queue. Offers never suspend and never drop
-- | elements. The reported `capacity` is the sentinel `top` value
-- | but is never enforced.
unbounded :: forall a. Effect (Queue a)
unbounded = mkQueue Unbounded top

-- | Allocate a bounded queue that drops the offered element when
-- | full. Offers always return immediately. Use `tryOffer` to learn
-- | whether the offer was accepted or dropped.
dropping :: forall a. Int -> Effect (Queue a)
dropping = mkQueue Dropping

-- | Allocate a bounded queue that drops the oldest stored element
-- | to make room for a new offer. Offers always return immediately
-- | and are always accepted.
sliding :: forall a. Int -> Effect (Queue a)
sliding = mkQueue Sliding

-- | Enqueue an element. Behaviour depends on the queue strategy:
-- |
-- |   * `BackPressure`: suspends if at capacity.
-- |   * `Dropping`: drops the element if at capacity.
-- |   * `Sliding`: drops the oldest stored element to make room.
-- |   * `Unbounded`: always appends.
-- |
-- | If a taker is already waiting it always receives the element
-- | directly, regardless of strategy.
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
      | st.strategy == Unbounded || length st.items < st.capacity -> do
          Ref.write (st { items = snoc st.items a }) ref
          cb (Right unit)
          pure (pure unit)
      | otherwise -> case st.strategy of
          BackPressure -> do
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
          Dropping -> do
            -- Drop the offered element; succeed immediately.
            cb (Right unit)
            pure (pure unit)
          Sliding -> do
            -- Drop the oldest stored element, append the new one.
            let
              newItems = case uncons st.items of
                Just { tail } -> snoc tail a
                Nothing -> snoc st.items a
            Ref.write (st { items = newItems }) ref
            cb (Right unit)
            pure (pure unit)
          Unbounded -> do
            -- Unreachable: handled in the guard above.
            Ref.write (st { items = snoc st.items a }) ref
            cb (Right unit)
            pure (pure unit)

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
-- | (handed to a taker, appended, or made room for via Sliding),
-- | `false` if the offer was dropped because the queue is full
-- | (Dropping) or if it would have to suspend (BackPressure).
tryOffer :: forall r e a. Queue a -> a -> RIO r e Boolean
tryOffer (Queue ref) a = liftEffect do
  st <- Ref.read ref
  case uncons st.takers of
    Just { head: t, tail: trest } -> do
      Ref.write (st { takers = trest }) ref
      t.deliver a
      pure true
    Nothing
      | st.strategy == Unbounded || length st.items < st.capacity -> do
          Ref.write (st { items = snoc st.items a }) ref
          pure true
      | otherwise -> case st.strategy of
          BackPressure -> pure false
          Dropping -> pure false
          Sliding -> do
            let
              newItems = case uncons st.items of
                Just { tail } -> snoc tail a
                Nothing -> snoc st.items a
            Ref.write (st { items = newItems }) ref
            pure true
          Unbounded -> do
            Ref.write (st { items = snoc st.items a }) ref
            pure true

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

-- | Offer each element in turn. Equivalent to `for_ xs (offer q)`,
-- | exposed as a named entry point for symmetry with rio-aff.
-- |
-- | Backpressure applies per element: on a bounded queue this call
-- | suspends once the buffer fills, resumes as slots free up, and
-- | returns when the last element has been accepted.
offerAll :: forall r e a. Queue a -> Array a -> RIO r e Unit
offerAll q xs = for_ xs (offer q)

-- | Drain everything currently buffered, without blocking. Returns
-- | the items in FIFO order. Wakes one blocked offerer per item
-- | drained (so bounded-queue producers can refill the buffer).
takeAll :: forall r e a. Queue a -> RIO r e (Array a)
takeAll q = go []
  where
  go acc = do
    item <- tryTake q
    case item of
      Nothing -> pure acc
      Just a -> go (Array.snoc acc a)

-- | Drain up to `n` items, non-blocking. Returns fewer than `n` if
-- | the queue runs dry before the cap. `n <= 0` yields an empty
-- | array immediately.
takeUpTo :: forall r e a. Queue a -> Int -> RIO r e (Array a)
takeUpTo q n
  | n <= 0 = pure []
  | otherwise = go [] n
      where
      go acc remaining
        | remaining <= 0 = pure acc
        | otherwise = do
            item <- tryTake q
            case item of
              Nothing -> pure acc
              Just a -> go (Array.snoc acc a) (remaining - 1)

-- | Current item count.
size :: forall r e a. Queue a -> RIO r e Int
size (Queue ref) = liftEffect (length <<< _.items <$> Ref.read ref)

-- | Configured capacity. For an `Unbounded` queue this is the
-- | sentinel `top` value (`Int` upper bound) and is never enforced.
capacity :: forall r e a. Queue a -> RIO r e Int
capacity (Queue ref) = liftEffect (_.capacity <$> Ref.read ref)

-- | Read the offer strategy.
strategy :: forall r e a. Queue a -> RIO r e Strategy
strategy (Queue ref) = liftEffect (_.strategy <$> Ref.read ref)
