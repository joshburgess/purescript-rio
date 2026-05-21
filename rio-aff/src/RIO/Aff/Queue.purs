-- | An async FIFO queue.
-- |
-- | Producers call `offer` (always non-blocking on unbounded
-- | queues; blocks on a full bounded queue). Consumers call `take`,
-- | which blocks until an item is available or the queue is
-- | shut down. `shutdown` wakes every blocked consumer with a
-- | terminal `Nothing` so cleanup paths can run.
-- |
-- | This is the non-STM counterpart to `RIO.Aff.STM.TQueue`. Reach for
-- | this one for plain producer / consumer pipelines; reach for the
-- | STM version when the read or write needs to compose
-- | atomically with other transactional operations.
-- |
-- | Implementation: a `Ref` holds the queue body, the list of
-- | blocked takers (so an interrupted taker can remove itself
-- | cleanly), and an optional capacity. Each `take` registers a
-- | `makeAff` callback; each `offer` either delivers directly to
-- | a waiting taker or appends to the body.
module RIO.Aff.Queue
  ( Queue
  , Strategy(..)
  , bounded
  , capacity
  , dropping
  , offer
  , offerAll
  , poll
  , shutdown
  , size
  , sliding
  , strategy
  , take
  , takeAll
  , takeUpTo
  , tryOffer
  , tryTake
  , unbounded
  ) where

import Prelude

import Data.Array (filter, length, snoc, uncons) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Internal (RIO(..), mkEffectRIO, mkRIO)

-- | How the queue behaves when an `offer` arrives at a full
-- | bounded queue.
-- |
-- |   * `BackPressure`: offer suspends until a slot frees up.
-- |   * `Dropping`: the offered element is dropped; offer returns
-- |     immediately. `tryOffer` returns `false` to signal the drop.
-- |   * `Sliding`: the oldest stored element is evicted to make
-- |     room for the new one; offer always succeeds.
-- |
-- | Unbounded queues never reach a full state, so the strategy is
-- | not consulted; reading `strategy` on an unbounded queue still
-- | returns the value the constructor recorded (`BackPressure`).
data Strategy
  = BackPressure
  | Dropping
  | Sliding

derive instance eqStrategy :: Eq Strategy

-- | A blocked taker.
type Taker a =
  { tag :: Int
  , resume :: Maybe a -> Effect Unit
  }

-- | A blocked offerer (only created on bounded queues at capacity).
type Offerer a =
  { tag :: Int
  , value :: a
  , resume :: Boolean -> Effect Unit
  }

type State a =
  { items :: Array a
  , takers :: Array (Taker a)
  , offerers :: Array (Offerer a)
  , capacity :: Maybe Int
  , strategy :: Strategy
  , isShutdown :: Boolean
  , nextTag :: Int
  }

-- | A FIFO queue. Constructor hidden; create with `bounded` or
-- | `unbounded`.
newtype Queue a = Queue (Ref (State a))

-- | Allocate an unbounded queue. `offer` never blocks.
unbounded :: forall a. Effect (Queue a)
unbounded = do
  ref <- Ref.new
    { items: []
    , takers: []
    , offerers: []
    , capacity: Nothing
    , strategy: BackPressure
    , isShutdown: false
    , nextTag: 0
    }
  pure (Queue ref)

-- | Allocate a bounded queue with the given capacity. `offer`
-- | blocks while the body is at capacity; this gives backpressure
-- | for free.
bounded :: forall a. Int -> Effect (Queue a)
bounded n = do
  ref <- Ref.new
    { items: []
    , takers: []
    , offerers: []
    , capacity: Just (max 0 n)
    , strategy: BackPressure
    , isShutdown: false
    , nextTag: 0
    }
  pure (Queue ref)

-- | Allocate a bounded queue that drops new offers when full.
-- | `offer` always returns immediately; use `tryOffer` to learn
-- | whether the element was accepted or dropped.
dropping :: forall a. Int -> Effect (Queue a)
dropping n = do
  ref <- Ref.new
    { items: []
    , takers: []
    , offerers: []
    , capacity: Just (max 0 n)
    , strategy: Dropping
    , isShutdown: false
    , nextTag: 0
    }
  pure (Queue ref)

-- | Allocate a bounded queue that evicts the oldest stored
-- | element to make room when full. `offer` always returns
-- | immediately and is always accepted.
sliding :: forall a. Int -> Effect (Queue a)
sliding n = do
  ref <- Ref.new
    { items: []
    , takers: []
    , offerers: []
    , capacity: Just (max 0 n)
    , strategy: Sliding
    , isShutdown: false
    , nextTag: 0
    }
  pure (Queue ref)

-- | Read the offer strategy. Returned in `RIO` for symmetry with
-- | the other inspectors; advisory (does not block).
strategy :: forall r e a. Queue a -> RIO r e Strategy
strategy (Queue ref) = mkEffectRIO \_ -> _.strategy <$> Ref.read ref

-- | Read the configured capacity. `Nothing` for an `unbounded` queue;
-- | `Just n` for `bounded n`, `dropping n`, or `sliding n`. Advisory
-- | (does not block).
capacity :: forall r e a. Queue a -> RIO r e (Maybe Int)
capacity (Queue ref) = mkEffectRIO \_ -> _.capacity <$> Ref.read ref

-- | Current size. Advisory: producers and consumers may change it
-- | between the read and any subsequent action.
size :: forall a. Queue a -> Effect Int
size (Queue ref) = Array.length <<< _.items <$> Ref.read ref

-- | Non-blocking dequeue. `Nothing` when the queue is empty.
poll :: forall r e a. Queue a -> RIO r e (Maybe a)
poll (Queue ref) = mkEffectRIO \_ -> do
  state <- Ref.read ref
  case Array.uncons state.items of
    Nothing -> pure Nothing
    Just { head, tail } -> do
      Ref.write (state { items = tail }) ref
      wakeOfferer ref
      pure (Just head)

-- | Block until a value is available or the queue is shut down.
-- | Returns `Nothing` if the queue is shut down and empty.
take :: forall r e a. Queue a -> RIO r e (Maybe a)
take (Queue ref) = mkRIO \_ -> takeAff ref

takeAff :: forall a. Ref (State a) -> Aff (Maybe a)
takeAff ref = makeAff \resume -> do
  state <- Ref.read ref
  case Array.uncons state.items of
    Just { head, tail } -> do
      Ref.write (state { items = tail }) ref
      wakeOfferer ref
      resume (Right (Just head))
      pure nonCanceler
    Nothing ->
      if state.isShutdown then do
        resume (Right Nothing)
        pure nonCanceler
      else do
        let tag = state.nextTag
        let taker = { tag, resume: \m -> resume (Right m) }
        Ref.write
          ( state
              { nextTag = state.nextTag + 1
              , takers = Array.snoc state.takers taker
              }
          )
          ref
        pure
          ( Canceler \_ -> liftEffect do
              s' <- Ref.read ref
              Ref.write
                ( s'
                    { takers = Array.filter (\t -> t.tag /= tag)
                        s'.takers
                    }
                )
                ref
          )

-- | Enqueue a value. Return shape depends on the queue's strategy:
-- |
-- |   * Unbounded: non-blocking, always returns `true`.
-- |   * `BackPressure` (the default for `bounded`): blocks while
-- |     the queue is full, returns `true` once accepted, returns
-- |     `false` if the queue is shut down before the offer
-- |     completes.
-- |   * `Dropping`: never blocks; returns `true` if the element
-- |     made it into the queue, `false` if it was dropped because
-- |     the queue was full.
-- |   * `Sliding`: never blocks; always returns `true`. The
-- |     oldest stored element is evicted when full.
offer :: forall r e a. Queue a -> a -> RIO r e Boolean
offer (Queue ref) a = mkRIO \_ -> offerAff ref a

offerAff :: forall a. Ref (State a) -> a -> Aff Boolean
offerAff ref a = makeAff \resume -> do
  state <- Ref.read ref
  if state.isShutdown then do
    resume (Right false)
    pure nonCanceler
  else case Array.uncons state.takers of
    Just { head, tail } -> do
      Ref.write (state { takers = tail }) ref
      head.resume (Just a)
      resume (Right true)
      pure nonCanceler
    Nothing ->
      case state.capacity of
        Just cap | Array.length state.items >= cap ->
          case state.strategy of
            BackPressure -> do
              let tag = state.nextTag
              let
                offerer =
                  { tag
                  , value: a
                  , resume: \ok -> resume (Right ok)
                  }
              Ref.write
                ( state
                    { nextTag = state.nextTag + 1
                    , offerers = Array.snoc state.offerers offerer
                    }
                )
                ref
              pure
                ( Canceler \_ -> liftEffect do
                    s' <- Ref.read ref
                    Ref.write
                      ( s'
                          { offerers = Array.filter (\o -> o.tag /= tag)
                              s'.offerers
                          }
                      )
                      ref
                )
            Dropping -> do
              resume (Right false)
              pure nonCanceler
            Sliding -> do
              let
                newItems = case Array.uncons state.items of
                  Just { tail } -> Array.snoc tail a
                  Nothing -> Array.snoc state.items a
              Ref.write (state { items = newItems }) ref
              resume (Right true)
              pure nonCanceler
        _ -> do
          Ref.write (state { items = Array.snoc state.items a }) ref
          resume (Right true)
          pure nonCanceler

-- | Non-blocking enqueue. Returns `true` if the element was
-- | accepted (handed directly to a waiting taker, appended, or
-- | made room for via `Sliding`), `false` otherwise:
-- |
-- |   * Unbounded: always returns `true`.
-- |   * `BackPressure` at capacity: returns `false` without
-- |     enqueueing (since a real `offer` would block).
-- |   * `Dropping` at capacity: returns `false` (the value is
-- |     silently dropped).
-- |   * `Sliding` at capacity: evicts the oldest element, appends
-- |     the new one, returns `true`.
-- |   * Shutdown queues: returns `false`.
tryOffer :: forall r e a. Queue a -> a -> RIO r e Boolean
tryOffer (Queue ref) a = mkEffectRIO \_ -> do
  state <- Ref.read ref
  if state.isShutdown then pure false
  else case Array.uncons state.takers of
    Just { head, tail } -> do
      Ref.write (state { takers = tail }) ref
      head.resume (Just a)
      pure true
    Nothing ->
      case state.capacity of
        Just cap | Array.length state.items >= cap ->
          case state.strategy of
            BackPressure -> pure false
            Dropping -> pure false
            Sliding -> do
              let
                newItems = case Array.uncons state.items of
                  Just { tail } -> Array.snoc tail a
                  Nothing -> Array.snoc state.items a
              Ref.write (state { items = newItems }) ref
              pure true
        _ -> do
          Ref.write (state { items = Array.snoc state.items a }) ref
          pure true

-- | Non-blocking dequeue. An alias for `poll`, named to match the
-- | rio-fiber spelling. Returns `Nothing` if the queue is empty.
tryTake :: forall r e a. Queue a -> RIO r e (Maybe a)
tryTake = poll

-- | Offer every element of an array in order. On unbounded queues
-- | this is always non-blocking; on bounded queues it blocks behind
-- | backpressure exactly as if each element were offered individually.
-- |
-- | Returns the array of items that were *not* delivered because the
-- | queue was shut down mid-offer. An empty array means everything
-- | landed.
offerAll :: forall r e a. Queue a -> Array a -> RIO r e (Array a)
offerAll q xs = case Array.uncons xs of
  Nothing -> pure []
  Just { head, tail } -> do
    ok <- offer q head
    if ok then offerAll q tail
    else pure xs

-- | Drain everything currently buffered, without blocking. Returns
-- | them in FIFO order. Wakes one blocked offerer per item drained
-- | (so bounded-queue producers can refill the buffer).
-- |
-- | This is the "snapshot" companion to `take`: it never waits, so
-- | the returned array reflects exactly what was available at call
-- | time.
takeAll :: forall r e a. Queue a -> RIO r e (Array a)
takeAll q = go []
  where
  go acc = do
    item <- poll q
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
            item <- poll q
            case item of
              Nothing -> pure acc
              Just a -> go (Array.snoc acc a) (remaining - 1)

-- | Shut down the queue. Every blocked taker wakes with `Nothing`;
-- | every blocked offerer wakes with `false`. Subsequent `offer`s
-- | return `false` immediately; subsequent `take`s return `Nothing`
-- | once the existing buffer is drained.
shutdown :: forall r e a. Queue a -> RIO r e Unit
shutdown (Queue ref) = mkEffectRIO \_ -> do
  state <- Ref.read ref
  Ref.write
    ( state
        { takers = []
        , offerers = []
        , isShutdown = true
        }
    )
    ref
  for_ state.takers (\t -> t.resume Nothing)
  for_ state.offerers (\o -> o.resume false)

-- | After a take consumed a slot, see if a blocked offerer can
-- | now succeed.
wakeOfferer :: forall a. Ref (State a) -> Effect Unit
wakeOfferer ref = do
  state <- Ref.read ref
  case Array.uncons state.offerers of
    Nothing -> pure unit
    Just { head, tail } -> do
      Ref.write
        ( state
            { offerers = tail
            , items = Array.snoc state.items head.value
            }
        )
        ref
      head.resume true

for_ :: forall a. Array a -> (a -> Effect Unit) -> Effect Unit
for_ xs f = case Array.uncons xs of
  Nothing -> pure unit
  Just { head, tail } -> do
    f head
    for_ tail f
