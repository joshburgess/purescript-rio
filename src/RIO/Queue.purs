-- | An async FIFO queue.
-- |
-- | Producers call `offer` (always non-blocking on unbounded
-- | queues; blocks on a full bounded queue). Consumers call `take`,
-- | which blocks until an item is available or the queue is
-- | shut down. `shutdown` wakes every blocked consumer with a
-- | terminal `Nothing` so cleanup paths can run.
-- |
-- | This is the non-STM counterpart to `RIO.STM.TQueue`. Reach for
-- | this one for plain producer / consumer pipelines; reach for the
-- | STM version when the read or write needs to compose
-- | atomically with other transactional operations.
-- |
-- | Implementation: a `Ref` holds the queue body, the list of
-- | blocked takers (so an interrupted taker can remove itself
-- | cleanly), and an optional capacity. Each `take` registers a
-- | `makeAff` callback; each `offer` either delivers directly to
-- | a waiting taker or appends to the body.
module RIO.Queue
  ( Queue
  , bounded
  , offer
  , offerAll
  , poll
  , shutdown
  , size
  , take
  , takeAll
  , takeUpTo
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

import RIO.Internal (RIO(..))

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
    , isShutdown: false
    , nextTag: 0
    }
  pure (Queue ref)

-- | Current size. Advisory: producers and consumers may change it
-- | between the read and any subsequent action.
size :: forall a. Queue a -> Effect Int
size (Queue ref) = Array.length <<< _.items <$> Ref.read ref

-- | Non-blocking dequeue. `Nothing` when the queue is empty.
poll :: forall r e a. Queue a -> RIO r e (Maybe a)
poll (Queue ref) = RIO \_ -> liftEffect do
  state <- Ref.read ref
  case Array.uncons state.items of
    Nothing -> pure (Right Nothing)
    Just { head, tail } -> do
      Ref.write (state { items = tail }) ref
      wakeOfferer ref
      pure (Right (Just head))

-- | Block until a value is available or the queue is shut down.
-- | Returns `Nothing` if the queue is shut down and empty.
take :: forall r e a. Queue a -> RIO r e (Maybe a)
take (Queue ref) = RIO \_ -> do
  result <- takeAff ref
  pure (Right result)

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

-- | Enqueue a value. On unbounded queues this is non-blocking and
-- | always returns `true`. On bounded queues it blocks while the
-- | queue is at capacity, returning `false` only if the queue is
-- | shut down before the offer is accepted.
offer :: forall r e a. Queue a -> a -> RIO r e Boolean
offer (Queue ref) a = RIO \_ -> do
  result <- offerAff ref a
  pure (Right result)

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
        Just cap | Array.length state.items >= cap -> do
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
        _ -> do
          Ref.write (state { items = Array.snoc state.items a }) ref
          resume (Right true)
          pure nonCanceler

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
shutdown (Queue ref) = RIO \_ -> liftEffect do
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
  pure (Right unit)

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
