-- | A broadcast pub/sub hub.
-- |
-- | A `Hub a` lets producers `publish` values and any number of
-- | subscribers each receive every published value in their own
-- | private `Queue`. Subscribers `subscribe` to obtain a queue
-- | and `unsubscribe` to drop it; values published while a
-- | subscriber is alive land in its queue.
-- |
-- | Implementation: the hub holds a `Ref` of subscriber queues.
-- | `publish` walks the list and `offer`s the value to each.
-- | Subscribers each get an unbounded queue so a slow consumer
-- | does not slow the producer down; the natural tradeoff is that
-- | a slow consumer can fall arbitrarily far behind. If you need
-- | backpressure, use `bounded` to allocate subscriber queues by
-- | hand and skip the hub.
-- |
-- | This is the non-STM counterpart to `RIO.STM.THub`. Reach for
-- | this one for simple broadcast; reach for the STM version when
-- | publication needs to compose atomically with other
-- | transactional operations.
module RIO.Hub
  ( Hub
  , make
  , publish
  , publishAll
  , shutdown
  , subscribe
  , subscriberCount
  , unsubscribe
  ) where

import Prelude

import Data.Array (filter, length, snoc) as Array
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Internal (RIO(..), unRIO)
import RIO.Queue (Queue)
import RIO.Queue as Queue

-- | A single subscription, tagged so it can be removed.
type Subscriber a =
  { tag :: Int
  , queue :: Queue a
  }

type State a =
  { subscribers :: Array (Subscriber a)
  , nextTag :: Int
  , isShutdown :: Boolean
  }

-- | A broadcast hub.
newtype Hub a = Hub (Ref (State a))

-- | Allocate a fresh hub with no subscribers.
make :: forall a. Effect (Hub a)
make = do
  ref <- Ref.new { subscribers: [], nextTag: 0, isShutdown: false }
  pure (Hub ref)

-- | Subscribe a new consumer. Returns a queue that receives every
-- | subsequent `publish`. The queue is unbounded; if you need
-- | backpressure, drop the hub and wire `Queue.bounded` queues
-- | directly.
-- |
-- | Returns `{ queue, unsubscribe }`. Call `unsubscribe` (or use
-- | the `unsubscribe` smart constructor below) to remove the
-- | subscription when you are done.
subscribe
  :: forall r e a
   . Hub a
  -> RIO r e { queue :: Queue a, unsubscribe :: RIO r e Unit }
subscribe (Hub ref) = RIO \r -> do
  queue <- liftEffect Queue.unbounded
  state <- liftEffect (Ref.read ref)
  let tag = state.nextTag
  liftEffect
    ( Ref.write
        ( state
            { nextTag = state.nextTag + 1
            , subscribers = Array.snoc state.subscribers
                { tag, queue }
            }
        )
        ref
    )
  -- A hub that has already been shut down still hands out a queue
  -- (so the call site does not have to special-case post-shutdown
  -- subscribes), but the queue is immediately shut down so the
  -- subscriber's first `take` returns `Nothing`.
  when state.isShutdown do
    _ <- unRIO (Queue.shutdown queue :: RIO _ () Unit) r
    pure unit
  let
    unsub :: forall r' e'. RIO r' e' Unit
    unsub = RIO \_ -> liftEffect do
      s' <- Ref.read ref
      Ref.write
        ( s'
            { subscribers = Array.filter (\sub -> sub.tag /= tag)
                s'.subscribers
            }
        )
        ref
      pure (Right unit)
  pure (Right { queue, unsubscribe: unsub })

-- | Remove a previously-acquired subscription by passing the
-- | `unsubscribe` returned from `subscribe`. Equivalent to running
-- | that action directly; provided for readability.
unsubscribe :: forall r e. RIO r e Unit -> RIO r e Unit
unsubscribe action = action

-- | Publish a value to every current subscriber. Each subscriber
-- | observes the value through its own queue (so a slow consumer
-- | cannot delay other consumers).
publish :: forall r e a. Hub a -> a -> RIO r e Unit
publish (Hub ref) a = RIO \r -> do
  state <- liftEffect (Ref.read ref)
  traverse_
    ( \sub -> do
        _ <- unRIO (Queue.offer sub.queue a :: RIO _ _ Boolean) r
        pure unit
    )
    state.subscribers
  pure (Right unit)

-- | Publish a batch of values in input order.
publishAll :: forall r e a. Hub a -> Array a -> RIO r e Unit
publishAll hub xs = for_ xs (publish hub)

-- | How many subscribers are currently attached. Advisory: can
-- | change concurrently.
subscriberCount :: forall a. Hub a -> Effect Int
subscriberCount (Hub ref) =
  Array.length <<< _.subscribers <$> Ref.read ref

-- | Shut down the hub.
-- |
-- | Marks the hub as shut down and walks every current subscriber's
-- | queue, calling `Queue.shutdown` on each so any consumer blocked
-- | on `take` wakes up with `Nothing`. Subsequent calls to
-- | `subscribe` still hand out a queue (so callers do not need a
-- | special case), but the new queue is shut down immediately, so
-- | the new subscriber's first `take` returns `Nothing`.
-- |
-- | Idempotent: a second `shutdown` after the first is a no-op.
-- |
-- | Use this when the upstream producer that feeds the hub has
-- | finished (or failed) and the broadcast should terminate every
-- | subscriber cleanly. The publisher should call `shutdown` *after*
-- | every published value has been offered, otherwise the in-flight
-- | tail of the stream is lost.
shutdown :: forall r e a. Hub a -> RIO r e Unit
shutdown (Hub ref) = RIO \r -> do
  state <- liftEffect (Ref.read ref)
  liftEffect (Ref.write (state { isShutdown = true }) ref)
  traverse_
    ( \sub -> do
        _ <- unRIO (Queue.shutdown sub.queue :: RIO _ () Unit) r
        pure unit
    )
    state.subscribers
  pure (Right unit)
