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
-- | This is the non-STM counterpart to `RIO.Aff.STM.THub`. Reach for
-- | this one for simple broadcast; reach for the STM version when
-- | publication needs to compose atomically with other
-- | transactional operations.
module RIO.Aff.Hub
  ( Hub
  , Subscription
  , make
  , makeBounded
  , publish
  , publishAll
  , publishDropNew
  , publishDropOld
  , shutdown
  , subscribe
  , subscribeScoped
  , subscriberCount
  , subscribers
  , take
  , tryPublish
  , unsubscribe
  ) where

import Prelude

import Data.Array (filter, length, snoc, uncons) as Array
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Internal (RIO(..), mkEffectRIO, mkRIO, unsafeUnRIO)
import RIO.Aff.Queue (Queue)
import RIO.Aff.Queue as Queue
import RIO.Aff.Resource (Scope, addFinalizer)

-- | A single subscription, tagged so it can be removed.
type Subscriber a =
  { tag :: Int
  , queue :: Queue a
  }

type State a =
  { subscribers :: Array (Subscriber a)
  , nextTag :: Int
  , isShutdown :: Boolean
  , subCapacity :: Maybe Int
  }

-- | A broadcast hub.
newtype Hub a = Hub (Ref (State a))

-- | Allocate a fresh hub with no subscribers. Each subscriber
-- | gets an unbounded private queue, so a slow consumer cannot
-- | apply backpressure on the publisher (the natural tradeoff is
-- | unbounded memory for a stalled subscriber).
-- |
-- | For drop-on-full semantics, reach for `makeBounded`; the
-- | `tryPublish` / `publishDropNew` / `publishDropOld` helpers
-- | only have non-trivial behaviour against bounded subscriber
-- | queues.
make :: forall a. Effect (Hub a)
make = do
  ref <- Ref.new
    { subscribers: []
    , nextTag: 0
    , isShutdown: false
    , subCapacity: Nothing
    }
  pure (Hub ref)

-- | Allocate a hub whose subscribers each get a *bounded*
-- | back-pressure queue of the given capacity. `publish` blocks
-- | when any subscriber's queue is full; reach for `tryPublish`,
-- | `publishDropNew`, or `publishDropOld` for non-blocking
-- | broadcast with explicit drop policies.
makeBounded :: forall a. Int -> Effect (Hub a)
makeBounded cap = do
  ref <- Ref.new
    { subscribers: []
    , nextTag: 0
    , isShutdown: false
    , subCapacity: Just (max 1 cap)
    }
  pure (Hub ref)

-- | A handle returned by `subscribe`. Holds the subscriber's queue
-- | and the action that removes it from the hub. Exposed as a
-- | named record so call sites can type the result without
-- | inlining the row.
type Subscription r e a =
  { queue :: Queue a
  , unsubscribe :: RIO r e Unit
  }

-- | Subscribe a new consumer. Returns a queue that receives every
-- | subsequent `publish`. The queue is unbounded; if you need
-- | backpressure, drop the hub and wire `Queue.bounded` queues
-- | directly.
-- |
-- | Returns a `Subscription`: a record holding the queue and the
-- | `unsubscribe` action. Call `unsubscribe` (or use the
-- | `unsubscribe` smart constructor below) to remove the
-- | subscription when you are done; for scope-managed teardown
-- | reach for `subscribeScoped` instead.
subscribe
  :: forall r e a
   . Hub a
  -> RIO r e (Subscription r e a)
subscribe (Hub ref) = mkRIO \r -> do
  state0 <- liftEffect (Ref.read ref)
  queue <- liftEffect case state0.subCapacity of
    Nothing -> Queue.unbounded
    Just cap -> Queue.bounded cap
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
    _ <- unsafeUnRIO (Queue.shutdown queue :: RIO _ () Unit) r
    pure unit
  let
    unsub :: forall r' e'. RIO r' e' Unit
    unsub = mkEffectRIO \_ -> do
      s' <- Ref.read ref
      Ref.write
        ( s'
            { subscribers = Array.filter (\sub -> sub.tag /= tag)
                s'.subscribers
            }
        )
        ref
  pure { queue, unsubscribe: unsub }

-- | Remove a previously-acquired subscription by passing the
-- | `unsubscribe` returned from `subscribe`. Equivalent to running
-- | that action directly; provided for readability.
unsubscribe :: forall r e. RIO r e Unit -> RIO r e Unit
unsubscribe action = action

-- | Subscribe and register `unsubscribe` as a finalizer on the
-- | given `Scope`. The returned `Queue` is live until the scope
-- | exits; on exit the subscription is removed automatically, on
-- | every termination path.
-- |
-- | This is the resource-safe form of `subscribe`. Use it inside a
-- | `scoped` block when you want the subscription's lifetime to
-- | match the scope's.
subscribeScoped
  :: forall r e a
   . Scope
  -> Hub a
  -> RIO r e (Queue a)
subscribeScoped scope hub = mkRIO \r -> do
  sub <- unsafeUnRIO (subscribe hub) r
  let unsubAff = unsafeUnRIO sub.unsubscribe r
  unsafeUnRIO (addFinalizer scope unsubAff) r
  pure sub.queue

-- | Publish a value to every current subscriber. Each subscriber
-- | observes the value through its own queue (so a slow consumer
-- | cannot delay other consumers).
publish :: forall r e a. Hub a -> a -> RIO r e Unit
publish (Hub ref) a = mkRIO \r -> do
  state <- liftEffect (Ref.read ref)
  traverse_
    ( \sub -> do
        _ <- unsafeUnRIO (Queue.offer sub.queue a :: RIO _ _ Boolean) r
        pure unit
    )
    state.subscribers

-- | Publish a batch of values in input order.
publishAll :: forall r e a. Hub a -> Array a -> RIO r e Unit
publishAll hub xs = for_ xs (publish hub)

-- | Non-blocking publish. Returns `true` only if *every*
-- | subscriber accepted the message; if any subscriber's queue
-- | was full the call returns `false` and that subscriber misses
-- | the message (the others still receive it, since fan-out is
-- | per-subscriber rather than transactional).
-- |
-- | On a `make`-built hub (unbounded subscriber queues) this
-- | always returns `true`; it is meaningful only on a
-- | `makeBounded` hub.
tryPublish :: forall r e a. Hub a -> a -> RIO r e Boolean
tryPublish (Hub ref) a = mkRIO \r -> do
  state <- liftEffect (Ref.read ref)
  goAll true state.subscribers r
  where
  goAll acc subs r = case Array.uncons subs of
    Nothing -> pure acc
    Just { head, tail } -> do
      ok <- unsafeUnRIO
        (Queue.tryOffer head.queue a :: RIO _ _ Boolean)
        r
      goAll (acc && ok) tail r

-- | Publish, dropping the message for any subscriber whose queue
-- | is full. Never suspends. Other subscribers still receive it.
-- |
-- | Equivalent to `tryPublish` with the boolean result discarded;
-- | exposed as a separate name so the intent ("drop, do not
-- | block") is visible at the call site.
publishDropNew :: forall r e a. Hub a -> a -> RIO r e Unit
publishDropNew hub a = do
  _ <- tryPublish hub a
  pure unit

-- | Publish, but if a subscriber's queue is full, evict the
-- | subscriber's oldest buffered element and offer the new one.
-- | Never suspends; the publisher's message is delivered to every
-- | subscriber (possibly displacing a per-subscriber backlog
-- | element).
-- |
-- | On an unbounded `make` hub the eviction branch never fires
-- | and the call is equivalent to `publish`.
publishDropOld :: forall r e a. Hub a -> a -> RIO r e Unit
publishDropOld (Hub ref) a = mkRIO \r -> do
  state <- liftEffect (Ref.read ref)
  traverse_
    ( \sub -> do
        accepted <- unsafeUnRIO
          (Queue.tryOffer sub.queue a :: RIO _ _ Boolean)
          r
        when (not accepted) do
          _ <- unsafeUnRIO
            (Queue.tryTake sub.queue :: RIO _ _ (Maybe a))
            r
          _ <- unsafeUnRIO
            (Queue.tryOffer sub.queue a :: RIO _ _ Boolean)
            r
          pure unit
    )
    state.subscribers

-- | How many subscribers are currently attached. Advisory: can
-- | change concurrently.
subscriberCount :: forall a. Hub a -> Effect Int
subscriberCount (Hub ref) =
  Array.length <<< _.subscribers <$> Ref.read ref

-- | `subscriberCount` lifted into `RIO`. Same advisory caveat
-- | applies.
subscribers :: forall r e a. Hub a -> RIO r e Int
subscribers hub = liftEffect (subscriberCount hub)

-- | Read the next value from this subscription's queue. Blocks
-- | (suspends the fiber) until a value arrives, the queue is shut
-- | down (returns the queue's `take` behaviour on shutdown), or the
-- | subscription is unsubscribed. Thin wrapper over `Queue.take`
-- | applied to the subscription's queue; pull it apart to use other
-- | `Queue` combinators (`takeAll`, `tryTake`, `poll`).
take :: forall r e a. Subscription r e a -> RIO r e (Maybe a)
take sub = Queue.take sub.queue

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
shutdown (Hub ref) = mkRIO \r -> do
  state <- liftEffect (Ref.read ref)
  liftEffect (Ref.write (state { isShutdown = true }) ref)
  traverse_
    ( \sub -> do
        _ <- unsafeUnRIO (Queue.shutdown sub.queue :: RIO _ () Unit) r
        pure unit
    )
    state.subscribers
