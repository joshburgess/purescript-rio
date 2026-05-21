-- | A broadcast channel.
-- |
-- | A `Hub a` fans out each published value to every active
-- | subscriber. Subscribers receive only messages published after
-- | they subscribe (cold subscription), and each subscriber buffers
-- | its own per-subscription queue so a slow consumer doesn't drop
-- | messages destined for a faster sibling. Publishing
-- | backpressures: a `publish` blocks until every subscriber's
-- | queue has room.
-- |
-- | Use `subscribeScoped` to tie a subscription's lifetime to a
-- | `Scope` so its slot is released automatically. Use `subscribe`
-- | when the caller owns the lifecycle explicitly.
module RIO.Fiber.Hub
  ( Hub
  , Subscription
  , make
  , makeBounded
  , publish
  , publishAll
  , tryPublish
  , publishDropNew
  , publishDropOld
  , shutdown
  , subscribe
  , subscribeScoped
  , unsubscribe
  , take
  , subscribers
  ) where

import Prelude

import Data.Array (filter, length, snoc)
import Data.Foldable (for_, all)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q
import RIO.Fiber.Scope (Scope, acquireRelease)

type SubState a = { id :: Int, queue :: Queue a }

type State a =
  { capacity :: Int
  , subs :: Array (SubState a)
  , nextId :: Int
  , isShutdown :: Boolean
  }

newtype Hub a = Hub (Ref (State a))

-- | A subscription handle. Each subscription has its own buffer; the
-- | publisher fans out into them, so slow consumers don't lose
-- | messages destined for fast siblings.
newtype Subscription a = Subscription
  { id :: Int
  , queue :: Queue a
  , hub :: Ref (State a)
  }

-- | Allocate a hub with the given per-subscriber buffer capacity.
make :: forall a. Int -> Effect (Hub a)
make cap = Hub <$> Ref.new
  { capacity: max 1 cap, subs: [], nextId: 0, isShutdown: false }

-- | Alias for `make` matching rio-aff's `makeBounded` naming.
-- | Provided so code ported from rio-aff reads the same.
makeBounded :: forall a. Int -> Effect (Hub a)
makeBounded = make

-- | Publish a value. Backpressures on every active subscriber: the
-- | call blocks until each subscriber's queue has accepted the
-- | message (in subscriber-id order).
publish :: forall r e a. Hub a -> a -> RIO r e Unit
publish (Hub ref) a = do
  st <- liftEffect (Ref.read ref)
  when (not st.isShutdown) do
    for_ st.subs \{ queue } -> Q.offer queue a

-- | Publish each value in turn. Equivalent to `traverse_ (publish hub)
-- | xs`, exposed as a named entry point for symmetry with rio-aff.
publishAll :: forall r e a. Hub a -> Array a -> RIO r e Unit
publishAll hub xs = for_ xs (publish hub)

-- | Non-blocking publish. Returns `true` only if *every* subscriber
-- | accepted the message; if any subscriber's queue was full the
-- | message is not delivered to anyone and the call returns `false`.
-- | (For per-subscriber drop semantics, build that on top of `Hub`
-- | + per-subscription `tryOffer`.)
tryPublish :: forall r e a. Hub a -> a -> RIO r e Boolean
tryPublish (Hub ref) a = do
  st <- liftEffect (Ref.read ref)
  if st.isShutdown then pure false
  else do
    results <- traverse (\{ queue } -> Q.tryOffer queue a) st.subs
    pure (all identity results)

-- | Publish, but drop the message for any subscriber whose queue is
-- | full instead of blocking. Other subscribers still receive it.
-- | Never suspends.
publishDropNew :: forall r e a. Hub a -> a -> RIO r e Unit
publishDropNew (Hub ref) a = do
  st <- liftEffect (Ref.read ref)
  when (not st.isShutdown) do
    for_ st.subs \{ queue } -> void (Q.tryOffer queue a)

-- | Publish, but if a subscriber's queue is full evict its oldest
-- | element and offer the new one. Never suspends; never drops the
-- | publisher's message (though it does drop a per-subscriber backlog
-- | element).
publishDropOld :: forall r e a. Hub a -> a -> RIO r e Unit
publishDropOld (Hub ref) a = do
  st <- liftEffect (Ref.read ref)
  when (not st.isShutdown) do
    for_ st.subs \{ queue } -> do
      accepted <- Q.tryOffer queue a
      when (not accepted) do
        _ <- Q.tryTake queue
        _ <- Q.tryOffer queue a
        pure unit

-- | Mark the hub as shut down. After `shutdown`, all `publish*`
-- | variants become no-ops (and `tryPublish` returns `false`).
-- |
-- | Note: this does not wake fibers already suspended in `take`,
-- | because rio-fiber's `Queue` has no shutdown primitive. Callers
-- | that need wake-up semantics should unsubscribe explicitly or
-- | scope subscriptions and close the surrounding `Scope`.
shutdown :: forall r e a. Hub a -> RIO r e Unit
shutdown (Hub ref) = liftEffect
  (Ref.modify_ (\s -> s { isShutdown = true }) ref)

-- | Subscribe. The returned `Subscription` must be released with
-- | `unsubscribe` (or use `subscribeScoped`).
subscribe :: forall r e a. Hub a -> RIO r e (Subscription a)
subscribe (Hub ref) = liftEffect do
  st <- Ref.read ref
  q <- Q.make st.capacity
  let
    id = st.nextId
    sub = { id, queue: q }
  Ref.write
    ( st
        { subs = snoc st.subs sub
        , nextId = id + 1
        }
    )
    ref
  pure (Subscription { id, queue: q, hub: ref })

-- | Subscribe and tie the subscription's lifetime to the given
-- | `Scope`. When the scope closes the slot is removed from the hub.
subscribeScoped
  :: forall r e a. Scope -> Hub a -> RIO r e (Subscription a)
subscribeScoped scope hub =
  acquireRelease scope (subscribe hub) unsubscribe

-- | Remove the subscription's slot from the hub. Any messages still
-- | buffered in this subscription's queue are dropped.
unsubscribe :: forall r e a. Subscription a -> RIO r e Unit
unsubscribe (Subscription { id, hub }) = liftEffect
  ( Ref.modify_
      (\s -> s { subs = filter (\sub -> sub.id /= id) s.subs })
      hub
  )

-- | Receive the next published value. Suspends until one arrives
-- | or until this subscription is unsubscribed (in which case the
-- | caller's fiber stays suspended; close before publish to drop).
take :: forall r e a. Subscription a -> RIO r e a
take (Subscription { queue }) = Q.take queue

-- | Current number of active subscriptions.
subscribers :: forall r e a. Hub a -> RIO r e Int
subscribers (Hub ref) = liftEffect (length <<< _.subs <$> Ref.read ref)
