-- | A transactional broadcast channel.
-- |
-- | A `TPubSub a` fans out each published value to every active
-- | subscriber, with each subscriber holding its own bounded
-- | `TQueue` buffer. Publish and subscribe are STM-typed, so a
-- | broadcast composes inside a larger transaction (e.g. "atomically
-- | move the cursor and notify watchers" or "publish only if some
-- | TVar guard holds").
-- |
-- | The pattern is the STM analogue of `RIO.Fiber.Hub`: each
-- | subscriber sees only messages published after it subscribes, and
-- | a slow consumer is isolated to its own queue. `publish`
-- | backpressures (retries) on the slowest subscriber when its queue
-- | is full; `tryPublish` writes to every subscriber queue that has
-- | room and returns `false` if any subscriber's queue was full (so a
-- | `false` result may still have delivered to some subscribers).
module RIO.Fiber.STM.TPubSub
  ( TPubSub
  , Subscription
  , make
  , publish
  , tryPublish
  , subscribe
  , unsubscribe
  , withSubscription
  , take
  , tryTake
  , subscribers
  , isEmptySubscription
  , lengthSubscription
  ) where

import Prelude

import Data.Array (filter, length, snoc)
import Data.Foldable (for_, all)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Effect (Effect)
import RIO.Fiber.Core (RIO, bracket)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TQueue (TQueue)
import RIO.Fiber.STM.TQueue as TQ

type SubState a = { id :: Int, queue :: TQueue a }

type State a =
  { capacity :: Int
  , subs :: Array (SubState a)
  , nextId :: Int
  }

-- | A transactional broadcast channel.
newtype TPubSub a = TPubSub (TVar (State a))

-- | A subscription handle. Each subscription has its own bounded
-- | buffer; publishers fan out into them, so a slow subscriber
-- | backpressures only itself (until its queue fills, at which
-- | point publish retries).
newtype Subscription a = Subscription
  { id :: Int
  , queue :: TQueue a
  , channel :: TVar (State a)
  }

-- | Allocate a pubsub with the given per-subscriber queue capacity.
make :: forall a. Int -> Effect (TPubSub a)
make cap = TPubSub <$> STM.newTVar
  { capacity: max 1 cap, subs: [], nextId: 0 }

-- | Broadcast a value to every active subscriber. Retries when any
-- | subscriber's queue is full, so the whole publish either reaches
-- | every subscriber or rolls back.
publish :: forall a. TPubSub a -> a -> STM Unit
publish (TPubSub tv) a = do
  st <- STM.readTVar tv
  for_ st.subs \{ queue } -> TQ.writeTQueue queue a

-- | Try to broadcast without retrying. Returns `true` only when every
-- | subscriber's queue had room. When `false`, the message may have
-- | landed in some subscribers' queues; callers who need all-or-
-- | nothing semantics should compose `publish` inside `orElse` with
-- | a fallback instead.
tryPublish :: forall a. TPubSub a -> a -> STM Boolean
tryPublish (TPubSub tv) a = do
  st <- STM.readTVar tv
  results <- traverse (\{ queue } -> TQ.tryWriteTQueue queue a) st.subs
  pure (all identity results)

-- | Subscribe. Returns a handle whose `take` reads the next value
-- | published after subscribe. The caller is responsible for
-- | `unsubscribe`-ing (or using a higher-level scoped wrapper).
subscribe :: forall a. TPubSub a -> STM (Subscription a)
subscribe (TPubSub tv) = do
  st <- STM.readTVar tv
  q <- TQ.newSTM st.capacity
  let
    id = st.nextId
    sub = { id, queue: q }
  STM.writeTVar tv
    ( st
        { subs = snoc st.subs sub
        , nextId = id + 1
        }
    )
  pure (Subscription { id, queue: q, channel: tv })

-- | Remove the subscription slot. Buffered values in this
-- | subscription's queue are dropped.
unsubscribe :: forall a. Subscription a -> STM Unit
unsubscribe (Subscription { id, channel }) = STM.modifyTVar channel
  (\s -> s { subs = filter (\sub -> sub.id /= id) s.subs })

-- | Read the next published value. Retries when no value is buffered.
take :: forall a. Subscription a -> STM a
take (Subscription { queue }) = TQ.readTQueue queue

-- | Try to read without retrying.
tryTake :: forall a. Subscription a -> STM (Maybe a)
tryTake (Subscription { queue }) = TQ.tryReadTQueue queue

-- | Current number of active subscribers.
subscribers :: forall a. TPubSub a -> STM Int
subscribers (TPubSub tv) = length <<< _.subs <$> STM.readTVar tv

-- | Whether this subscription's buffer is currently empty.
isEmptySubscription :: forall a. Subscription a -> STM Boolean
isEmptySubscription (Subscription { queue }) = TQ.isEmptyTQueue queue

-- | How many values are buffered for this subscriber but not yet
-- | taken.
lengthSubscription :: forall a. Subscription a -> STM Int
lengthSubscription (Subscription { queue }) = TQ.lengthTQueue queue

-- | Scoped subscribe / unsubscribe. The subscription is released on
-- | every termination path of `use` (success, typed failure, defect,
-- | interrupt). Preferred over a manual `subscribe`/`unsubscribe` pair.
withSubscription
  :: forall r e a b
   . TPubSub a
  -> (Subscription a -> RIO r e b)
  -> RIO r e b
withSubscription hub use =
  bracket
    (STM.atomically (subscribe hub))
    (\sub -> STM.atomically (unsubscribe sub))
    use
