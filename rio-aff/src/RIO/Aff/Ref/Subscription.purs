-- | A `Ref` that publishes its current value and every subsequent
-- | change as a `Stream`.
-- |
-- | A `SubscriptionRef` is a `SynchronizedRef`-style cell paired
-- | with a `Hub`. Every write commits to the underlying ref and then
-- | broadcasts the new value to every active subscriber. The
-- | `changes` stream emits the current value once and then every
-- | future write, with no possibility of missing or duplicating an
-- | update: subscribe and snapshot happen under the same lock, so a
-- | concurrent write either lands in the snapshot or in the hub,
-- | but never in both. Effect-ts, ZIO and FS2's `SignallingRef`/
-- | `SubscriptionRef` are the same pattern.
-- |
-- | ```purescript
-- | program = do
-- |   cfg <- Sub.make defaultConfig
-- |   _ <- fork (Sub.set cfg newConfig)
-- |   scoped do
-- |     scope <- ask (Proxy :: Proxy "scope")
-- |     Sub.changes scope cfg
-- |       # S.tap renderConfig
-- |       # S.take 2
-- |       # S.run
-- | ```
module RIO.Aff.Ref.Subscription
  ( SubscriptionRef
  , make
  , makeEffect
  , read
  , set
  , modify
  , modifyM
  , modifyM_
  , update
  , updateM
  , changes
  , subscribers
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Core (RIO)
import RIO.Aff.Hub (Hub)
import RIO.Aff.Hub as Hub
import RIO.Aff.Internal (mkRIO, unsafeUnRIO)
import RIO.Aff.Queue (Queue)
import RIO.Aff.Queue as Queue
import RIO.Aff.Resource (Scope, addFinalizer)
import RIO.Aff.Semaphore (Semaphore)
import RIO.Aff.Semaphore as Semaphore
import RIO.Aff.Stream (Stream(..), Step(..))

-- | A `Ref a` whose updates are broadcast to every active
-- | `changes` subscriber.
newtype SubscriptionRef a = SubscriptionRef
  { ref :: Ref a
  , hub :: Hub a
  , sem :: Semaphore
  }

-- | Allocate a fresh `SubscriptionRef` initialised to `value`.
make :: forall r e a. a -> RIO r e (SubscriptionRef a)
make = liftEffect <<< makeEffect

-- | `Effect`-typed variant for callers that allocate at the top of
-- | `main`.
makeEffect :: forall a. a -> Effect (SubscriptionRef a)
makeEffect value = do
  ref <- Ref.new value
  hub <- Hub.make
  sem <- Semaphore.make 1
  pure (SubscriptionRef { ref, hub, sem })

-- | Read the current value. Atomic with respect to in-flight
-- | `modifyM`s.
read :: forall r e a. SubscriptionRef a -> RIO r e a
read (SubscriptionRef s) = Semaphore.withPermit s.sem do
  liftEffect (Ref.read s.ref)

-- | Overwrite the value and publish the new value to subscribers.
set :: forall r e a. SubscriptionRef a -> a -> RIO r e Unit
set (SubscriptionRef s) value = Semaphore.withPermit s.sem do
  liftEffect (Ref.write value s.ref)
  Hub.publish s.hub value

-- | Apply a pure function under the lock, write the result, and
-- | publish it. Returns the new value.
modify
  :: forall r e a
   . SubscriptionRef a
  -> (a -> a)
  -> RIO r e a
modify (SubscriptionRef s) f = Semaphore.withPermit s.sem do
  next <- liftEffect (Ref.modify f s.ref)
  Hub.publish s.hub next
  pure next

-- | Apply an effectful update under the lock. The update body runs
-- | inside the critical section, so reads and other updates wait
-- | until it completes. Returns the new value.
modifyM
  :: forall r e a
   . SubscriptionRef a
  -> (a -> RIO r e a)
  -> RIO r e a
modifyM (SubscriptionRef s) f = Semaphore.withPermit s.sem do
  current <- liftEffect (Ref.read s.ref)
  next <- f current
  liftEffect (Ref.write next s.ref)
  Hub.publish s.hub next
  pure next

-- | `modifyM` that discards the new value.
modifyM_
  :: forall r e a
   . SubscriptionRef a
  -> (a -> RIO r e a)
  -> RIO r e Unit
modifyM_ ref f = void (modifyM ref f)

-- | Alias for `modify` that reads as "update".
update
  :: forall r e a
   . SubscriptionRef a
  -> (a -> a)
  -> RIO r e Unit
update ref f = void (modify ref f)

-- | Alias for `modifyM_` that reads as "update".
updateM
  :: forall r e a
   . SubscriptionRef a
  -> (a -> RIO r e a)
  -> RIO r e Unit
updateM = modifyM_

-- | A stream that emits the current value immediately, then every
-- | subsequent write. The subscription is tied to `scope`: the
-- | underlying hub slot is released when the scope closes. Initial
-- | subscribe + snapshot happen under the same lock as writes, so
-- | a concurrent write is either reflected in the initial value or
-- | delivered via the hub, never both.
changes
  :: forall r e a
   . Scope
  -> SubscriptionRef a
  -> Stream r e a
changes scope (SubscriptionRef s) = Stream do
  Tuple sub current <- Semaphore.withPermit s.sem do
    -- Run Hub.subscribe at row `()` so we can later run its
    -- `unsubscribe` in an Aff finalizer without leaking the caller
    -- row into the finalizer's environment.
    sub <- mkRIO \_ ->
      unsafeUnRIO (Hub.subscribe s.hub :: RIO () () _) {}
    addFinalizer scope (unsafeUnRIO sub.unsubscribe {})
    current <- liftEffect (Ref.read s.ref)
    pure (Tuple sub.queue current)
  pure (Yield current (drain sub))
  where
  drain :: Queue a -> Stream r e a
  drain q = Stream do
    m <- Queue.take q
    case m of
      Nothing -> pure Done
      Just a -> pure (Yield a (drain q))

-- | Current number of active `changes` subscribers.
subscribers :: forall r e a. SubscriptionRef a -> RIO r e Int
subscribers (SubscriptionRef s) = liftEffect (Hub.subscriberCount s.hub)
