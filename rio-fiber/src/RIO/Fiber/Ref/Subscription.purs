-- | A `Ref` that publishes its current value and every subsequent
-- | change as a `Stream`.
-- |
-- | A `SubscriptionRef` is a `SynchronizedRef`-style cell paired with
-- | a `Hub`. Every write commits to the underlying ref and then
-- | broadcasts the new value to every active subscriber. The
-- | `changes` stream emits the current value once and then every
-- | future write, with no possibility of missing or duplicating an
-- | update: subscribe and snapshot happen under the same lock, so a
-- | concurrent write either lands in the snapshot or in the hub, but
-- | never in both. Effect-ts, ZIO and FS2's `SignallingRef`/
-- | `SubscriptionRef` are the same pattern.
-- |
-- | ```purescript
-- | -- Mirror a config value into a UI as it updates.
-- | program = do
-- |   cfg <- Sub.make 16 defaultConfig
-- |   _ <- F.fork (Sub.set cfg newConfig)
-- |   Scope.scoped \scope ->
-- |     Sub.changes scope cfg
-- |       # S.tap renderConfig
-- |       # S.take 2
-- |       # S.run
-- | ```
module RIO.Fiber.Ref.Subscription
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

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as ERef

import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Hub (Hub, Subscription)
import RIO.Fiber.Hub as Hub
import RIO.Fiber.Scope (Scope)
import RIO.Fiber.Semaphore (Semaphore)
import RIO.Fiber.Semaphore as Semaphore
import RIO.Fiber.Stream (Stream(..), Step(..))

-- | A `Ref a` whose updates are broadcast to every active
-- | `changes` subscriber.
newtype SubscriptionRef a = SubscriptionRef
  { ref :: ERef.Ref a
  , hub :: Hub a
  , sem :: Semaphore
  }

-- | Allocate a fresh `SubscriptionRef` initialised to `value`.
-- | `bufferCapacity` is the per-subscriber queue capacity passed to
-- | the underlying `Hub`. Subscribers that fall behind will
-- | backpressure publishers once their queue is full.
make
  :: forall r e a
   . Int
  -> a
  -> RIO r e (SubscriptionRef a)
make bufferCapacity = liftEffect <<< makeEffect bufferCapacity

-- | `Effect`-typed variant for callers that allocate at the top of
-- | `main`.
makeEffect :: forall a. Int -> a -> Effect (SubscriptionRef a)
makeEffect bufferCapacity value = do
  ref <- ERef.new value
  hub <- Hub.make bufferCapacity
  sem <- Semaphore.make 1
  pure (SubscriptionRef { ref, hub, sem })

-- | Read the current value. Atomic with respect to in-flight
-- | `modifyM`s.
read :: forall r e a. SubscriptionRef a -> RIO r e a
read (SubscriptionRef s) = Semaphore.withPermit s.sem do
  liftEffect (ERef.read s.ref)

-- | Overwrite the value and publish the new value to subscribers.
-- | Backpressures on subscriber queues.
set :: forall r e a. SubscriptionRef a -> a -> RIO r e Unit
set (SubscriptionRef s) value = Semaphore.withPermit s.sem do
  liftEffect (ERef.write value s.ref)
  Hub.publish s.hub value

-- | Apply a pure function under the lock, write the result, and
-- | publish it. Returns the new value.
modify
  :: forall r e a
   . SubscriptionRef a
  -> (a -> a)
  -> RIO r e a
modify (SubscriptionRef s) f = Semaphore.withPermit s.sem do
  next <- liftEffect (ERef.modify f s.ref)
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
  current <- liftEffect (ERef.read s.ref)
  next <- f current
  liftEffect (ERef.write next s.ref)
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
-- | subscribe + snapshot happen under the same lock as writes, so a
-- | concurrent write is either reflected in the initial value or
-- | delivered via the hub, never both.
changes
  :: forall r e a
   . Scope
  -> SubscriptionRef a
  -> Stream r e a
changes scope (SubscriptionRef s) = Stream do
  Tuple sub current <- Semaphore.withPermit s.sem do
    sub <- Hub.subscribeScoped scope s.hub
    current <- liftEffect (ERef.read s.ref)
    pure (Tuple sub current)
  pure (Yield current (drain sub))
  where
  drain :: Subscription a -> Stream r e a
  drain sub = Stream do
    a <- Hub.take sub
    pure (Yield a (drain sub))

-- | Current number of active `changes` subscribers.
subscribers :: forall r e a. SubscriptionRef a -> RIO r e Int
subscribers (SubscriptionRef s) = Hub.subscribers s.hub
