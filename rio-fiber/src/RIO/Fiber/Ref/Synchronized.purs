-- | A `Ref` that supports atomic effectful updates.
-- |
-- | `Effect.Ref` deliberately lacks a `modifyM`-style combinator
-- | for `RIO` actions, because any `RIO`-typed update body can
-- | yield to other fibers between read and write, which would
-- | race. `Ref.Synchronized` is the principled answer: it pairs a
-- | plain `Ref` with a `Semaphore` of one permit and serialises
-- | every effectful update behind that permit. Reads and pure
-- | updates still take the permit, so a long-running effectful
-- | update will block concurrent readers; this is the same trade-
-- | off ZIO's `Ref.Synchronized` and Effect's `SynchronizedRef`
-- | make.
-- |
-- | Use this when you need to perform an effect inside the update
-- | (look up a service, read a Ref, log, etc.) and still want the
-- | "read, compute, write" sequence to be observed atomically by
-- | other fibers. For pure updates, prefer `Effect.Ref` directly;
-- | for transactional composition with other shared state, prefer
-- | `RIO.Fiber.STM`'s `TVar`.
-- |
-- | ```purescript
-- | -- Increment a counter while reading from a service inside the
-- | -- update body. No other fiber can observe a torn state.
-- | program = do
-- |   ref <- Ref.Synchronized.new 0
-- |   _ <- Ref.Synchronized.modifyM ref \n -> do
-- |     bonus <- asks _.bonusForUser
-- |     pure (n + bonus)
-- |   Ref.Synchronized.read ref
-- | ```
module RIO.Fiber.Ref.Synchronized
  ( SynchronizedRef
  , new
  , newEffect
  , read
  , write
  , modify
  , modifyM
  , modifyM_
  , update
  , updateM
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref as ERef

import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Semaphore (Semaphore)
import RIO.Fiber.Semaphore as Semaphore

-- | A mutable cell with a single-permit semaphore guarding
-- | effectful updates. The constructor is hidden.
newtype SynchronizedRef a = SynchronizedRef
  { ref :: ERef.Ref a
  , sem :: Semaphore
  }

-- | Create a fresh `SynchronizedRef` initialised to `value`.
new :: forall r e a. a -> RIO r e (SynchronizedRef a)
new = liftEffect <<< newEffect

-- | `Effect`-typed variant for callers that allocate at the top of
-- | `main`.
newEffect :: forall a. a -> Effect (SynchronizedRef a)
newEffect value = do
  ref <- ERef.new value
  sem <- Semaphore.make 1
  pure (SynchronizedRef { ref, sem })

-- | Read the current value. Atomic with respect to in-flight
-- | `modifyM` calls: a `read` waits for any running effectful
-- | update to complete.
read :: forall r e a. SynchronizedRef a -> RIO r e a
read (SynchronizedRef s) = Semaphore.withPermit s.sem do
  liftEffect (ERef.read s.ref)

-- | Overwrite the value. Waits for any running effectful update.
write :: forall r e a. SynchronizedRef a -> a -> RIO r e Unit
write (SynchronizedRef s) value = Semaphore.withPermit s.sem do
  liftEffect (ERef.write value s.ref)

-- | Apply a pure function under the lock. Returns the new value.
-- | Equivalent to `modifyM ref (pure <<< f)` but avoids the extra
-- | wrapper.
modify
  :: forall r e a
   . SynchronizedRef a
  -> (a -> a)
  -> RIO r e a
modify (SynchronizedRef s) f = Semaphore.withPermit s.sem do
  liftEffect (ERef.modify f s.ref)

-- | Apply an effectful function to the current value under the
-- | lock and store the result. Returns the new value. While the
-- | update body runs, every other operation on this ref blocks.
modifyM
  :: forall r e a
   . SynchronizedRef a
  -> (a -> RIO r e a)
  -> RIO r e a
modifyM (SynchronizedRef s) f = Semaphore.withPermit s.sem do
  current <- liftEffect (ERef.read s.ref)
  next <- f current
  liftEffect do
    ERef.write next s.ref
    pure next

-- | `modifyM` that discards the new value.
modifyM_
  :: forall r e a
   . SynchronizedRef a
  -> (a -> RIO r e a)
  -> RIO r e Unit
modifyM_ ref f = void (modifyM ref f)

-- | Alias for `modify` that reads as "update".
update
  :: forall r e a
   . SynchronizedRef a
  -> (a -> a)
  -> RIO r e Unit
update ref f = void (modify ref f)

-- | Alias for `modifyM_` that reads as "update".
updateM
  :: forall r e a
   . SynchronizedRef a
  -> (a -> RIO r e a)
  -> RIO r e Unit
updateM = modifyM_
