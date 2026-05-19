-- | A minimal pessimistic Software Transactional Memory.
-- |
-- | An `STM` action is a sequence of reads and writes over `TVar`s
-- | that runs atomically inside `atomically`. The MVP uses a single
-- | global mutex rather than optimistic concurrency: only one `STM`
-- | block runs at a time, so a long transaction blocks every other
-- | one. That trades throughput for a trivially-correct, easy-to-
-- | reason-about implementation. Optimistic per-TVar versioning is a
-- | follow-up.
-- |
-- | `retry` aborts the current attempt and re-runs it once any
-- | other transaction commits. (Granular wakeups on specific
-- | `TVar`s are not yet implemented; every successful commit wakes
-- | every retrying fiber.) Composing alternatives via `orElse` is
-- | not yet provided.
module RIO.Fiber.STM
  ( STM
  , TVar
  , newTVar
  , readTVar
  , writeTVar
  , modifyTVar
  , swapTVar
  , retry
  , atomically
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Deferred (Deferred)
import RIO.Fiber.Deferred as D
import RIO.Fiber.Semaphore (Semaphore)
import RIO.Fiber.Semaphore as Sem

data Result a = Done a | NeedRetry

-- | A transaction. `STM` actions form a monad; sequence them with
-- | `do` and run the whole sequence atomically with `atomically`.
newtype STM a = STM (Effect (Result a))

instance functorSTM :: Functor STM where
  map f (STM run) = STM do
    r <- run
    pure case r of
      Done a -> Done (f a)
      NeedRetry -> NeedRetry

instance applySTM :: Apply STM where
  apply (STM rf) (STM ra) = STM do
    rfa <- rf
    case rfa of
      NeedRetry -> pure NeedRetry
      Done f -> do
        raa <- ra
        pure case raa of
          NeedRetry -> NeedRetry
          Done a -> Done (f a)

instance applicativeSTM :: Applicative STM where
  pure a = STM (pure (Done a))

instance bindSTM :: Bind STM where
  bind (STM run) k = STM do
    r <- run
    case r of
      NeedRetry -> pure NeedRetry
      Done a -> case k a of STM run' -> run'

instance monadSTM :: Monad STM

-- | A transactional reference cell.
newtype TVar a = TVar (Ref a)

-- | Allocate a fresh `TVar`. Allocation runs outside any
-- | transaction; the returned cell is immediately usable.
newTVar :: forall a. a -> Effect (TVar a)
newTVar a = TVar <$> Ref.new a

-- | Read the current value of the cell.
readTVar :: forall a. TVar a -> STM a
readTVar (TVar ref) = STM (Done <$> Ref.read ref)

-- | Replace the value of the cell.
writeTVar :: forall a. TVar a -> a -> STM Unit
writeTVar (TVar ref) a = STM do
  Ref.write a ref
  pure (Done unit)

-- | Read, transform, and write the cell.
modifyTVar :: forall a. TVar a -> (a -> a) -> STM Unit
modifyTVar t f = do
  v <- readTVar t
  writeTVar t (f v)

-- | Atomically replace the cell's value and return the previous
-- | value.
swapTVar :: forall a. TVar a -> a -> STM a
swapTVar t a = do
  prev <- readTVar t
  writeTVar t a
  pure prev

-- | Abort the current transaction and re-run it the next time
-- | any other transaction commits.
retry :: forall a. STM a
retry = STM (pure NeedRetry)

-- | Run the transaction atomically. Acquires the global lock, runs
-- | the body, and (on success) wakes every fiber currently blocked
-- | on `retry`. On `retry` the lock is released and the caller
-- | suspends until some other transaction commits, then re-runs.
atomically :: forall r e a. STM a -> RIO r e a
atomically stm = loop
  where
  loop = do
    Sem.acquireN 1 stmLock
    case stm of
      STM run -> do
        r <- liftEffect run
        case r of
          Done a -> do
            -- Notify retrying fibers. Inside the lock so subscribers
            -- registered before our release see the wake-up.
            d <- liftEffect (Ref.read changedRef)
            d' <- liftEffect D.make
            liftEffect (Ref.write d' changedRef)
            _ <- liftEffect (D._succeed d unit)
            Sem.releaseN 1 stmLock
            pure a
          NeedRetry -> do
            d <- liftEffect (Ref.read changedRef)
            Sem.releaseN 1 stmLock
            D.awaitPure d
            loop

stmLock :: Semaphore
stmLock = unsafePerformEffect (Sem.make 1)

changedRef :: Ref (Deferred () Unit)
changedRef = unsafePerformEffect do
  d <- D.make
  Ref.new d
