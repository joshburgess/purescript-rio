-- | A minimal pessimistic Software Transactional Memory.
-- |
-- | An `STM` action is a sequence of reads and writes over `TVar`s
-- | that runs atomically inside `atomically`. Transactions serialize
-- | through a single global lock; the trade is throughput for a
-- | trivially-correct implementation. Optimistic per-TVar versioning
-- | is a future enhancement.
-- |
-- | Reads and writes are buffered in a per-transaction log so that
-- | `orElse` can roll back a failed alternative without leaking
-- | partial writes. `retry` registers the transaction's read-set on
-- | each TVar's waiter list and suspends; a commit that writes to
-- | any of those TVars wakes only the fibers that observed them.
module RIO.Fiber.STM
  ( STM
  , TVar
  , newTVar
  , readTVar
  , writeTVar
  , modifyTVar
  , swapTVar
  , retry
  , orElse
  , atomically
  ) where

import Prelude

import Data.Array (drop, find, length, snoc)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
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

-- | A transactional reference cell.
newtype TVar a = TVar
  { id :: Int
  , value :: Ref a
  , staged :: Ref (Maybe a)
  , wasRead :: Ref Boolean
  , waiters :: Ref (Array (Deferred () Unit))
  }

-- | A per-attempt log entry: hooks that commit / register / reset the
-- | underlying TVar's transaction-local state. Type-erased over the
-- | TVar's payload so the log is a single homogeneous array.
newtype Touched = Touched
  { id :: Int
  , commit :: Effect Unit
  , register :: Deferred () Unit -> Effect Unit
  , reset :: Effect Unit
  , clearWrite :: Effect Unit
  }

touchedId :: Touched -> Int
touchedId (Touched t) = t.id

type TxLog = Ref (Array Touched)

-- | A transaction. `STM` actions form a monad; sequence them with
-- | `do` and run the whole sequence atomically with `atomically`.
newtype STM a = STM (TxLog -> Effect (Result a))

instance functorSTM :: Functor STM where
  map f (STM run) = STM \log -> do
    r <- run log
    pure case r of
      Done a -> Done (f a)
      NeedRetry -> NeedRetry

instance applySTM :: Apply STM where
  apply (STM rf) (STM ra) = STM \log -> do
    rfa <- rf log
    case rfa of
      NeedRetry -> pure NeedRetry
      Done f -> do
        raa <- ra log
        pure case raa of
          NeedRetry -> NeedRetry
          Done a -> Done (f a)

instance applicativeSTM :: Applicative STM where
  pure a = STM \_ -> pure (Done a)

instance bindSTM :: Bind STM where
  bind (STM run) k = STM \log -> do
    r <- run log
    case r of
      NeedRetry -> pure NeedRetry
      Done a -> case k a of STM run' -> run' log

instance monadSTM :: Monad STM

-- | Allocate a fresh `TVar`. Allocation runs outside any
-- | transaction; the returned cell is immediately usable.
newTVar :: forall a. a -> Effect (TVar a)
newTVar a = do
  id <- Ref.modify (_ + 1) nextTVarId
  value <- Ref.new a
  staged <- Ref.new Nothing
  wasRead <- Ref.new false
  waiters <- Ref.new []
  pure (TVar { id, value, staged, wasRead, waiters })

-- | Read the current value of the cell. Sees prior writes in the
-- | same transaction; otherwise reads from the committed value.
readTVar :: forall a. TVar a -> STM a
readTVar tv@(TVar t) = STM \log -> do
  ensureTouched tv log
  mStaged <- Ref.read t.staged
  case mStaged of
    Just v -> pure (Done v)
    Nothing -> do
      Ref.write true t.wasRead
      v <- Ref.read t.value
      pure (Done v)

-- | Replace the value of the cell within the current transaction.
-- | The write is buffered until the transaction commits.
writeTVar :: forall a. TVar a -> a -> STM Unit
writeTVar tv@(TVar t) a = STM \log -> do
  ensureTouched tv log
  Ref.write (Just a) t.staged
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

-- | Abort the current transaction and re-run it the next time any
-- | TVar it read is written.
retry :: forall a. STM a
retry = STM \_ -> pure NeedRetry

-- | Try the first alternative; if it retries, roll back its writes
-- | (but keep its reads, so the combined transaction wakes on either
-- | branch's read-set) and try the second.
orElse :: forall a. STM a -> STM a -> STM a
orElse (STM left) (STM right) = STM \log -> do
  arr0 <- Ref.read log
  let snapshot = length arr0
  r <- left log
  case r of
    Done a -> pure (Done a)
    NeedRetry -> do
      arr1 <- Ref.read log
      let extras = drop snapshot arr1
      traverse_ (\(Touched te) -> te.clearWrite) extras
      right log

-- | Run the transaction atomically. Acquires the global STM lock,
-- | runs the body, and on success commits staged writes and wakes
-- | fibers blocked on any written TVar. On `retry` (or `orElse`
-- | exhausting both branches) the caller releases the lock, suspends
-- | on the read-set's waiter cells, and re-runs once any of them
-- | fires.
atomically :: forall r e a. STM a -> RIO r e a
atomically (STM run) = loop
  where
  loop = do
    Sem.acquireN 1 stmLock
    log <- liftEffect (Ref.new ([] :: Array Touched))
    r <- liftEffect (run log)
    arr <- liftEffect (Ref.read log)
    case r of
      Done a -> do
        liftEffect do
          -- Apply staged writes and wake per-TVar waiters. Inside
          -- the lock so a subscriber registered before our release
          -- can't miss the wake-up.
          traverse_ (\(Touched te) -> te.commit) arr
          traverse_ (\(Touched te) -> te.reset) arr
        Sem.releaseN 1 stmLock
        pure a
      NeedRetry -> do
        wake <- liftEffect D.make
        liftEffect do
          traverse_ (\(Touched te) -> te.register wake) arr
          -- Clear staged writes (rolled back) and wasRead flags.
          traverse_ (\(Touched te) -> te.reset) arr
        Sem.releaseN 1 stmLock
        D.awaitPure wake
        loop

stmLock :: Semaphore
stmLock = unsafePerformEffect (Sem.make 1)

nextTVarId :: Ref Int
nextTVarId = unsafePerformEffect (Ref.new 0)

ensureTouched :: forall a. TVar a -> TxLog -> Effect Unit
ensureTouched (TVar t) log = do
  arr <- Ref.read log
  case find (\e -> touchedId e == t.id) arr of
    Just _ -> pure unit
    Nothing -> Ref.write (snoc arr (mkTouched (TVar t))) log

mkTouched :: forall a. TVar a -> Touched
mkTouched (TVar t) = Touched
  { id: t.id
  , commit: do
      m <- Ref.read t.staged
      case m of
        Nothing -> pure unit
        Just v -> do
          Ref.write v t.value
          ws <- Ref.read t.waiters
          Ref.write [] t.waiters
          traverse_ (\d -> void (D._succeed d unit)) ws
  , register: \d -> do
      r <- Ref.read t.wasRead
      when r (Ref.modify_ (\xs -> snoc xs d) t.waiters)
  , reset: do
      Ref.write Nothing t.staged
      Ref.write false t.wasRead
  , clearWrite: Ref.write Nothing t.staged
  }

