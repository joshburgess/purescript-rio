-- | Software Transactional Memory with optimistic concurrency.
-- |
-- | An `STM` action is a sequence of reads and writes over `TVar`s
-- | that runs atomically inside `atomically`. Each TVar carries a
-- | version counter; a transaction body runs without holding any
-- | lock, recording the version of every TVar it reads and staging
-- | writes in a per-attempt log. At commit time the runner briefly
-- | acquires a global commit lock, validates that every read TVar's
-- | version is unchanged, and either applies the staged writes
-- | (bumping versions, draining per-TVar waiters) or aborts and
-- | retries.
-- |
-- | `retry` aborts and re-runs once any TVar in the read-set is
-- | written. `orElse` snapshots the log, runs the first alternative,
-- | and on retry rolls back its writes (keeping reads, so the
-- | combined transaction wakes on either branch's reads) before
-- | trying the second.
module RIO.Fiber.STM
  ( STM
  , TVar
  , newTVar
  , newTVarSTM
  , readTVar
  , writeTVar
  , modifyTVar
  , swapTVar
  , retry
  , check
  , orElse
  , atomically
  ) where

import Prelude

import Data.Array (drop, find, length, snoc, uncons)
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
import Unsafe.Coerce (unsafeCoerce)

data Result a = Done a | NeedRetry

-- | A transactional reference cell. Each TVar has a version counter
-- | that is bumped on every commit that wrote to it; readers use the
-- | version to detect interfering writes at commit time.
newtype TVar a = TVar
  { id :: Int
  , value :: Ref a
  , version :: Ref Int
  , waiters :: Ref (Array (Deferred () Unit))
  }

-- | An opaque "any" value: type-erased so the log can be homogeneous.
foreign import data AnyValue :: Type

eraseValue :: forall a. a -> AnyValue
eraseValue = unsafeCoerce

restoreValue :: forall a. AnyValue -> a
restoreValue = unsafeCoerce

-- | A per-attempt log entry. One per TVar touched during the
-- | transaction. Per-type knowledge is sealed inside the closures;
-- | the entry itself is uniform so the log can be a plain array.
newtype Entry = Entry
  { id :: Int
  , originalVersion :: Ref (Maybe Int)
  , staged :: Ref (Maybe AnyValue)
  , validate :: Effect Boolean
  , commit :: Effect Unit
  , clearStaged :: Effect Unit
  , register :: Deferred () Unit -> Effect Unit
  }

entryId :: Entry -> Int
entryId (Entry e) = e.id

type TxLog = Ref (Array Entry)

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
  version <- Ref.new 0
  waiters <- Ref.new []
  pure (TVar { id, value, version, waiters })

-- | Allocate a fresh `TVar` inside an STM action. The new cell is
-- | not yet visible to other transactions; it becomes observable
-- | only when it's stored (via `writeTVar`) into a previously-
-- | reachable cell.
newTVarSTM :: forall a. a -> STM (TVar a)
newTVarSTM a = STM \_ -> do
  tv <- newTVar a
  pure (Done tv)

-- | Read the current value of the cell. If we've staged a write to
-- | the cell earlier in this transaction, see that staged value;
-- | otherwise read the committed value and record the version we
-- | observed for later commit-time validation.
readTVar :: forall a. TVar a -> STM a
readTVar tv@(TVar t) = STM \log -> do
  entry <- ensureEntry tv log
  case entry of
    Entry e -> do
      mStaged <- Ref.read e.staged
      case mStaged of
        Just v -> pure (Done (restoreValue v))
        Nothing -> do
          mOrig <- Ref.read e.originalVersion
          case mOrig of
            Nothing -> do
              ver <- Ref.read t.version
              Ref.write (Just ver) e.originalVersion
              v <- Ref.read t.value
              pure (Done v)
            Just _ -> do
              v <- Ref.read t.value
              pure (Done v)

-- | Replace the value of the cell within the current transaction.
-- | The write is buffered until commit; concurrent transactions
-- | observe the pre-write value until this one commits.
writeTVar :: forall a. TVar a -> a -> STM Unit
writeTVar tv a = STM \log -> do
  entry <- ensureEntry tv log
  case entry of
    Entry e -> do
      Ref.write (Just (eraseValue a)) e.staged
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

-- | Guard a transaction on a predicate. `check true` is a no-op;
-- | `check false` retries.
check :: Boolean -> STM Unit
check b = if b then pure unit else retry

-- | Try the first alternative; if it retries, roll back its writes
-- | (but keep its reads) and try the second. If both retry, the
-- | whole transaction retries with the union of both read-sets.
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
      traverse_ (\(Entry e) -> e.clearStaged) extras
      right log

-- | Run the transaction atomically. The body runs without holding
-- | any lock; the commit step briefly acquires the global commit
-- | lock to validate read-set versions and apply staged writes.
-- | If validation fails (some other transaction wrote to a TVar we
-- | read), the whole transaction retries.
atomically :: forall r e a. STM a -> RIO r e a
atomically (STM run) = loop
  where
  loop = do
    log <- liftEffect (Ref.new ([] :: Array Entry))
    r <- liftEffect (run log)
    arr <- liftEffect (Ref.read log)
    case r of
      Done a -> do
        committed <- attemptCommit arr
        if committed then pure a
        else loop
      NeedRetry -> do
        result <- attemptRetry arr
        case result of
          RetryImmediately -> loop
          RetryAwait wake -> do
            D.awaitPure wake
            loop

  attemptCommit arr = do
    Sem.acquireN 1 commitLock
    ok <- liftEffect (allValidate arr)
    if ok then do
      liftEffect (traverse_ (\(Entry e) -> e.commit) arr)
      Sem.releaseN 1 commitLock
      pure true
    else do
      Sem.releaseN 1 commitLock
      pure false

  attemptRetry arr = do
    Sem.acquireN 1 commitLock
    ok <- liftEffect (allValidate arr)
    if not ok then do
      Sem.releaseN 1 commitLock
      pure RetryImmediately
    else do
      wake <- liftEffect D.make
      liftEffect (traverse_ (\(Entry e) -> e.register wake) arr)
      Sem.releaseN 1 commitLock
      pure (RetryAwait wake)

data RetryDecision
  = RetryImmediately
  | RetryAwait (Deferred () Unit)

allValidate :: Array Entry -> Effect Boolean
allValidate xs = go xs
  where
  go ys = case uncons ys of
    Nothing -> pure true
    Just { head: Entry e, tail } -> do
      v <- e.validate
      if v then go tail else pure false

commitLock :: Semaphore
commitLock = unsafePerformEffect (Sem.make 1)

nextTVarId :: Ref Int
nextTVarId = unsafePerformEffect (Ref.new 0)

ensureEntry :: forall a. TVar a -> TxLog -> Effect Entry
ensureEntry tv@(TVar t) log = do
  arr <- Ref.read log
  case find (\e -> entryId e == t.id) arr of
    Just e -> pure e
    Nothing -> do
      e <- mkEntry tv
      Ref.write (snoc arr e) log
      pure e

mkEntry :: forall a. TVar a -> Effect Entry
mkEntry (TVar t) = do
  originalVersion <- Ref.new (Nothing :: Maybe Int)
  staged <- Ref.new (Nothing :: Maybe AnyValue)
  pure $ Entry
    { id: t.id
    , originalVersion
    , staged
    , validate: do
        mOrig <- Ref.read originalVersion
        case mOrig of
          Nothing -> pure true
          Just expected -> do
            current <- Ref.read t.version
            pure (current == expected)
    , commit: do
        m <- Ref.read staged
        case m of
          Nothing -> pure unit
          Just v -> do
            Ref.write (restoreValue v) t.value
            Ref.modify_ (_ + 1) t.version
            ws <- Ref.read t.waiters
            Ref.write [] t.waiters
            traverse_ (\d -> void (D._succeed d unit)) ws
    , clearStaged: Ref.write Nothing staged
    , register: \d -> do
        mOrig <- Ref.read originalVersion
        case mOrig of
          Just _ -> Ref.modify_ (\xs -> snoc xs d) t.waiters
          Nothing -> pure unit
    }

