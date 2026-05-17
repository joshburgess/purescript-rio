-- | Software Transactional Memory over `RIO`. An `STM e a` is a
-- | pure description of a transaction that reads and writes
-- | `TRef`s; `atomically` runs it as an `RIO` action that either
-- | commits every write at once or applies none.
-- |
-- | The implementation relies on the JavaScript event loop: an
-- | `STM` body is a synchronous `Effect`, so no other fiber can
-- | observe its intermediate writes. There is no need for version
-- | numbers or pessimistic locks; the commit phase just flushes the
-- | accumulated write log in one `Effect` block and fires any
-- | waiters registered on the written `TRef`s.
-- |
-- | `retry` aborts the current attempt and re-runs it once any
-- | `TRef` the transaction read changes; `check` is `retry` guarded
-- | by a predicate; `orElse` falls through on `retry`. Typed
-- | failures inside an `STM` abort the transaction (no writes are
-- | applied) and surface on the parent `RIO`'s error row.
-- |
-- | The error row inside `STM` is independent of the surrounding
-- | `RIO` row; `atomically` connects them. Nothing prevents the
-- | same `TRef` from being used in transactions with different
-- | error rows.
-- |
-- | Derived structures built on top of `TRef` plus the primitives
-- | here ship in sibling modules: `RIO.STM.TQueue`, `RIO.STM.THub`,
-- | `RIO.STM.TMap`, `RIO.STM.TArray`, `RIO.STM.TSemaphore`, and
-- | `RIO.STM.TDeferred`.
module RIO.STM
  ( STM
  , TRef
  , TVar
  , atomically
  , check
  , failSTM
  , modifyTRef
  , modifyTVar
  , newTRef
  , newTVar
  , orElse
  , readTRef
  , readTVar
  , retry
  , throwSTM
  , writeTRef
  , writeTVar
  ) where

import Prelude

import Data.Array (find, snoc) as Array
import Data.Array (filter, last)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.AVar as EAVar
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

import RIO.Internal (RIO(..), mkRIO, rioFail)

-- | A transactional reference. Created with `newTRef`, read with
-- | `readTRef`, written with `writeTRef`, modified with
-- | `modifyTRef`. All operations are `STM`-valued and take effect
-- | only when the surrounding transaction commits.
-- |
-- | The constructor is hidden; identity is by the underlying `Ref`,
-- | so two `TRef`s built from the same `newTRef` call are equal and
-- | distinct from any other.
newtype TRef :: Type -> Type
newtype TRef a = TRef
  { id :: Int
  , value :: Ref a
  , waiters :: Ref (Array (Effect Unit))
  }

-- | A transaction: a description of reads, writes, and decisions
-- | against `TRef`s. Run with `atomically`.
-- |
-- | `STM` is a `Monad`: bind threads the transaction log through
-- | each step. Bind short-circuits on `retry` (the transaction
-- | aborts and waits) and on typed failure (the transaction aborts
-- | and surfaces the failure on the parent's row).
newtype STM :: Row Type -> Type -> Type
newtype STM e a = STM (Log -> Effect (TxResult e a))

data TxResult :: Row Type -> Type -> Type
data TxResult e a
  = TxSuccess a
  | TxFailed (Variant e)
  | TxRetry

type Log =
  { reads :: Ref (Array ReadEntry)
  , writes :: Ref (Array WriteEntry)
  }

type ReadEntry =
  { id :: Int
  , subscribe :: Effect Unit -> Effect Unit
  }

type WriteEntry =
  { id :: Int
  , apply :: Effect Unit
  , value :: AnyValue
  }

foreign import data AnyValue :: Type

toAny :: forall a. a -> AnyValue
toAny = unsafeCoerce

fromAny :: forall a. AnyValue -> a
fromAny = unsafeCoerce

idCounter :: Ref Int
idCounter = unsafePerformEffect (Ref.new 0)

freshTRefId :: Effect Int
freshTRefId = Ref.modify (_ + 1) idCounter

mapTxResult :: forall e a b. (a -> b) -> TxResult e a -> TxResult e b
mapTxResult f = case _ of
  TxSuccess a -> TxSuccess (f a)
  TxFailed v -> TxFailed v
  TxRetry -> TxRetry

instance functorSTM :: Functor (STM e) where
  map f (STM g) = STM \log -> map (mapTxResult f) (g log)

instance applySTM :: Apply (STM e) where
  apply (STM mf) (STM ma) = STM \log -> do
    rf <- mf log
    case rf of
      TxSuccess h -> map (mapTxResult h) (ma log)
      TxFailed v -> pure (TxFailed v)
      TxRetry -> pure TxRetry

instance applicativeSTM :: Applicative (STM e) where
  pure a = STM \_ -> pure (TxSuccess a)

instance bindSTM :: Bind (STM e) where
  bind (STM m) k = STM \log -> do
    r <- m log
    case r of
      TxSuccess a ->
        let
          STM h = k a
        in
          h log
      TxFailed v -> pure (TxFailed v)
      TxRetry -> pure TxRetry

instance monadSTM :: Monad (STM e)

-- | Allocate a fresh `TRef` initialised to the given value.
-- |
-- | ```purescript
-- | counter <- atomically (newTRef 0)
-- | ```
newTRef :: forall e a. a -> STM e (TRef a)
newTRef a = STM \_ -> do
  rid <- freshTRefId
  value <- Ref.new a
  waiters <- Ref.new []
  pure (TxSuccess (TRef { id: rid, value, waiters }))

-- | Read the current value of a `TRef` inside a transaction. If
-- | the transaction has already written to this `TRef`, returns
-- | the pending value; otherwise reads from the underlying `Ref`
-- | and records the read in the log so `retry` can wait on it.
-- |
-- | ```purescript
-- | currentValue <- atomically (readTRef counter)
-- | ```
readTRef :: forall e a. TRef a -> STM e a
readTRef (TRef r) = STM \log -> do
  ws <- Ref.read log.writes
  case findLatestWrite r.id ws of
    Just v -> pure (TxSuccess (fromAny v))
    Nothing -> do
      a <- Ref.read r.value
      rs <- Ref.read log.reads
      case Array.find (\e -> e.id == r.id) rs of
        Just _ -> pure unit
        Nothing -> do
          let
            entry =
              { id: r.id
              , subscribe: \cb ->
                  Ref.modify_ (\xs -> Array.snoc xs cb) r.waiters
              }
          Ref.write (Array.snoc rs entry) log.reads
      pure (TxSuccess a)
  where
  findLatestWrite :: Int -> Array WriteEntry -> Maybe AnyValue
  findLatestWrite k xs = case last (filter (\e -> e.id == k) xs) of
    Nothing -> Nothing
    Just e -> Just e.value

-- | Write a new value to a `TRef`. The write is staged in the
-- | transaction log and applied only when the transaction commits.
-- | Subsequent `readTRef` calls in the same transaction see the
-- | staged value.
-- |
-- | ```purescript
-- | atomically (writeTRef counter 42)
-- | ```
writeTRef :: forall e a. TRef a -> a -> STM e Unit
writeTRef (TRef r) a = STM \log -> do
  let
    entry =
      { id: r.id
      , apply: do
          Ref.write a r.value
          ws <- Ref.read r.waiters
          Ref.write [] r.waiters
          for_ ws identity
      , value: toAny a
      }
  Ref.modify_ (\xs -> Array.snoc xs entry) log.writes
  pure (TxSuccess unit)

-- | `readTRef` then `writeTRef`, in one call. Equivalent to
-- | `readTRef ref >>= writeTRef ref <<< f`.
-- |
-- | ```purescript
-- | atomically (modifyTRef counter (_ + 1))
-- | ```
modifyTRef :: forall e a. TRef a -> (a -> a) -> STM e Unit
modifyTRef ref f = do
  a <- readTRef ref
  writeTRef ref (f a)

-- | `TVar` is a synonym for `TRef`, provided for ZIO / Haskell-STM
-- | parity. The two are interchangeable; use whichever name reads
-- | better in context.
type TVar :: Type -> Type
type TVar = TRef

-- | `newTRef` under the `TVar` name. See `newTRef`.
newTVar :: forall e a. a -> STM e (TVar a)
newTVar = newTRef

-- | `readTRef` under the `TVar` name. See `readTRef`.
readTVar :: forall e a. TVar a -> STM e a
readTVar = readTRef

-- | `writeTRef` under the `TVar` name. See `writeTRef`.
writeTVar :: forall e a. TVar a -> a -> STM e Unit
writeTVar = writeTRef

-- | `modifyTRef` under the `TVar` name. See `modifyTRef`.
modifyTVar :: forall e a. TVar a -> (a -> a) -> STM e Unit
modifyTVar = modifyTRef

-- | Abort the current transaction attempt and re-run it once any
-- | `TRef` the transaction read changes.
-- |
-- | A transaction that retries while it has no reads in its log
-- | would deadlock; `atomically` does *not* protect against this
-- | (it has no way to tell a "waiting for a change that can never
-- | come" retry from a legitimate one).
-- |
-- | ```purescript
-- | -- block until the counter is positive
-- | atomically do
-- |   n <- readTRef counter
-- |   check (n > 0)
-- |   modifyTRef counter (_ - 1)
-- | ```
retry :: forall e a. STM e a
retry = STM \_ -> pure TxRetry

-- | `retry` guarded by a predicate. `check true` is `pure unit`;
-- | `check false` is `retry`.
check :: forall e. Boolean -> STM e Unit
check b = if b then pure unit else retry

-- | Inject a typed failure into the transaction. Aborts the
-- | transaction (no writes are applied) and surfaces the failure
-- | on the parent `RIO`'s error row when `atomically` runs.
-- |
-- | ```purescript
-- | atomically do
-- |   balance <- readTRef account
-- |   when (balance < amount)
-- |     (failSTM (Proxy :: _ "insufficient") { have: balance, need: amount })
-- |   writeTRef account (balance - amount)
-- | ```
failSTM
  :: forall sym a v e e1
   . Row.Cons sym v e1 e
  => IsSymbol sym
  => Proxy sym
  -> v
  -> STM e a
failSTM sym v = STM \_ -> pure (TxFailed (Variant.inj sym v))

-- | Re-raise an already-constructed `Variant e` inside an `STM e`
-- | transaction. Useful when a derived primitive (e.g.
-- | `TDeferred`) stores a typed failure and wants to surface it on
-- | the caller's row without re-tagging.
-- |
-- | ```purescript
-- | -- propagate a stored failure or commit normally
-- | atomically do
-- |   mv <- readTRef cell
-- |   case mv of
-- |     Just (Left v) -> throwSTM v
-- |     Just (Right a) -> writeTRef out a
-- |     Nothing -> retry
-- | ```
throwSTM :: forall e a. Variant e -> STM e a
throwSTM v = STM \_ -> pure (TxFailed v)

-- | Run `left`; if it retries, fall through and run `right`. A
-- | typed failure in `left` propagates without falling through.
-- |
-- | The log effect of a fallen-through `left` is rolled back
-- | before `right` runs, so a retried branch leaves no reads or
-- | writes behind.
-- |
-- | ```purescript
-- | atomically (takeOne queueA `orElse` takeOne queueB)
-- | ```
orElse :: forall e a. STM e a -> STM e a -> STM e a
orElse (STM left) (STM right) = STM \log -> do
  oldReads <- Ref.read log.reads
  oldWrites <- Ref.read log.writes
  res <- left log
  case res of
    TxRetry -> do
      Ref.write oldReads log.reads
      Ref.write oldWrites log.writes
      right log
    other -> pure other

-- | Run a transaction. The body executes as a single `Effect`;
-- | on success every staged write commits at once and waiters
-- | registered on written `TRef`s fire. On typed failure the body
-- | aborts and the failure surfaces on the parent's row with no
-- | writes applied. On `retry` the body suspends until one of the
-- | `TRef`s it read is written, then replays.
-- |
-- | Successful and failed transactions consume `Effect` time only.
-- | A retrying transaction awaits an `AVar` signal in `Aff`, so a
-- | parent fiber's `interrupt` cancels the wait at the next async
-- | boundary.
-- |
-- | ```purescript
-- | _ <- atomically do
-- |   balance <- readTRef account
-- |   check (balance >= amount)
-- |   writeTRef account (balance - amount)
-- | ```
atomically :: forall r e a. STM e a -> RIO r e a
atomically (STM body) = mkRIO \_ -> attempt
  where
  attempt = do
    log <- liftEffect do
      reads <- Ref.new []
      writes <- Ref.new []
      pure { reads, writes }
    res <- liftEffect (body log)
    case res of
      TxSuccess a -> do
        liftEffect do
          ws <- Ref.read log.writes
          for_ ws \w -> w.apply
        pure a
      TxFailed v -> rioFail v
      TxRetry -> do
        signal <- AVar.empty
        liftEffect (registerWaiters log signal)
        _ <- AVar.read signal
        attempt

  registerWaiters :: Log -> AVar Unit -> Effect Unit
  registerWaiters log signal = do
    rs <- Ref.read log.reads
    for_ rs \r ->
      r.subscribe (void (EAVar.tryPut unit signal))
