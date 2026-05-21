-- | Batched, deduped, cached resolution of keyed requests.
-- |
-- | A `DataLoader` collects individual `load k` calls into batches and
-- | hands them all to a user-supplied `batch :: Array k -> RIO r e
-- | (Map k v)` function. Within a single window:
-- |
-- |   * Two concurrent `load k` calls for the same key share one
-- |     in-flight request and one result.
-- |   * Distinct keys submitted close together are coalesced into a
-- |     single `batch` invocation (subject to `maxBatch`).
-- |   * Results are cached by key for the lifetime of the loader, so
-- |     a later `load k` returns the previous result instantly.
-- |
-- | This is the same shape as Facebook's DataLoader / Effect's
-- | RequestResolver pattern: amortise N + 1 queries into a single
-- | round-trip per tick.
-- |
-- | If `batch` raises (typed failure or defect), every waiter for
-- | that batch sees the same `Cause`. The cache entries for those
-- | keys are evicted so a later `load` can retry.
module RIO.Aff.DataLoader
  ( DataLoader
  , clear
  , clearAll
  , load
  , make
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for_)
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Cause (Cause, attemptCause, failCause)
import RIO.Aff.Concurrency (forkScoped)
import RIO.Aff.Core (RIO)
import RIO.Aff.Deferred (Deferred, awaitDeferred, makeDeferred, succeedDeferred)
import RIO.Aff.Internal (mkRIO, unsafeUnRIO)
import RIO.Aff.Resource (Scope)
import RIO.Aff.Semaphore (Semaphore, withPermit)
import RIO.Aff.Semaphore as Sem

-- | A keyed loader. `r` and `e` come from the batch function; `k` is
-- | the request key (must be `Ord`); `v` is the per-key result.
newtype DataLoader r e k v = DataLoader
  { batch :: Array k -> RIO r e (Map k v)
  , window :: Milliseconds
  , maxBatch :: Int
  , entries :: Ref (Map k (Slot e v))
  , queue :: Ref (Array k)
  , flushPending :: Ref Boolean
  , gate :: Semaphore
  , scope :: Scope
  }

-- | A cache slot. The Deferred's success channel carries the full
-- | result, including any `Cause` from a failed batch, so we can
-- | re-raise it identically for every waiter without packaging
-- | per-key failures into the Deferred's typed row.
type Slot e v = Deferred () (Either (Cause e) (Maybe v))

-- | Build a loader. The background flush fibers are forked into
-- | `scope`; when the scope closes, no more batches will fire (but
-- | already-fulfilled cache entries remain readable).
-- |
-- | * `batch` resolves a chunk of keys. Keys missing from the
-- |   returned map are reported back to their callers as `Nothing`.
-- | * `window` is how long the loader waits after the first pending
-- |   key before firing a batch. Use `Milliseconds 0.0` to fire on
-- |   the next tick.
-- | * `maxBatch` caps the number of keys handed to a single `batch`
-- |   call; the loader chunks larger queues into multiple calls.
make
  :: forall r e k v
   . Ord k
  => Scope
  -> { batch :: Array k -> RIO r e (Map k v)
     , window :: Milliseconds
     , maxBatch :: Int
     }
  -> RIO r e (DataLoader r e k v)
make scope cfg = do
  entries <- liftEffect (Ref.new Map.empty)
  queue <- liftEffect (Ref.new [])
  flushPending <- liftEffect (Ref.new false)
  gate <- liftEffect (Sem.make 1)
  pure
    ( DataLoader
        { batch: cfg.batch
        , window: cfg.window
        , maxBatch: cfg.maxBatch
        , entries
        , queue
        , flushPending
        , gate
        , scope
        }
    )

-- | Look up a single key. If an in-flight or completed batch already
-- | covers `k`, returns its result; otherwise enqueues `k` for the
-- | next flush. `Nothing` means the batch returned no value for that
-- | key.
load
  :: forall r e k v
   . Ord k
  => DataLoader r e k v
  -> k
  -> RIO r e (Maybe v)
load dl@(DataLoader rec) k = do
  slot <- withPermit rec.gate do
    entries <- liftEffect (Ref.read rec.entries)
    case Map.lookup k entries of
      Just d -> pure d
      Nothing -> do
        d <- (makeDeferred :: RIO r e (Slot e v))
        liftEffect (Ref.modify_ (Map.insert k d) rec.entries)
        liftEffect (Ref.modify_ (\xs -> Array.snoc xs k) rec.queue)
        pending <- liftEffect (Ref.read rec.flushPending)
        when (not pending) do
          liftEffect (Ref.write true rec.flushPending)
          _ <- forkScoped rec.scope (flushLoop dl)
          pure unit
        pure d
  result <- awaitPure slot
  case result of
    Right mv -> pure mv
    Left c -> failCause c

-- | Drop a single key from the cache. A later `load k` will go back
-- | through `batch`.
clear
  :: forall r e k v
   . Ord k
  => DataLoader r e k v
  -> k
  -> RIO r e Unit
clear (DataLoader rec) k = withPermit rec.gate
  (liftEffect (Ref.modify_ (Map.delete k) rec.entries))

-- | Drop every cached entry. Pending keys (still waiting for their
-- | first flush) are untouched: they were already enqueued and will
-- | resolve normally, but their cache slots are gone so future loads
-- | for the same keys will re-batch.
clearAll
  :: forall r e k v
   . DataLoader r e k v
  -> RIO r e Unit
clearAll (DataLoader rec) = withPermit rec.gate
  (liftEffect (Ref.write Map.empty rec.entries))

-- | Background fiber: wait the configured window, drain the queue,
-- | then dispatch one or more batches honouring `maxBatch`.
flushLoop :: forall r e k v. Ord k => DataLoader r e k v -> RIO r e Unit
flushLoop dl@(DataLoader rec) = do
  liftAff (Aff.delay rec.window)
  keys <- withPermit rec.gate do
    ks <- liftEffect (Ref.read rec.queue)
    liftEffect (Ref.write [] rec.queue)
    liftEffect (Ref.write false rec.flushPending)
    pure ks
  let chunks = chunkBy rec.maxBatch keys
  for_ chunks (runBatch dl)

-- | One batch round-trip for a chunk of keys. The chunk is at most
-- | `maxBatch` keys long.
runBatch
  :: forall r e k v
   . Ord k
  => DataLoader r e k v
  -> Array k
  -> RIO r e Unit
runBatch (DataLoader rec) keys = do
  entries <- liftEffect (Ref.read rec.entries)
  cause <- attemptCause (rec.batch keys)
  case cause of
    Right resultMap ->
      for_ keys \k -> case Map.lookup k entries of
        Just d -> do
          _ <- succeedDeferred d (Right (Map.lookup k resultMap))
          pure unit
        Nothing -> pure unit
    Left c -> do
      -- Batch failed: re-raise the same cause for every waiter and
      -- evict their cache slots so a later load can retry.
      withPermit rec.gate do
        liftEffect
          ( Ref.modify_
              (\m -> Array.foldl (\acc k -> Map.delete k acc) m keys)
              rec.entries
          )
      for_ keys \k -> case Map.lookup k entries of
        Just d -> do
          _ <- succeedDeferred d (Left c)
          pure unit
        Nothing -> pure unit

-- | Await a Deferred whose typed-error row is `()`. The result can
-- | run inside any caller error row because the Deferred itself
-- | cannot produce a typed failure.
awaitPure :: forall r e a. Deferred () a -> RIO r e a
awaitPure d = mkRIO \r -> unsafeUnRIO (awaitDeferred d) r

-- | Slice an array into chunks of at most `n` elements. `n <= 0` is
-- | treated as "one chunk containing everything".
chunkBy :: forall a. Int -> Array a -> Array (Array a)
chunkBy n xs
  | n <= 0 = [ xs ]
  | Array.null xs = []
  | otherwise =
      let { before, after } = Array.splitAt n xs
      in Array.cons before (chunkBy n after)
