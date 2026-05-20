-- | A batching loader: many `load` calls in the same tick fan
-- | into a single `batchFn` call.
-- |
-- | The shape mirrors the DataLoader pattern used by GraphQL
-- | resolvers. Each fiber that wants a value calls
-- | `load loader key`. The loader queues the key in a pending
-- | set; on the next macrotask (`Aff.delay (Milliseconds 0.0)`),
-- | it flushes the pending set through `batchFn :: Array k -> Aff (Map k v)`
-- | and resolves every awaiter with the right value. Concurrent
-- | `load` calls for the same key dedupe to a single `Deferred`;
-- | a process-wide cache (optional) can also serve repeats
-- | without re-fetching.
-- |
-- | ```purescript
-- | userLoader <- Query.makeLoader
-- |   { batchFn: \ids -> do
-- |       rows <- DB.fetchUsers ids
-- |       pure (Map.fromFoldable (map (\u -> Tuple u.id u) rows))
-- |   , maxBatchSize: Just 100
-- |   , enableCache: true
-- |   }
-- |
-- | Concurrency.zipWithPar Tuple
-- |   (Query.load userLoader 1)
-- |   (Query.load userLoader 2)
-- | -- two `load` calls, one batched fetch of [1, 2]
-- | ```
-- |
-- | ## Failure model
-- |
-- | The batch function returns whichever keys it could resolve;
-- | any key absent from the returned `Map` is reported as
-- | `QueryMissingKey` to the awaiter. If the `Aff` itself
-- | rejects, *every* awaiter in the batch sees
-- | `QueryBatchFailure` carrying the host exception's message.
-- |
-- | ## Cache invalidation
-- |
-- | `prime` seeds the cache without fetching; `clear` drops a
-- | single key; `clearAll` drops every cached entry. Pending
-- | requests in flight are *not* affected by `clear`; reach for
-- | `clearAll` after a write to evict stale entries.
module RIO.Aff.Query
  ( Loader
  , Config
  , QueryError(..)
  , makeLoader
  , load
  , loadOpt
  , loadMany
  , prime
  , clear
  , clearAll
  , size
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Variant as Variant
import Effect.Aff (Aff, attempt, delay) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (message) as Exception
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Concurrency (fork) as Concurrency
import RIO.Aff.Core (RIO, catchSome, mapError)
import RIO.Aff.Deferred
  ( Deferred
  , awaitDeferred
  , failDeferred
  , makeDeferred
  , succeedDeferred
  )

-- | A handle to a configured loader. Allocate with `makeLoader`;
-- | use through `load`, `loadOpt`, `loadMany`, `prime`, `clear`,
-- | `clearAll`, `size`.
newtype Loader k v = Loader (Ref (State k v))

type State k v =
  { batchFn :: Array k -> Aff.Aff (Map k v)
  , maxBatchSize :: Maybe Int
  , cacheEnabled :: Boolean
  , cache :: Map k v
  , pending :: Map k (Deferred (queryError :: QueryError) v)
  , scheduled :: Boolean
  }

-- | Configuration for a fresh loader.
-- |
-- |   * `batchFn` runs a single batch fetch. The returned `Map`
-- |     resolves whichever keys the source could supply; keys
-- |     missing from the map propagate `QueryMissingKey` to the
-- |     awaiter.
-- |   * `maxBatchSize` caps the number of keys per `batchFn`
-- |     invocation. The loader splits the pending set into
-- |     consecutive chunks of this size and fires them
-- |     sequentially. `Nothing` means no cap.
-- |   * `enableCache` controls whether resolved values stay
-- |     resident for the loader's lifetime. Disable when the
-- |     loader is request-scoped and you want every load to hit
-- |     the source.
type Config k v =
  { batchFn :: Array k -> Aff.Aff (Map k v)
  , maxBatchSize :: Maybe Int
  , enableCache :: Boolean
  }

-- | The error tag raised by `load` / `loadMany`.
-- |
-- |   * `QueryMissingKey` fires when a key was not present in
-- |     the `batchFn` result. The string is the show'n key.
-- |   * `QueryBatchFailure` fires when the `Aff` returned by
-- |     `batchFn` rejects. Every awaiter in the affected batch
-- |     sees the same message.
data QueryError
  = QueryMissingKey String
  | QueryBatchFailure String

derive instance eqQueryError :: Eq QueryError

instance showQueryError :: Show QueryError where
  show = case _ of
    QueryMissingKey k -> "(QueryMissingKey " <> show k <> ")"
    QueryBatchFailure m -> "(QueryBatchFailure " <> show m <> ")"

-- | Allocate a fresh loader. The cache (if enabled) starts
-- | empty; nothing is fetched until a `load` happens.
makeLoader :: forall r e k v. Config k v -> RIO r e (Loader k v)
makeLoader cfg = do
  ref <- liftEffect $ Ref.new
    { batchFn: cfg.batchFn
    , maxBatchSize: cfg.maxBatchSize
    , cacheEnabled: cfg.enableCache
    , cache: Map.empty
    , pending: Map.empty
    , scheduled: false
    }
  pure (Loader ref)

-- | Fetch one key. Concurrent calls for the same key share a
-- | single `Deferred`; multiple distinct keys queued in the same
-- | tick fan into a single `batchFn` call.
load
  :: forall r e k v
   . Ord k
  => Show k
  => Loader k v
  -> k
  -> RIO r (queryError :: QueryError | e) v
load loader k = do
  d <- enqueue loader k
  awaitOpen d

-- | Like `load`, but returns `Nothing` instead of raising
-- | `QueryMissingKey`. `QueryBatchFailure` still surfaces - a
-- | rejected batch is not the same as a missing key.
loadOpt
  :: forall r e k v
   . Ord k
  => Show k
  => Loader k v
  -> k
  -> RIO r (queryError :: QueryError | e) (Maybe v)
loadOpt loader k = do
  d <- enqueue loader k
  catchSome
    ( Variant.on (Proxy :: Proxy "queryError")
        ( case _ of
            QueryMissingKey _ -> Just (pure Nothing)
            _ -> Nothing
        )
        (\_ -> Nothing)
    )
    (Just <$> awaitOpen d)

-- | Fetch many keys in parallel through the same loader. The
-- | result preserves input order. If any key surfaces an error,
-- | the error propagates; partial results are not returned.
loadMany
  :: forall r e k v
   . Ord k
  => Show k
  => Loader k v
  -> Array k
  -> RIO r (queryError :: QueryError | e) (Array v)
loadMany loader ks = do
  ds <- traverse (enqueue loader) ks
  traverse awaitOpen ds

-- | Pre-populate the cache without going through `batchFn`.
-- | Useful when the value is already in hand (a write-through
-- | from another query, a seeded fixture). No-op when the
-- | loader's cache is disabled.
prime
  :: forall r e k v
   . Ord k
  => Loader k v
  -> k
  -> v
  -> RIO r e Unit
prime (Loader ref) k v = liftEffect $ Ref.modify_
  ( \s ->
      if s.cacheEnabled then s { cache = Map.insert k v s.cache }
      else s
  )
  ref

-- | Drop a single cached entry. Pending fetches for the same
-- | key are unaffected; call before they queue, not after.
clear
  :: forall r e k v
   . Ord k
  => Loader k v
  -> k
  -> RIO r e Unit
clear (Loader ref) k = liftEffect $ Ref.modify_
  (\s -> s { cache = Map.delete k s.cache })
  ref

-- | Drop every cached entry.
clearAll :: forall r e k v. Loader k v -> RIO r e Unit
clearAll (Loader ref) = liftEffect $ Ref.modify_
  (\s -> s { cache = Map.empty })
  ref

-- | The current cache size. Advisory.
size :: forall r e k v. Loader k v -> RIO r e Int
size (Loader ref) =
  liftEffect (_.cache >>> Map.size <$> Ref.read ref)

-- Internal: await a Deferred whose error row is fixed to the
-- closed `(queryError :: QueryError)` from inside an open-row
-- caller. `Variant.expand` widens the underlying Variant; this
-- is safe because the inhabitants of a closed row are by
-- construction a subset of any open row that contains it.
awaitOpen
  :: forall r e a
   . Deferred (queryError :: QueryError) a
  -> RIO r (queryError :: QueryError | e) a
awaitOpen d = mapError Variant.expand (awaitDeferred d)

-- Internal: enqueue one key, returning a Deferred the caller
-- should await on. Schedules a flush if not already scheduled.
enqueue
  :: forall r e k v
   . Ord k
  => Show k
  => Loader k v
  -> k
  -> RIO r e (Deferred (queryError :: QueryError) v)
enqueue (Loader ref) k = do
  -- Fast paths: cached, or already pending.
  fast <- liftEffect $ Ref.read ref >>= \s -> case Map.lookup k s.cache of
    Just v -> pure (Just (Left v))
    Nothing -> case Map.lookup k s.pending of
      Just d -> pure (Just (Right d))
      Nothing -> pure Nothing

  case fast of
    Just (Left v) -> do
      -- Cached: synthesise a pre-resolved Deferred so the caller
      -- can `await` uniformly.
      d <- makeDeferred
      _ <- succeedDeferred d v
      pure d
    Just (Right d) -> pure d
    Nothing -> do
      d <- makeDeferred
      needFlush <- liftEffect $ Ref.modify'
        ( \s ->
            -- Re-check under the same ref read in case another
            -- fiber filled the slot between the fast path and
            -- here. Map insertion is "last write wins" but
            -- pending dedup matters; if there is already a
            -- waiter, hand it back.
            case Map.lookup k s.pending of
              Just existing ->
                { state: s
                , value: { d: existing, needFlush: false }
                }
              Nothing ->
                let
                  s' = s { pending = Map.insert k d s.pending }
                in
                  if s.scheduled then
                    { state: s', value: { d, needFlush: false } }
                  else
                    { state: s' { scheduled = true }
                    , value: { d, needFlush: true }
                    }
        )
        ref

      when needFlush.needFlush
        (void (Concurrency.fork (flush (Loader ref))))

      pure needFlush.d

-- Internal: drain the pending set and resolve every Deferred.
flush
  :: forall r e k v
   . Ord k
  => Show k
  => Loader k v
  -> RIO r e Unit
flush (Loader ref) = do
  -- Yield to the next macrotask so that any `load` calls already
  -- in flight on this tick can register before we read the
  -- pending set.
  liftAff (Aff.delay (Milliseconds 0.0))

  state <- liftEffect $ Ref.modify'
    ( \s ->
        { state: s { pending = Map.empty, scheduled = false }
        , value: s
        }
    )
    ref

  let
    pending = state.pending
    keys = Array.fromFoldable (Map.keys pending)
    chunks = case state.maxBatchSize of
      Nothing -> [ keys ]
      Just n
        | n <= 0 -> [ keys ]
        | otherwise -> chunkBy n keys

  for_ chunks \batch -> do
    attempted <- liftAff (Aff.attempt (state.batchFn batch))
    case attempted of
      Left err -> do
        let msg = Exception.message err
        for_ batch \k -> case Map.lookup k pending of
          Nothing -> pure unit
          Just d -> void $ failDeferred d
            ( Variant.inj (Proxy :: Proxy "queryError")
                (QueryBatchFailure msg)
            )
      Right resolved -> do
        for_ batch \k -> case Map.lookup k pending of
          Nothing -> pure unit
          Just d -> case Map.lookup k resolved of
            Just v -> do
              when state.cacheEnabled $ liftEffect $ Ref.modify_
                (\s -> s { cache = Map.insert k v s.cache })
                ref
              void (succeedDeferred d v)
            Nothing -> void $ failDeferred d
              ( Variant.inj (Proxy :: Proxy "queryError")
                  (QueryMissingKey (show k))
              )

chunkBy :: forall a. Int -> Array a -> Array (Array a)
chunkBy n xs
  | n <= 0 = [ xs ]
  | otherwise = case Array.uncons xs of
      Nothing -> []
      Just _ ->
        let
          h = Array.take n xs
          t = Array.drop n xs
        in
          [ h ] <> chunkBy n t
