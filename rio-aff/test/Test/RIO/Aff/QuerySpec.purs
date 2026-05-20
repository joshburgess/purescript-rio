module Test.RIO.Aff.QuerySpec (spec) where

import Prelude

import Data.Array (filter) as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Aff (Aff, error, throwError)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Concurrency (zipWithPar)
import RIO.Aff.Core (RIO, runRIO)
import RIO.Aff.Query (QueryError(..))
import RIO.Aff.Query as Query

mkBatcher
  :: forall k v
   . { calls :: Ref.Ref (Array (Array k))
     , response :: Array k -> Aff (Map k v)
     }
  -> (Array k -> Aff (Map k v))
mkBatcher cfg keys = do
  liftEffect (Ref.modify_ (\xs -> xs <> [ keys ]) cfg.calls)
  cfg.response keys

run
  :: forall a
   . RIO () (queryError :: QueryError) a
  -> Aff (Either QueryError a)
run p = do
  res <- runRIO p
  pure case res of
    Right a -> Right a
    Left v -> Left
      (Variant.case_ # Variant.on (Proxy :: Proxy "queryError") identity $ v)

spec :: Spec Unit
spec = describe "RIO.Aff.Query (DataLoader)" do
  it "batches concurrent loads of distinct keys into one fetch" do
    calls <- liftEffect (Ref.new [])
    result <- run do
      loader <- Query.makeLoader
        { batchFn: mkBatcher
            { calls
            , response: \ks -> pure
                (Map.fromFoldable (map (\k -> Tuple k (k * 10)) ks))
            }
        , maxBatchSize: Nothing
        , enableCache: false
        }
      zipWithPar Tuple (Query.load loader 1) (Query.load loader 2)
    case result of
      Right (Tuple a b) -> do
        a `shouldEqual` 10
        b `shouldEqual` 20
      Left e -> fail ("expected Right, got: " <> show e)
    recorded <- liftEffect (Ref.read calls)
    case recorded of
      [ batch ] -> batch `shouldEqual` [ 1, 2 ]
      _ -> fail
        ("expected exactly one batch, got: " <> show (map (map show) recorded))

  it "dedupes concurrent loads of the same key" do
    calls <- liftEffect (Ref.new [])
    result <- run do
      loader <- Query.makeLoader
        { batchFn: mkBatcher
            { calls
            , response: \ks -> pure
                (Map.fromFoldable (map (\k -> Tuple k (k * 10)) ks))
            }
        , maxBatchSize: Nothing
        , enableCache: false
        }
      zipWithPar Tuple (Query.load loader 7) (Query.load loader 7)
    result `shouldEqual` Right (Tuple 70 70)
    recorded <- liftEffect (Ref.read calls)
    case recorded of
      [ batch ] -> batch `shouldEqual` [ 7 ]
      _ -> fail
        ("expected exactly one batch, got: " <> show (map (map show) recorded))

  it "raises QueryMissingKey when the batchFn omits a key" do
    calls <- liftEffect (Ref.new ([] :: Array (Array Int)))
    result <- run do
      loader <- Query.makeLoader
        { batchFn: mkBatcher
            { calls
            , response: \_ -> pure (Map.empty :: Map Int Int)
            }
        , maxBatchSize: Nothing
        , enableCache: false
        }
      Query.load loader 42
    case result of
      Left (QueryMissingKey s) -> s `shouldEqual` "42"
      Right v -> fail ("expected QueryMissingKey, got: " <> show (v :: Int))
      Left other -> fail ("expected QueryMissingKey, got: " <> show other)

  it "loadOpt: missing returns Nothing; present returns Just" do
    result <- run do
      loader <- Query.makeLoader
        { batchFn: \ks -> pure
            ( Map.fromFoldable
                ( map (\k -> Tuple k (k + 100))
                    (Array.filter (\k -> k `mod` 2 == 0) ks)
                )
            )
        , maxBatchSize: Nothing
        , enableCache: false
        }
      zipWithPar Tuple
        (Query.loadOpt loader 4)
        (Query.loadOpt loader 7)
    result `shouldEqual` Right (Tuple (Just 104) Nothing)

  it "QueryBatchFailure surfaces when the batchFn rejects" do
    result <- run do
      loader <- Query.makeLoader
        { batchFn: \_ -> throwError (error "db is down") :: Aff (Map Int Int)
        , maxBatchSize: Nothing
        , enableCache: false
        }
      Query.load loader 1
    case result of
      Left (QueryBatchFailure m) -> m `shouldEqual` "db is down"
      Right v -> fail ("expected QueryBatchFailure, got: " <> show (v :: Int))
      Left other -> fail ("expected QueryBatchFailure, got: " <> show other)

  it "cache hits skip the batchFn" do
    calls <- liftEffect (Ref.new [])
    result <- run do
      loader <- Query.makeLoader
        { batchFn: mkBatcher
            { calls
            , response: \ks -> pure
                (Map.fromFoldable (map (\k -> Tuple k (k * 2)) ks))
            }
        , maxBatchSize: Nothing
        , enableCache: true
        }
      _ <- Query.load loader 5
      _ <- Query.load loader 5
      _ <- Query.load loader 5
      Query.size loader
    result `shouldEqual` Right 1
    recorded <- liftEffect (Ref.read calls)
    case recorded of
      [ batch ] -> batch `shouldEqual` [ 5 ]
      _ -> fail
        ("expected exactly one batch, got: " <> show (map (map show) recorded))

  it "prime / clear / clearAll behave as documented" do
    calls <- liftEffect (Ref.new [])
    result <- run do
      loader <- Query.makeLoader
        { batchFn: mkBatcher
            { calls
            , response: \_ -> pure (Map.singleton 1 "fetched")
            }
        , maxBatchSize: Nothing
        , enableCache: true
        }
      Query.prime loader 1 "primed"
      a <- Query.load loader 1
      _ <- Query.clear loader 1
      b <- Query.load loader 1
      _ <- Query.clearAll loader
      n <- Query.size loader
      pure { a, b, n }
    case result of
      Right v -> do
        v.a `shouldEqual` "primed"
        v.b `shouldEqual` "fetched"
        v.n `shouldEqual` 0
      Left e -> fail ("expected Right, got: " <> show e)

  it "maxBatchSize chunks the pending set" do
    calls <- liftEffect (Ref.new [])
    result <- run do
      loader <- Query.makeLoader
        { batchFn: mkBatcher
            { calls
            , response: \ks -> pure
                (Map.fromFoldable (map (\k -> Tuple k k) ks))
            }
        , maxBatchSize: Just 2
        , enableCache: false
        }
      Query.loadMany loader [ 1, 2, 3, 4, 5 ]
    result `shouldEqual` Right [ 1, 2, 3, 4, 5 ]
    recorded <- liftEffect (Ref.read calls)
    case recorded of
      [ b1, b2, b3 ] -> do
        b1 `shouldEqual` [ 1, 2 ]
        b2 `shouldEqual` [ 3, 4 ]
        b3 `shouldEqual` [ 5 ]
      _ -> fail
        ("expected three batches, got: " <> show (map (map show) recorded))
