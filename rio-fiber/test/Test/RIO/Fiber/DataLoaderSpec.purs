module Test.RIO.Fiber.DataLoaderSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.DataLoader as DL
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

type Boom = (boom :: String)

spec :: Spec Unit
spec = describe "rio-fiber: DataLoader" do
  it "coalesces concurrent loads of distinct keys into one batch" do
    calls <- liftEffect (Ref.new [] :: _ (Ref.Ref (Array (Array Int))))
    let
      batch :: Array Int -> F.RIO () () (Map Int Int)
      batch ks = do
        F.liftEffect (Ref.modify_ (\xs -> Array.snoc xs ks) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k (k * 10)) ks))

      prog :: F.RIO () () { results :: Array (Maybe Int), batches :: Array (Array Int) }
      prog = Scope.scoped \scope -> do
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        results <- F.parTraverse (DL.load loader) [ 1, 2, 3, 4 ]
        batches <- F.liftEffect (Ref.read calls)
        pure { results, batches }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.results `shouldEqual` [ Just 10, Just 20, Just 30, Just 40 ]
        Array.length rec.batches `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "dedupes repeated loads of the same key within a batch" do
    calls <- liftEffect (Ref.new [] :: _ (Ref.Ref (Array (Array Int))))
    let
      batch :: Array Int -> F.RIO () () (Map Int Int)
      batch ks = do
        F.liftEffect (Ref.modify_ (\xs -> Array.snoc xs ks) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k (k * 10)) ks))

      prog :: F.RIO () () { results :: Array (Maybe Int), batches :: Array (Array Int) }
      prog = Scope.scoped \scope -> do
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        results <- F.parTraverse (DL.load loader) [ 1, 1, 2, 1, 2 ]
        batches <- F.liftEffect (Ref.read calls)
        pure { results, batches }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.results `shouldEqual` [ Just 10, Just 10, Just 20, Just 10, Just 20 ]
        case rec.batches of
          [ ks ] -> Array.sort ks `shouldEqual` [ 1, 2 ]
          xs -> fail ("expected one batch of [1,2], got " <> show xs)
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "caches resolved keys across sequential loads" do
    calls <- liftEffect (Ref.new 0)
    let
      batch :: Array Int -> F.RIO () () (Map Int Int)
      batch ks = do
        F.liftEffect (Ref.modify_ (_ + 1) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k (k * 10)) ks))

      prog :: F.RIO () () { v1 :: Maybe Int, v2 :: Maybe Int, batches :: Int }
      prog = Scope.scoped \scope -> do
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        v1 <- DL.load loader 7
        v2 <- DL.load loader 7
        batches <- F.liftEffect (Ref.read calls)
        pure { v1, v2, batches }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.v1 `shouldEqual` Just 70
        rec.v2 `shouldEqual` Just 70
        rec.batches `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "chunks oversized queues by maxBatch" do
    calls <- liftEffect (Ref.new [] :: _ (Ref.Ref (Array Int)))
    let
      batch :: Array Int -> F.RIO () () (Map Int Int)
      batch ks = do
        F.liftEffect (Ref.modify_ (\xs -> Array.snoc xs (Array.length ks)) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k k) ks))

      prog :: F.RIO () () { results :: Array (Maybe Int), batchSizes :: Array Int }
      prog = Scope.scoped \scope -> do
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 3
          }
        results <- F.parTraverse (DL.load loader) [ 1, 2, 3, 4, 5, 6, 7 ]
        batchSizes <- F.liftEffect (Ref.read calls)
        pure { results, batchSizes }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.results `shouldEqual` [ Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7 ]
        Array.sort rec.batchSizes `shouldEqual` [ 1, 3, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "missing keys come back as Nothing" do
    let
      batch :: Array Int -> F.RIO () () (Map Int Int)
      batch ks =
        -- Only return entries for even keys
        pure (Map.fromFoldable (map (\k -> Tuple k k) (Array.filter (\k -> mod k 2 == 0) ks)))

      prog :: F.RIO () () (Array (Maybe Int))
      prog = Scope.scoped \scope -> do
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        F.parTraverse (DL.load loader) [ 1, 2, 3, 4 ]

    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ Nothing, Just 2, Nothing, Just 4 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "batch failure surfaces on every waiter and evicts cache" do
    calls <- liftEffect (Ref.new 0)
    let
      batch :: Array Int -> F.RIO () Boom (Map Int Int)
      batch _ = do
        n <- F.liftEffect (Ref.modify (_ + 1) calls)
        if n == 1 then F.fail (Variant.inj (Proxy :: Proxy "boom") "first batch")
        else pure (Map.fromFoldable (map (\k -> Tuple k k) [ 1, 2 ]))

      prog :: F.RIO () Boom { failed :: Boolean, retried :: Maybe Int, totalCalls :: Int }
      prog = Scope.scoped \scope -> do
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        -- First batch fails. Use causeOf because the failure path
        -- inside a forked flush re-raises via failCause (M_CAUSE).
        c1 <- F.causeOf (DL.load loader 1)
        let failed = case c1 of
              Left _ -> true
              Right _ -> false
        -- Second load should retry the batch.
        c2 <- F.causeOf (DL.load loader 1)
        let retried = case c2 of
              Right v -> v
              Left _ -> Nothing
        totalCalls <- F.liftEffect (Ref.read calls)
        pure { failed, retried, totalCalls }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.failed `shouldEqual` true
        rec.retried `shouldEqual` Just 1
        rec.totalCalls `shouldEqual` 2
      Fail v -> Variant.match { boom: \s -> fail ("got Fail boom: " <> s) } v
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "clear evicts a single key" do
    calls <- liftEffect (Ref.new 0)
    let
      batch :: Array Int -> F.RIO () () (Map Int Int)
      batch ks = do
        F.liftEffect (Ref.modify_ (_ + 1) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k k) ks))

      prog :: F.RIO () () { v1 :: Maybe Int, v2 :: Maybe Int, batches :: Int }
      prog = Scope.scoped \scope -> do
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        v1 <- DL.load loader 5
        DL.clear loader 5
        v2 <- DL.load loader 5
        batches <- F.liftEffect (Ref.read calls)
        pure { v1, v2, batches }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.v1 `shouldEqual` Just 5
        rec.v2 `shouldEqual` Just 5
        rec.batches `shouldEqual` 2
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
