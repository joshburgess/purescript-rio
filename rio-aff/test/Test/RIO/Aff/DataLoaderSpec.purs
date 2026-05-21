module Test.RIO.Aff.DataLoaderSpec (spec) where

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
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Cause (Cause(..), attemptCause)
import RIO.Aff.Concurrency (parTraverse)
import RIO.Aff.Core (ask, fail, runRIO, runRIO') as R
import RIO.Aff.Core (RIO)
import RIO.Aff.DataLoader as DL
import RIO.Aff.Resource (scoped)

type Boom = (boom :: String)

spec :: Spec Unit
spec = describe "RIO.Aff.DataLoader" do
  it "coalesces concurrent loads of distinct keys into one batch" do
    calls <- liftEffect (Ref.new ([] :: Array (Array Int)))
    let
      batch :: forall r. Array Int -> RIO r () (Map Int Int)
      batch ks = do
        liftEffect (Ref.modify_ (\xs -> Array.snoc xs ks) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k (k * 10)) ks))

      program
        :: RIO () ()
             { results :: Array (Maybe Int), batches :: Array (Array Int) }
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        results <- parTraverse (DL.load loader) [ 1, 2, 3, 4 ]
        batches <- liftEffect (Ref.read calls)
        pure { results, batches }

    rec <- R.runRIO' program
    rec.results `shouldEqual` [ Just 10, Just 20, Just 30, Just 40 ]
    Array.length rec.batches `shouldEqual` 1

  it "dedupes repeated loads of the same key within a batch" do
    calls <- liftEffect (Ref.new ([] :: Array (Array Int)))
    let
      batch :: forall r. Array Int -> RIO r () (Map Int Int)
      batch ks = do
        liftEffect (Ref.modify_ (\xs -> Array.snoc xs ks) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k (k * 10)) ks))

      program
        :: RIO () ()
             { results :: Array (Maybe Int), batches :: Array (Array Int) }
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        results <- parTraverse (DL.load loader) [ 1, 1, 2, 1, 2 ]
        batches <- liftEffect (Ref.read calls)
        pure { results, batches }

    rec <- R.runRIO' program
    rec.results `shouldEqual` [ Just 10, Just 10, Just 20, Just 10, Just 20 ]
    case rec.batches of
      [ ks ] -> Array.sort ks `shouldEqual` [ 1, 2 ]
      xs -> fail ("expected one batch of [1,2], got " <> show xs)

  it "caches resolved keys across sequential loads" do
    calls <- liftEffect (Ref.new 0)
    let
      batch :: forall r. Array Int -> RIO r () (Map Int Int)
      batch ks = do
        liftEffect (Ref.modify_ (_ + 1) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k (k * 10)) ks))

      program
        :: RIO () ()
             { v1 :: Maybe Int, v2 :: Maybe Int, batches :: Int }
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        v1 <- DL.load loader 7
        v2 <- DL.load loader 7
        batches <- liftEffect (Ref.read calls)
        pure { v1, v2, batches }

    rec <- R.runRIO' program
    rec.v1 `shouldEqual` Just 70
    rec.v2 `shouldEqual` Just 70
    rec.batches `shouldEqual` 1

  it "chunks oversized queues by maxBatch" do
    calls <- liftEffect (Ref.new ([] :: Array Int))
    let
      batch :: forall r. Array Int -> RIO r () (Map Int Int)
      batch ks = do
        liftEffect
          (Ref.modify_ (\xs -> Array.snoc xs (Array.length ks)) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k k) ks))

      program
        :: RIO () ()
             { results :: Array (Maybe Int), batchSizes :: Array Int }
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 3
          }
        results <- parTraverse (DL.load loader) [ 1, 2, 3, 4, 5, 6, 7 ]
        batchSizes <- liftEffect (Ref.read calls)
        pure { results, batchSizes }

    rec <- R.runRIO' program
    rec.results `shouldEqual`
      [ Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7 ]
    Array.sort rec.batchSizes `shouldEqual` [ 1, 3, 3 ]

  it "missing keys come back as Nothing" do
    let
      batch :: forall r. Array Int -> RIO r () (Map Int Int)
      batch ks =
        pure
          ( Map.fromFoldable
              ( map (\k -> Tuple k k)
                  (Array.filter (\k -> mod k 2 == 0) ks)
              )
          )

      program :: RIO () () (Array (Maybe Int))
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        parTraverse (DL.load loader) [ 1, 2, 3, 4 ]

    xs <- R.runRIO' program
    xs `shouldEqual` [ Nothing, Just 2, Nothing, Just 4 ]

  it "batch failure surfaces on every waiter and evicts cache" do
    calls <- liftEffect (Ref.new 0)
    let
      batch :: forall r. Array Int -> RIO r Boom (Map Int Int)
      batch _ = do
        n <- liftEffect (Ref.modify (_ + 1) calls)
        if n == 1 then
          R.fail (Proxy :: Proxy "boom") "first batch"
        else
          pure (Map.fromFoldable (map (\k -> Tuple k k) [ 1, 2 ]))

      program
        :: RIO () Boom
             { failed :: Boolean
             , retried :: Maybe Int
             , totalCalls :: Int
             }
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        c1 <- attemptCause (DL.load loader 1)
        let
          failed = case c1 of
            Left _ -> true
            Right _ -> false
        c2 <- attemptCause (DL.load loader 1)
        let
          retried = case c2 of
            Right v -> v
            Left _ -> Nothing
        totalCalls <- liftEffect (Ref.read calls)
        pure { failed, retried, totalCalls }

    out <- R.runRIO program
    case out of
      Right rec -> do
        rec.failed `shouldEqual` true
        case rec.failed, rec.retried of
          true, Just 1 -> pure unit
          _, _ -> fail ("unexpected outcome: " <> show rec.retried)
        rec.totalCalls `shouldEqual` 2
      Left _ -> fail "expected Right; failure was meant to be absorbed by attemptCause"

  it "batch failure reports a Fail cause (not Die)" do
    let
      batch :: forall r. Array Int -> RIO r Boom (Map Int Int)
      batch _ = R.fail (Proxy :: Proxy "boom") "nope"

      program :: RIO () Boom String
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        c <- attemptCause (DL.load loader 1)
        pure case c of
          Right _ -> "right"
          Left (Fail v) ->
            Variant.match { boom: \s -> "fail-boom:" <> s } v
          Left (Die _) -> "die"
          Left (Parallel _ _) -> "parallel"
          Left (Sequential _ _) -> "sequential"

    out <- R.runRIO program
    case out of
      Right s -> s `shouldEqual` "fail-boom:nope"
      Left _ -> fail "expected Right"

  it "clear evicts a single key" do
    calls <- liftEffect (Ref.new 0)
    let
      batch :: forall r. Array Int -> RIO r () (Map Int Int)
      batch ks = do
        liftEffect (Ref.modify_ (_ + 1) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k k) ks))

      program
        :: RIO () ()
             { v1 :: Maybe Int, v2 :: Maybe Int, batches :: Int }
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        v1 <- DL.load loader 5
        DL.clear loader 5
        v2 <- DL.load loader 5
        batches <- liftEffect (Ref.read calls)
        pure { v1, v2, batches }

    rec <- R.runRIO' program
    rec.v1 `shouldEqual` Just 5
    rec.v2 `shouldEqual` Just 5
    rec.batches `shouldEqual` 2

  it "clearAll empties the cache" do
    calls <- liftEffect (Ref.new 0)
    let
      batch :: forall r. Array Int -> RIO r () (Map Int Int)
      batch ks = do
        liftEffect (Ref.modify_ (_ + 1) calls)
        pure (Map.fromFoldable (map (\k -> Tuple k k) ks))

      program
        :: RIO () ()
             { v1 :: Maybe Int, v2 :: Maybe Int, batches :: Int }
      program = scoped do
        scope <- R.ask (Proxy :: Proxy "scope")
        loader <- DL.make scope
          { batch
          , window: Milliseconds 5.0
          , maxBatch: 100
          }
        v1 <- DL.load loader 3
        DL.clearAll loader
        v2 <- DL.load loader 3
        batches <- liftEffect (Ref.read calls)
        pure { v1, v2, batches }

    rec <- R.runRIO' program
    rec.v1 `shouldEqual` Just 3
    rec.v2 `shouldEqual` Just 3
    rec.batches `shouldEqual` 2
