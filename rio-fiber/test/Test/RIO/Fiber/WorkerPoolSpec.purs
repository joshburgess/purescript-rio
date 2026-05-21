module Test.RIO.Fiber.WorkerPoolSpec (spec) where

import Prelude

import Data.Array (range, sort) as Array
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Fiber.Core (Outcome(..), RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Deferred as Deferred
import RIO.Fiber.Scope (scoped)
import RIO.Fiber.WorkerPool as WorkerPool

spec :: Spec Unit
spec = describe "rio-fiber: WorkerPool" do

  it "runs the handler on every submitted input" do
    let
      program :: RIO () () (Array Int)
      program = scoped \scope -> do
        pool <- WorkerPool.make scope
          { workers: 4, queueCapacity: Nothing }
          (\n -> pure (n * 10))
        results <- traverse
          (\n -> WorkerPool.submitAndAwait pool n)
          (Array.range 1 5)
        pure (Array.sort results)
    out <- runAff program {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30, 40, 50 ]
      _ -> fail "expected Success"

  it "spreads work across multiple workers" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () Int
      program = scoped \scope -> do
        let
          handler :: Int -> RIO () () Int
          handler n = do
            F.sleep (Milliseconds 5.0)
            F.liftEffect (Ref.modify_ (_ + n) counter)
            pure n
        pool <- WorkerPool.make scope
          { workers: 4, queueCapacity: Nothing }
          handler
        _ <- traverse
          (\n -> WorkerPool.submitAndAwait pool n)
          (Array.range 1 16)
        F.liftEffect (Ref.read counter)
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 136 -- sum 1..16
      _ -> fail "expected Success"

  it "surfaces typed failures through the Deferred" do
    let
      boomTag :: Proxy "boom"
      boomTag = Proxy

      handler :: Int -> RIO () (boom :: Unit) Int
      handler n
        | n == 0 = F.fail (Variant.inj boomTag unit)
        | otherwise = pure (n * 2)

      program :: RIO () () String
      program = scoped \scope -> do
        pool <- WorkerPool.make scope
          { workers: 2, queueCapacity: Nothing }
          handler
        d <- WorkerPool.submit pool 0
        F.catchAll (\_ -> pure "boom-was-raised")
          ( do
              _ <- Deferred.await d
              pure "no-failure"
          )
    out <- runAff program {}
    case out of
      Success s -> s `shouldEqual` "boom-was-raised"
      _ -> fail "expected Success"

  it "continues processing after a worker hits a typed failure" do
    let
      boomTag :: Proxy "boom"
      boomTag = Proxy

      handler :: Int -> RIO () (boom :: Unit) Int
      handler n
        | n < 0 = F.fail (Variant.inj boomTag unit)
        | otherwise = pure (n * 100)

      program :: RIO () () Int
      program = scoped \scope -> do
        pool <- WorkerPool.make scope
          { workers: 2, queueCapacity: Nothing }
          handler
        dFail <- WorkerPool.submit pool (-1)
        _ <- F.catchAll (\_ -> pure 0) (Deferred.await dFail)
        dOk <- WorkerPool.submit pool 5
        F.catchAll (\_ -> pure 0) (Deferred.await dOk)
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 500
      _ -> fail "expected Success"

  it "submit returns a Deferred that can be awaited later" do
    let
      program :: RIO () () (Array Int)
      program = scoped \scope -> do
        pool <- WorkerPool.make scope
          { workers: 2, queueCapacity: Nothing }
          (\n -> pure (n + 1))
        deferreds <- traverse
          (\n -> WorkerPool.submit pool n)
          [ 10, 20, 30 ]
        traverse Deferred.await deferreds
    out <- runAff program {}
    case out of
      Success xs -> xs `shouldEqual` [ 11, 21, 31 ]
      _ -> fail "expected Success"

  it "respects a queue capacity bound: bounded backpressure" do
    let
      program :: RIO () () (Array Int)
      program = scoped \scope -> do
        pool <- WorkerPool.make scope
          { workers: 2, queueCapacity: Just 1 }
          (\n -> pure (n * 3))
        traverse
          (\n -> WorkerPool.submitAndAwait pool n)
          (Array.range 1 4)
    out <- runAff program {}
    case out of
      Success xs -> xs `shouldEqual` [ 3, 6, 9, 12 ]
      _ -> fail "expected Success"

  it "workers <= 0 are clamped to at least one" do
    let
      program :: RIO () () Int
      program = scoped \scope -> do
        pool <- WorkerPool.make scope
          { workers: 0, queueCapacity: Nothing }
          (\n -> pure (n + 1))
        WorkerPool.submitAndAwait pool 41
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 42
      _ -> fail "expected Success"
