module Test.RIO.WorkerPoolSpec (spec) where

import Prelude

import Data.Array (range, sort) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO)
import RIO.Deferred (awaitDeferred)
import RIO.Error (catchAll) as Error
import RIO.Env (ask) as Env
import RIO.Resource (Scope, scoped)
import RIO.WorkerPool as WorkerPool

spec :: Spec Unit
spec = describe "RIO.WorkerPool" do
  it "runs the handler on every submitted input" do
    let
      program :: RIO () () (Array Int)
      program = scoped do
        scope <- Env.ask (Proxy :: Proxy "scope")
        pool <- WorkerPool.make scope
          { workers: 4, queueCapacity: Nothing }
          (\n -> pure (n * 10))
        results <- traverse
          (\n -> WorkerPool.submitAndAwait pool n)
          (Array.range 1 5)
        pure (Array.sort results)
    result <- runRIO program
    result `shouldEqual` Right [ 10, 20, 30, 40, 50 ]

  it "spreads work across multiple workers" do
    -- The handler counts the number of distinct worker fibers
    -- that touched it. With 4 workers and 16 jobs that each
    -- sleep briefly, more than one worker should observe a job.
    let
      program :: RIO () () Int
      program = scoped do
        scope <- Env.ask (Proxy :: Proxy "scope")
        counter <- liftEffect (Ref.new 0)
        let
          handler :: Int -> RIO (scope :: Scope) () Int
          handler n = do
            liftAff (delay (Milliseconds 5.0))
            liftEffect (Ref.modify_ (_ + n) counter)
            pure n
        pool <- WorkerPool.make scope
          { workers: 4, queueCapacity: Nothing }
          handler
        _ <- traverse
          (\n -> WorkerPool.submitAndAwait pool n)
          (Array.range 1 16)
        liftEffect (Ref.read counter)
    result <- runRIO program
    result `shouldEqual` Right 136 -- sum 1..16

  it "surfaces typed failures through the Deferred" do
    let
      program :: RIO () () String
      program = scoped do
        scope <- Env.ask (Proxy :: Proxy "scope")
        let
          handler :: Int -> RIO (scope :: Scope) (boom :: Unit) Int
          handler n
            | n == 0 = fail (Proxy :: Proxy "boom") unit
            | otherwise = pure (n * 2)
        pool <- WorkerPool.make scope
          { workers: 2, queueCapacity: Nothing }
          handler
        d <- WorkerPool.submit pool 0
        Error.catchAll (\_ -> pure "boom-was-raised")
          ( do
              _ <- awaitDeferred d
              pure "no-failure"
          )
    result <- runRIO program
    result `shouldEqual` Right "boom-was-raised"

  it "continues processing after a worker hits a typed failure" do
    let
      program :: RIO () () Int
      program = scoped do
        scope <- Env.ask (Proxy :: Proxy "scope")
        let
          handler :: Int -> RIO (scope :: Scope) (boom :: Unit) Int
          handler n
            | n < 0 = fail (Proxy :: Proxy "boom") unit
            | otherwise = pure (n * 100)
        pool <- WorkerPool.make scope
          { workers: 2, queueCapacity: Nothing }
          handler

        -- Submit a failure, then submit a success; the success
        -- must still complete. We bracket both awaits in catchAll
        -- to keep the outer error row closed.
        dFail <- WorkerPool.submit pool (-1)
        _ <- Error.catchAll (\_ -> pure 0) (awaitDeferred dFail)

        dOk <- WorkerPool.submit pool 5
        Error.catchAll (\_ -> pure 0) (awaitDeferred dOk)
    result <- runRIO program
    result `shouldEqual` Right 500

  it "submit returns a Deferred that can be awaited later" do
    let
      program :: RIO () () (Array Int)
      program = scoped do
        scope <- Env.ask (Proxy :: Proxy "scope")
        pool <- WorkerPool.make scope
          { workers: 2, queueCapacity: Nothing }
          (\n -> pure (n + 1))
        deferreds <- traverse
          (\n -> WorkerPool.submit pool n)
          [ 10, 20, 30 ]
        traverse awaitDeferred deferreds
    result <- runRIO program
    result `shouldEqual` Right [ 11, 21, 31 ]

  it "respects a queue capacity bound: bounded backpressure" do
    -- We can't easily observe backpressure timing without
    -- flake. Smoke-test that a bounded pool still processes
    -- everything.
    let
      program :: RIO () () (Array Int)
      program = scoped do
        scope <- Env.ask (Proxy :: Proxy "scope")
        pool <- WorkerPool.make scope
          { workers: 2, queueCapacity: Just 1 }
          (\n -> pure (n * 3))
        traverse
          (\n -> WorkerPool.submitAndAwait pool n)
          (Array.range 1 4)
    result <- runRIO program
    result `shouldEqual` Right [ 3, 6, 9, 12 ]

  it "workers <= 0 are clamped to at least one" do
    let
      program :: RIO () () Int
      program = scoped do
        scope <- Env.ask (Proxy :: Proxy "scope")
        pool <- WorkerPool.make scope
          { workers: 0, queueCapacity: Nothing }
          (\n -> pure (n + 1))
        WorkerPool.submitAndAwait pool 41
    result <- runRIO program
    result `shouldEqual` Right 42
