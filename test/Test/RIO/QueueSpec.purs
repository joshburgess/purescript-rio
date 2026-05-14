module Test.RIO.QueueSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay, forkAff, joinFiber)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Queue (bounded, offer, poll, shutdown, size, take, unbounded)

spec :: Spec Unit
spec = do
  describe "RIO.Queue" do

    describe "unbounded" do
      it "offer / take preserves FIFO order" do
        q <- liftEffect unbounded
        _ <- runRIO' (offer q 1 *> offer q 2 *> offer q 3 :: RIO () () Boolean)
        a <- runRIO' (take q :: RIO () () (Maybe Int))
        b <- runRIO' (take q :: RIO () () (Maybe Int))
        c <- runRIO' (take q :: RIO () () (Maybe Int))
        a `shouldEqual` Just 1
        b `shouldEqual` Just 2
        c `shouldEqual` Just 3

      it "take blocks until a value arrives" do
        q <- liftEffect unbounded
        f <- forkAff (runRIO' (take q :: RIO () () (Maybe Int)))
        delay (Milliseconds 10.0)
        _ <- runRIO' (offer q 42 :: RIO () () Boolean)
        v <- joinFiber f
        v `shouldEqual` Just 42

      it "poll returns Nothing when empty" do
        q <- liftEffect (unbounded :: _ (_ Int))
        r <- runRIO' (poll q :: RIO () () (Maybe Int))
        r `shouldEqual` Nothing

      it "shutdown drains pending takers with Nothing" do
        q <- liftEffect (unbounded :: _ (_ Int))
        f <- forkAff (runRIO' (take q :: RIO () () (Maybe Int)))
        delay (Milliseconds 10.0)
        runRIO' (shutdown q :: RIO () () Unit)
        r <- joinFiber f
        r `shouldEqual` Nothing

    describe "bounded" do
      it "respects capacity by blocking the producer" do
        q <- liftEffect (bounded 1)
        log <- liftEffect (Ref.new ([] :: Array String))
        let
          push :: String -> Int -> RIO () () Unit
          push name n = do
            _ <- offer q n
            liftEffect
              (Ref.modify_ (\xs -> xs <> [ name <> "-done" ]) log)

        f1 <- forkAff (runRIO' (push "first" 1))
        delay (Milliseconds 5.0)
        first <- liftEffect (Ref.read log)
        f2 <- forkAff (runRIO' (push "second" 2))
        delay (Milliseconds 5.0)
        beforeTake <- liftEffect (Ref.read log)
        _ <- runRIO' (take q :: RIO () () (Maybe Int))
        joinFiber f1
        joinFiber f2
        finalLog <- liftEffect (Ref.read log)
        s <- liftEffect (size q)
        first `shouldEqual` [ "first-done" ]
        beforeTake `shouldEqual` [ "first-done" ]
        finalLog `shouldEqual` [ "first-done", "second-done" ]
        s `shouldEqual` 1
