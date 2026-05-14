module Test.RIO.QueueSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay, error, forkAff, joinFiber, killFiber)
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

      it "size reflects offer / take" do
        q <- liftEffect (unbounded :: _ (_ Int))
        s0 <- liftEffect (size q)
        _ <- runRIO' (offer q 1 *> offer q 2 *> offer q 3 :: RIO () () Boolean)
        s3 <- liftEffect (size q)
        _ <- runRIO' (take q :: RIO () () (Maybe Int))
        s2 <- liftEffect (size q)
        s0 `shouldEqual` 0
        s3 `shouldEqual` 3
        s2 `shouldEqual` 2

      it "poll returns Just and removes the item when non-empty" do
        q <- liftEffect unbounded
        _ <- runRIO' (offer q 1 *> offer q 2 :: RIO () () Boolean)
        a <- runRIO' (poll q :: RIO () () (Maybe Int))
        b <- runRIO' (poll q :: RIO () () (Maybe Int))
        c <- runRIO' (poll q :: RIO () () (Maybe Int))
        a `shouldEqual` Just 1
        b `shouldEqual` Just 2
        c `shouldEqual` Nothing

      it "after shutdown, take drains buffered items then returns Nothing" do
        q <- liftEffect unbounded
        _ <- runRIO' (offer q 1 *> offer q 2 :: RIO () () Boolean)
        runRIO' (shutdown q :: RIO () () Unit)
        a <- runRIO' (take q :: RIO () () (Maybe Int))
        b <- runRIO' (take q :: RIO () () (Maybe Int))
        c <- runRIO' (take q :: RIO () () (Maybe Int))
        a `shouldEqual` Just 1
        b `shouldEqual` Just 2
        c `shouldEqual` Nothing

      it "after shutdown, offer returns false" do
        q <- liftEffect (unbounded :: _ (_ Int))
        runRIO' (shutdown q :: RIO () () Unit)
        ok <- runRIO' (offer q 99 :: RIO () () Boolean)
        ok `shouldEqual` false

      it "a killed taker removes itself from the takers list" do
        -- Module docstring promises that the takers list lets "an
        -- interrupted taker remove itself cleanly". If the kill
        -- did not run the registered canceler, a later `offer`
        -- would try to deliver to the dead taker (the resume is a
        -- no-op) and the value would be lost; the subsequent
        -- `take` would then block forever. Pin the cleanup by
        -- offering after the kill and observing that the value
        -- is buffered (a fresh take retrieves it).
        q <- liftEffect (unbounded :: _ (_ Int))
        f <- forkAff (runRIO' (take q :: RIO () () (Maybe Int)))
        delay (Milliseconds 5.0)
        killFiber (error "test-cancel") f
        delay (Milliseconds 5.0)
        ok <- runRIO' (offer q 7 :: RIO () () Boolean)
        ok `shouldEqual` true
        v <- runRIO' (take q :: RIO () () (Maybe Int))
        v `shouldEqual` Just 7

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

      it "shutdown wakes blocked offerers with false" do
        q <- liftEffect (bounded 1)
        _ <- runRIO' (offer q 1 :: RIO () () Boolean)
        -- this offer parks because the queue is at capacity
        f <- forkAff (runRIO' (offer q 2 :: RIO () () Boolean))
        delay (Milliseconds 5.0)
        runRIO' (shutdown q :: RIO () () Unit)
        result <- joinFiber f
        result `shouldEqual` false

      it "a killed offerer removes itself from the offerers list" do
        -- Symmetric to the taker-cleanup contract: a bounded queue
        -- parks producers when at capacity. The Canceler that
        -- `offer` registers must remove the producer entry on
        -- kill; otherwise a later `take` would wake the dead
        -- offerer (no-op resume) and the parked value would
        -- silently land in `items` without a live signal that the
        -- offer succeeded. Pin the cleanup by killing a parked
        -- offerer and observing that the queue only carries the
        -- one live item (the second offer's value never makes it
        -- in).
        q <- liftEffect (bounded 1)
        _ <- runRIO' (offer q 1 :: RIO () () Boolean)
        f <- forkAff (runRIO' (offer q 99 :: RIO () () Boolean))
        delay (Milliseconds 5.0)
        killFiber (error "test-cancel") f
        delay (Milliseconds 5.0)
        a <- runRIO' (take q :: RIO () () (Maybe Int))
        b <- runRIO' (poll q :: RIO () () (Maybe Int))
        a `shouldEqual` Just 1
        b `shouldEqual` Nothing
