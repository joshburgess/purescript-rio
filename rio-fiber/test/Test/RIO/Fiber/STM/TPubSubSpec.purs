module Test.RIO.Fiber.STM.TPubSubSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TPubSub (TPubSub)
import RIO.Fiber.STM.TPubSub as TPS
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TPubSub" do
  it "fans publishes out to every subscriber" do
    ps <- liftEffect (TPS.make 4 :: _ (TPubSub Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        s1 <- STM.atomically (TPS.subscribe ps)
        s2 <- STM.atomically (TPS.subscribe ps)
        STM.atomically (TPS.publish ps 7)
        a <- STM.atomically (TPS.take s1)
        b <- STM.atomically (TPS.take s2)
        pure [ a, b ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 7, 7 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "subscribers see only messages published after subscribe" do
    ps <- liftEffect (TPS.make 4 :: _ (TPubSub Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = do
        STM.atomically (TPS.publish ps 1) -- no subs, dropped
        sub <- STM.atomically (TPS.subscribe ps)
        first <- STM.atomically (TPS.tryTake sub)
        STM.atomically (TPS.publish ps 2)
        STM.atomically (TPS.tryTake sub) -- consume to side-effect
          *> pure first
    -- Re-check: tryTake before publish should be Nothing.
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryPublish reports false when at least one subscriber is full" do
    ps <- liftEffect (TPS.make 1 :: _ (TPubSub Int))
    let
      prog :: F.RIO () () { first :: Boolean, second :: Boolean }
      prog = do
        _ <- STM.atomically (TPS.subscribe ps)
        first <- STM.atomically (TPS.tryPublish ps 1)
        second <- STM.atomically (TPS.tryPublish ps 2)
        pure { first, second }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.first `shouldEqual` true
        r.second `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "publish backpressures (retries) when any subscriber's queue is full" do
    ps <- liftEffect (TPS.make 1 :: _ (TPubSub Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        sub <- STM.atomically (TPS.subscribe ps)
        STM.atomically (TPS.publish ps 1)
        -- The second publish must retry until the consumer drains.
        fib <- F.fork (STM.atomically (TPS.publish ps 2))
        F.sleep (Milliseconds 5.0)
        a <- STM.atomically (TPS.take sub)
        _ <- F.join fib
        b <- STM.atomically (TPS.take sub)
        pure [ a, b ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "unsubscribe removes the slot and stops fanout to it" do
    ps <- liftEffect (TPS.make 4 :: _ (TPubSub Int))
    let
      prog :: F.RIO () () { left :: Int, dropped :: Maybe Int }
      prog = do
        s1 <- STM.atomically (TPS.subscribe ps)
        s2 <- STM.atomically (TPS.subscribe ps)
        STM.atomically (TPS.unsubscribe s2)
        STM.atomically (TPS.publish ps 99)
        left <- STM.atomically (TPS.subscribers ps)
        dropped <- STM.atomically (TPS.tryTake s2)
        _ <- STM.atomically (TPS.take s1) -- drain to avoid leak
        pure { left, dropped }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.left `shouldEqual` 1
        r.dropped `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
