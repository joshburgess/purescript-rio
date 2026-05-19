module Test.RIO.Fiber.STMSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM (TVar)
import RIO.Fiber.STM as STM
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM" do
  it "readTVar reflects the initial value" do
    t <- liftEffect (STM.newTVar 7 :: _ (TVar Int))
    let
      prog :: F.RIO () () Int
      prog = STM.atomically (STM.readTVar t)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 7
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "writeTVar then readTVar in one transaction" do
    t <- liftEffect (STM.newTVar 0 :: _ (TVar Int))
    let
      prog :: F.RIO () () Int
      prog = STM.atomically do
        STM.writeTVar t 42
        STM.readTVar t
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "modifyTVar applies the function" do
    t <- liftEffect (STM.newTVar 10 :: _ (TVar Int))
    let
      prog :: F.RIO () () Int
      prog = do
        STM.atomically (STM.modifyTVar t (_ + 5))
        STM.atomically (STM.readTVar t)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 15
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "swapTVar returns the prior value and installs the new one" do
    t <- liftEffect (STM.newTVar 1 :: _ (TVar Int))
    let
      prog :: F.RIO () () { prev :: Int, now :: Int }
      prog = do
        prev <- STM.atomically (STM.swapTVar t 99)
        now <- STM.atomically (STM.readTVar t)
        pure { prev, now }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.prev `shouldEqual` 1
        r.now `shouldEqual` 99
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "transactions from multiple fibers commit serially" do
    t <- liftEffect (STM.newTVar 0 :: _ (TVar Int))
    let
      bump :: F.RIO () () Unit
      bump = STM.atomically (STM.modifyTVar t (_ + 1))

      prog :: F.RIO () () Int
      prog = do
        _ <- F.parTraverse (\_ -> bump) [ 1, 2, 3, 4, 5 ]
        STM.atomically (STM.readTVar t)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 5
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "retry suspends the transaction until another commits" do
    t <- liftEffect (STM.newTVar 0 :: _ (TVar Int))
    let
      waitForOne :: F.RIO () () Int
      waitForOne = STM.atomically do
        n <- STM.readTVar t
        if n >= 1 then pure n else STM.retry

      prog :: F.RIO () () Int
      prog = do
        waiter <- F.fork waitForOne
        -- give the waiter a chance to register on retry
        F.sleep (Milliseconds 10.0)
        STM.atomically (STM.writeTVar t 1)
        F.join waiter
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
