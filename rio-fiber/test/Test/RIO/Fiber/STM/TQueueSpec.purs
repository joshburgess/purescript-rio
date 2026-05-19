module Test.RIO.Fiber.STM.TQueueSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TQueue (TQueue)
import RIO.Fiber.STM.TQueue as TQ
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TQueue" do
  it "new queue is empty and reports its capacity" do
    q <- liftEffect (TQ.new 4 :: _ (TQueue Int))
    let
      prog :: F.RIO () () { empty :: Boolean, cap :: Int }
      prog = do
        empty <- STM.atomically (TQ.isEmptyTQueue q)
        let cap = TQ.capacityTQueue q
        pure { empty, cap }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.empty `shouldEqual` true
        r.cap `shouldEqual` 4
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "write then read FIFO" do
    q <- liftEffect (TQ.new 8 :: _ (TQueue Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        STM.atomically do
          TQ.writeTQueue q 10
          TQ.writeTQueue q 20
          TQ.writeTQueue q 30
        STM.atomically do
          a <- TQ.readTQueue q
          b <- TQ.readTQueue q
          c <- TQ.readTQueue q
          pure [ a, b, c ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 10, 20, 30 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryReadTQueue returns Nothing on empty" do
    q <- liftEffect (TQ.new 2 :: _ (TQueue Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = STM.atomically (TQ.tryReadTQueue q)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryWriteTQueue returns false when full" do
    q <- liftEffect (TQ.new 2 :: _ (TQueue Int))
    let
      prog :: F.RIO () () (Array Boolean)
      prog = STM.atomically do
        a <- TQ.tryWriteTQueue q 1
        b <- TQ.tryWriteTQueue q 2
        c <- TQ.tryWriteTQueue q 3
        pure [ a, b, c ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ true, true, false ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "writeTQueue blocks when full until a reader drains" do
    q <- liftEffect (TQ.new 1 :: _ (TQueue Int))
    let
      prog :: F.RIO () () Int
      prog = do
        STM.atomically (TQ.writeTQueue q 1)
        -- A second writer must suspend until something is read.
        producer <- F.fork (STM.atomically (TQ.writeTQueue q 2))
        F.sleep (Milliseconds 10.0)
        first <- STM.atomically (TQ.readTQueue q)
        _ <- F.join producer
        second <- STM.atomically (TQ.readTQueue q)
        pure (first + second)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 3
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "readTQueue blocks until another fiber writes" do
    q <- liftEffect (TQ.new 4 :: _ (TQueue Int))
    let
      prog :: F.RIO () () Int
      prog = do
        consumer <- F.fork (STM.atomically (TQ.readTQueue q))
        F.sleep (Milliseconds 10.0)
        STM.atomically (TQ.writeTQueue q 99)
        F.join consumer
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 99
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "check retries until the predicate holds" do
    q <- liftEffect (TQ.new 4 :: _ (TQueue Int))
    let
      prog :: F.RIO () () Int
      prog = do
        -- This transaction waits until at least 3 elements arrive.
        consumer <- F.fork $ STM.atomically do
          len <- TQ.lengthTQueue q
          STM.check (len >= 3)
          TQ.readTQueue q
        F.sleep (Milliseconds 5.0)
        STM.atomically (TQ.writeTQueue q 1)
        F.sleep (Milliseconds 5.0)
        STM.atomically (TQ.writeTQueue q 2)
        F.sleep (Milliseconds 5.0)
        STM.atomically (TQ.writeTQueue q 3)
        F.join consumer
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
