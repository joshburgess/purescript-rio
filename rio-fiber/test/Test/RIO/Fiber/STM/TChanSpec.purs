module Test.RIO.Fiber.STM.TChanSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TChan (TChan)
import RIO.Fiber.STM.TChan as TChan
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TChan" do
  it "new channel is empty" do
    ch <- liftEffect (TChan.new :: _ (TChan Int))
    let
      prog :: F.RIO () () Boolean
      prog = STM.atomically (TChan.isEmptyTChan ch)
    out <- runAff prog {}
    case out of
      Success b -> b `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "writeTChan then readTChan FIFO" do
    ch <- liftEffect (TChan.new :: _ (TChan Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        STM.atomically do
          TChan.writeTChan ch 1
          TChan.writeTChan ch 2
          TChan.writeTChan ch 3
        STM.atomically do
          a <- TChan.readTChan ch
          b <- TChan.readTChan ch
          c <- TChan.readTChan ch
          pure [ a, b, c ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryReadTChan returns Nothing on empty" do
    ch <- liftEffect (TChan.new :: _ (TChan Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = STM.atomically (TChan.tryReadTChan ch)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "peekTChan does not consume" do
    ch <- liftEffect (TChan.new :: _ (TChan Int))
    let
      prog :: F.RIO () () { peeked :: Int, taken :: Int, empty :: Boolean }
      prog = do
        STM.atomically (TChan.writeTChan ch 42)
        peeked <- STM.atomically (TChan.peekTChan ch)
        taken <- STM.atomically (TChan.readTChan ch)
        empty <- STM.atomically (TChan.isEmptyTChan ch)
        pure { peeked, taken, empty }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.peeked `shouldEqual` 42
        r.taken `shouldEqual` 42
        r.empty `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "readTChan blocks until another fiber writes" do
    ch <- liftEffect (TChan.new :: _ (TChan Int))
    let
      prog :: F.RIO () () Int
      prog = do
        consumer <- F.fork (STM.atomically (TChan.readTChan ch))
        F.sleep (Milliseconds 10.0)
        STM.atomically (TChan.writeTChan ch 7)
        F.join consumer
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 7
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
