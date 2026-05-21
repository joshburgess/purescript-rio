module Test.RIO.Fiber.STM.TDeferredSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TDeferred (TDeferred)
import RIO.Fiber.STM.TDeferred as TDeferred
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TDeferred" do
  it "poll returns Nothing on an empty cell" do
    d <- liftEffect (TDeferred.make :: _ (TDeferred Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = STM.atomically (TDeferred.poll d)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "complete sets the cell and returns true; await reads the value" do
    d <- liftEffect (TDeferred.make :: _ (TDeferred Int))
    let
      prog :: F.RIO () () { won :: Boolean, got :: Int }
      prog = do
        won <- STM.atomically (TDeferred.complete d 42)
        got <- STM.atomically (TDeferred.await d)
        pure { won, got }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.won `shouldEqual` true
        r.got `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "the second complete returns false and the cell keeps the first value" do
    d <- liftEffect (TDeferred.make :: _ (TDeferred Int))
    let
      prog :: F.RIO () () { won1 :: Boolean, won2 :: Boolean, got :: Int }
      prog = do
        won1 <- STM.atomically (TDeferred.complete d 1)
        won2 <- STM.atomically (TDeferred.complete d 2)
        got <- STM.atomically (TDeferred.await d)
        pure { won1, won2, got }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.won1 `shouldEqual` true
        r.won2 `shouldEqual` false
        r.got `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "await suspends until complete fires from another fiber" do
    d <- liftEffect (TDeferred.make :: _ (TDeferred String))
    let
      prog :: F.RIO () () String
      prog = do
        consumer <- F.fork (STM.atomically (TDeferred.await d))
        F.sleep (Milliseconds 10.0)
        _ <- STM.atomically (TDeferred.complete d "ready")
        F.join consumer
    out <- runAff prog {}
    case out of
      Success s -> s `shouldEqual` "ready"
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
