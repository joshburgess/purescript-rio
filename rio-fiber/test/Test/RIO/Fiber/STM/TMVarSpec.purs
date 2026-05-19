module Test.RIO.Fiber.STM.TMVarSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TMVar (TMVar)
import RIO.Fiber.STM.TMVar as TMVar
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TMVar" do
  it "newEmpty starts empty" do
    m <- liftEffect (TMVar.newEmpty :: _ (TMVar Int))
    let
      prog :: F.RIO () () Boolean
      prog = STM.atomically (TMVar.isEmpty m)
    out <- runAff prog {}
    case out of
      Success b -> b `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "new starts full and read returns the value" do
    m <- liftEffect (TMVar.new 42)
    let
      prog :: F.RIO () () Int
      prog = STM.atomically (TMVar.read m)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "take empties the cell" do
    m <- liftEffect (TMVar.new 7)
    let
      prog :: F.RIO () () { taken :: Int, empty :: Boolean }
      prog = do
        taken <- STM.atomically (TMVar.take m)
        empty <- STM.atomically (TMVar.isEmpty m)
        pure { taken, empty }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.taken `shouldEqual` 7
        r.empty `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "put then take round-trips" do
    m <- liftEffect (TMVar.newEmpty :: _ (TMVar Int))
    let
      prog :: F.RIO () () Int
      prog = do
        STM.atomically (TMVar.put m 99)
        STM.atomically (TMVar.take m)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 99
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryTake returns Nothing when empty" do
    m <- liftEffect (TMVar.newEmpty :: _ (TMVar Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = STM.atomically (TMVar.tryTake m)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryPut returns false when full" do
    m <- liftEffect (TMVar.new 1)
    let
      prog :: F.RIO () () Boolean
      prog = STM.atomically (TMVar.tryPut m 2)
    out <- runAff prog {}
    case out of
      Success ok -> ok `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "take suspends until put from another fiber" do
    m <- liftEffect (TMVar.newEmpty :: _ (TMVar Int))
    let
      prog :: F.RIO () () Int
      prog = do
        consumer <- F.fork (STM.atomically (TMVar.take m))
        F.sleep (Milliseconds 10.0)
        STM.atomically (TMVar.put m 5)
        F.join consumer
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 5
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
