module Test.RIO.Fiber.STM.TArraySpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TArray (TArray)
import RIO.Fiber.STM.TArray as TArray
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TArray" do
  it "make + length reports the size" do
    arr <- liftEffect (TArray.make [ 10, 20, 30 ] :: _ (TArray Int))
    TArray.length arr `shouldEqual` 3

  it "read at a valid index returns Just the slot value" do
    arr <- liftEffect (TArray.make [ 10, 20, 30 ])
    let
      prog :: F.RIO () () (Maybe Int)
      prog = STM.atomically (TArray.read arr 1)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Just 20
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "read out-of-bounds is Nothing" do
    arr <- liftEffect (TArray.make [ 1, 2 ])
    let
      prog :: F.RIO () () (Maybe Int)
      prog = STM.atomically (TArray.read arr 99)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "write updates the slot and leaves others alone" do
    arr <- liftEffect (TArray.make [ 1, 2, 3 ])
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        STM.atomically (TArray.write arr 1 99)
        STM.atomically (TArray.freeze arr)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 99, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "modify applies a function in place" do
    arr <- liftEffect (TArray.make [ 4, 5, 6 ])
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        STM.atomically (TArray.modify arr 0 (_ * 10))
        STM.atomically (TArray.modify arr 2 (_ + 1))
        STM.atomically (TArray.freeze arr)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 40, 5, 7 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "concurrent writes from multiple fibers commit to distinct slots" do
    arr <- liftEffect (TArray.replicate 5 0)
    let
      bump :: Int -> F.RIO () () Unit
      bump i = STM.atomically (TArray.modify arr i (_ + 1))

      prog :: F.RIO () () (Array Int)
      prog = do
        _ <- F.parTraverse bump [ 0, 1, 2, 3, 4 ]
        STM.atomically (TArray.freeze arr)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 1, 1, 1, 1 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
