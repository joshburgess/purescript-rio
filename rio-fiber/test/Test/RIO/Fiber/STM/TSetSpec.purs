module Test.RIO.Fiber.STM.TSetSpec (spec) where

import Prelude

import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TSet (TSet)
import RIO.Fiber.STM.TSet as TSet
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TSet" do
  it "empty starts with size 0 and reports null" do
    s <- liftEffect (TSet.empty :: _ (TSet Int))
    let
      prog :: F.RIO () () { size :: Int, n :: Boolean }
      prog = do
        size <- STM.atomically (TSet.size s)
        n <- STM.atomically (TSet.null s)
        pure { size, n }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.size `shouldEqual` 0
        r.n `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "insert is idempotent and member reflects it" do
    s <- liftEffect (TSet.empty :: _ (TSet Int))
    let
      prog :: F.RIO () () { has :: Boolean, size :: Int }
      prog = do
        STM.atomically (TSet.insert 7 s)
        STM.atomically (TSet.insert 7 s)
        has <- STM.atomically (TSet.member 7 s)
        size <- STM.atomically (TSet.size s)
        pure { has, size }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.has `shouldEqual` true
        r.size `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "delete removes the element" do
    s <- liftEffect (TSet.empty :: _ (TSet Int))
    let
      prog :: F.RIO () () Boolean
      prog = do
        STM.atomically (TSet.insert 1 s)
        STM.atomically (TSet.delete 1 s)
        STM.atomically (TSet.member 1 s)
    out <- runAff prog {}
    case out of
      Success v -> v `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "toArray snapshots in ascending order" do
    s <- liftEffect (TSet.empty :: _ (TSet Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        STM.atomically (TSet.insert 3 s)
        STM.atomically (TSet.insert 1 s)
        STM.atomically (TSet.insert 2 s)
        STM.atomically (TSet.toArray s)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "atomic multi-element write is observed all-or-nothing" do
    s <- liftEffect (TSet.empty :: _ (TSet Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        STM.atomically do
          TSet.insert 1 s
          TSet.insert 2 s
          TSet.insert 3 s
        STM.atomically (TSet.toArray s)
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
