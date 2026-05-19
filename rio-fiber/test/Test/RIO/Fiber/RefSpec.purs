module Test.RIO.Fiber.RefSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Ref as FR
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: FiberRef" do
  it "returns the initial value when never set" do
    ref <- liftEffect (FR.newFiberRef 7)
    let
      prog :: F.RIO () () Int
      prog = FR.getFiberRef ref
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 7
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "round-trips set and get within the same fiber" do
    ref <- liftEffect (FR.newFiberRef 0)
    let
      prog :: F.RIO () () Int
      prog = do
        FR.setFiberRef ref 42
        FR.getFiberRef ref
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "modifies via a transformer" do
    ref <- liftEffect (FR.newFiberRef 10)
    let
      prog :: F.RIO () () Int
      prog = do
        FR.setFiberRef ref 5
        FR.modifyFiberRef ref (_ + 3)
        FR.modifyFiberRef ref (_ * 2)
        FR.getFiberRef ref
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 16
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "child inherits parent's value at fork time" do
    ref <- liftEffect (FR.newFiberRef 0)
    let
      prog :: F.RIO () () Int
      prog = do
        FR.setFiberRef ref 99
        fib <- F.fork (FR.getFiberRef ref)
        F.join fib
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 99
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "child's writes do not leak to parent" do
    seen <- liftEffect (Ref.new 0)
    ref <- liftEffect (FR.newFiberRef 1)
    let
      prog :: F.RIO () () Int
      prog = do
        fib <- F.fork do
          FR.setFiberRef ref 500
          FR.getFiberRef ref
        _ <- F.join fib
        n <- FR.getFiberRef ref
        F.liftEffect (Ref.write n seen)
        pure n
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)
    parentVal <- liftEffect (Ref.read seen)
    parentVal `shouldEqual` 1

  it "parent writes after fork do not leak to running child" do
    childSaw <- liftEffect (Ref.new 0)
    ref <- liftEffect (FR.newFiberRef 1)
    let
      prog :: F.RIO () () Unit
      prog = do
        fib <- F.fork do
          -- give parent a chance to mutate before we read
          F.sleep (Milliseconds 10.0)
          n <- FR.getFiberRef ref
          F.liftEffect (Ref.write n childSaw)
        FR.setFiberRef ref 777
        _ <- F.join fib
        pure unit
    _ <- runAff prog {}
    seen <- liftEffect (Ref.read childSaw)
    seen `shouldEqual` 1

  it "siblings forked in parTraverse are isolated from each other" do
    ref <- liftEffect (FR.newFiberRef 0)
    let
      prog :: F.RIO () () (Array Int)
      prog = F.parTraverse
        ( \i -> do
            FR.setFiberRef ref i
            F.sleep (Milliseconds 5.0)
            FR.getFiberRef ref
        )
        [ 1, 2, 3, 4, 5 ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
