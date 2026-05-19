module Test.RIO.Fiber.SemaphoreSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Semaphore as Sem
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Semaphore" do
  it "acquireN returns immediately when permits are available" do
    s <- liftEffect (Sem.make 3)
    let
      prog :: F.RIO () () Int
      prog = do
        Sem.acquireN 2 s
        Sem.available s
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "acquireN suspends until releaseN runs" do
    s <- liftEffect (Sem.make 1)
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: String -> F.RIO () () Unit
      record m = F.liftEffect (Ref.modify_ (\xs -> xs <> [ m ]) log)

      prog :: F.RIO () () Unit
      prog = do
        Sem.acquireN 1 s
        fib <- F.fork do
          Sem.acquireN 1 s
          record "fork-acquired"
        F.sleep (Milliseconds 10.0)
        record "before-release"
        Sem.releaseN 1 s
        _ <- F.join fib
        pure unit
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ "before-release", "fork-acquired" ]

  it "withPermit releases on success" do
    s <- liftEffect (Sem.make 1)
    let
      prog :: F.RIO () () { duringInside :: Int, afterInside :: Int }
      prog = do
        duringInside <- Sem.withPermit s (Sem.available s)
        afterInside <- Sem.available s
        pure { duringInside, afterInside }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.duringInside `shouldEqual` 0
        r.afterInside `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "withPermit releases on interrupt" do
    s <- liftEffect (Sem.make 1)
    let
      forever :: F.RIO () () Unit
      forever = do
        F.sleep (Milliseconds 100.0)
        forever

      prog :: F.RIO () () Int
      prog = do
        fib <- F.fork (Sem.withPermit s forever)
        F.sleep (Milliseconds 5.0)
        F.interrupt fib
        _ <- F.causeOf (F.join fib)
        Sem.available s
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "cancelling an awaiter does not strand permits" do
    s <- liftEffect (Sem.make 1)
    let
      prog :: F.RIO () () Int
      prog = do
        Sem.acquireN 1 s
        waiter <- F.fork (Sem.acquireN 1 s)
        F.sleep (Milliseconds 5.0)
        F.interrupt waiter
        _ <- F.causeOf (F.join waiter)
        -- still hold the original permit; release should bring us
        -- back to one and no one is waiting
        Sem.releaseN 1 s
        Sem.available s
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "queue is FIFO" do
    s <- liftEffect (Sem.make 0)
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: String -> F.RIO () () Unit
      record m = F.liftEffect (Ref.modify_ (\xs -> xs <> [ m ]) log)

      prog :: F.RIO () () Unit
      prog = do
        a <- F.fork do
          Sem.acquireN 1 s
          record "a"
        F.sleep (Milliseconds 5.0)
        b <- F.fork do
          Sem.acquireN 1 s
          record "b"
        F.sleep (Milliseconds 5.0)
        Sem.releaseN 1 s
        Sem.releaseN 1 s
        _ <- F.join a
        _ <- F.join b
        pure unit
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ "a", "b" ]

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
