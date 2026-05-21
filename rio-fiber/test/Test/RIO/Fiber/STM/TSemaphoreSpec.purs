module Test.RIO.Fiber.STM.TSemaphoreSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.STM as STM
import RIO.Fiber.STM.TSemaphore (TSemaphore)
import RIO.Fiber.STM.TSemaphore as TSemaphore
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: STM.TSemaphore" do
  it "make exposes the initial permit count" do
    sem <- liftEffect (TSemaphore.make 3 :: _ TSemaphore)
    let
      prog :: F.RIO () () Int
      prog = STM.atomically (TSemaphore.available sem)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 3
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "make clamps negative counts to zero" do
    sem <- liftEffect (TSemaphore.make (-5))
    let
      prog :: F.RIO () () Int
      prog = STM.atomically (TSemaphore.available sem)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "acquire decrements and release increments the permit count" do
    sem <- liftEffect (TSemaphore.make 2)
    let
      prog :: F.RIO () () { after :: Int, end :: Int }
      prog = do
        STM.atomically (TSemaphore.acquire sem)
        STM.atomically (TSemaphore.acquire sem)
        after <- STM.atomically (TSemaphore.available sem)
        STM.atomically (TSemaphore.release sem)
        end <- STM.atomically (TSemaphore.available sem)
        pure { after, end }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.after `shouldEqual` 0
        r.end `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "acquireN takes N permits atomically; tryAcquire reports failure when undersupplied" do
    sem <- liftEffect (TSemaphore.make 5)
    let
      prog :: F.RIO () () { got :: Boolean, left :: Int }
      prog = do
        STM.atomically (TSemaphore.acquireN 3 sem)
        got <- STM.atomically (TSemaphore.tryAcquireN 3 sem)
        left <- STM.atomically (TSemaphore.available sem)
        pure { got, left }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.got `shouldEqual` false
        r.left `shouldEqual` 2
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "acquire suspends when no permits remain; release from another fiber wakes it" do
    sem <- liftEffect (TSemaphore.make 0)
    let
      prog :: F.RIO () () Boolean
      prog = do
        waiter <- F.fork (STM.atomically (TSemaphore.acquire sem))
        F.sleep (Milliseconds 10.0)
        STM.atomically (TSemaphore.release sem)
        _ <- F.join waiter
        pure true
    out <- runAff prog {}
    case out of
      Success r -> r `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryAcquire returns true and decrements when a permit is available" do
    sem <- liftEffect (TSemaphore.make 1)
    let
      prog :: F.RIO () () { ok :: Boolean, left :: Int }
      prog = do
        ok <- STM.atomically (TSemaphore.tryAcquire sem)
        left <- STM.atomically (TSemaphore.available sem)
        pure { ok, left }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.ok `shouldEqual` true
        r.left `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
