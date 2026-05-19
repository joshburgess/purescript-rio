module Test.RIO.Fiber.TestClockSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.TestClock as TestClock
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: TestClock" do
  it "reads the initial epoch" do
    tc <- liftEffect (TestClock.make (Milliseconds 1000.0))
    let
      prog :: F.RIO () () Milliseconds
      prog = Clock.withClock (TestClock.clock tc) Clock.currentEpoch
    out <- runAff prog {}
    case out of
      Success ms -> ms `shouldEqual` Milliseconds 1000.0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "advance moves time forward" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      prog :: F.RIO () () Milliseconds
      prog = Clock.withClock (TestClock.clock tc) do
        TestClock.advance tc (Milliseconds 250.0)
        TestClock.advance tc (Milliseconds 100.0)
        Clock.currentEpoch
    out <- runAff prog {}
    case out of
      Success ms -> ms `shouldEqual` Milliseconds 350.0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "setEpoch jumps to an absolute time" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      prog :: F.RIO () () Milliseconds
      prog = Clock.withClock (TestClock.clock tc) do
        TestClock.setEpoch tc (Milliseconds 9999.0)
        Clock.currentEpoch
    out <- runAff prog {}
    case out of
      Success ms -> ms `shouldEqual` Milliseconds 9999.0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "advance wakes a sleeping fiber once its deadline is reached" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      prog :: F.RIO () () Milliseconds
      prog = Clock.withClock (TestClock.clock tc) do
        waiter <- F.fork do
          F.sleep (Milliseconds 100.0)
          Clock.currentEpoch
        -- yield so the waiter has a chance to register its sleep
        F.sleep (Milliseconds 0.0)
        TestClock.advance tc (Milliseconds 50.0)
        -- still not enough; yield again before second advance
        F.sleep (Milliseconds 0.0)
        TestClock.advance tc (Milliseconds 100.0)
        F.join waiter
    out <- runAff prog {}
    case out of
      Success ms -> ms `shouldEqual` Milliseconds 150.0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "sleep does not fire until advance crosses its deadline" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      prog :: F.RIO () () Boolean
      prog = Clock.withClock (TestClock.clock tc) do
        waiter <- F.fork (F.sleep (Milliseconds 500.0))
        TestClock.advance tc (Milliseconds 100.0)
        -- waiter still suspended; race it against a zero-sleep to
        -- detect that it has not completed.
        sentinel <- F.fork (F.sleep (Milliseconds 0.0))
        _ <- F.join sentinel
        -- now release the waiter
        TestClock.advance tc (Milliseconds 500.0)
        _ <- F.join waiter
        pure true
    out <- runAff prog {}
    case out of
      Success b -> b `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
