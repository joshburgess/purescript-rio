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

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
