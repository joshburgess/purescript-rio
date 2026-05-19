module Test.RIO.Fiber.TestClockSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Exception as Effect.Exception
import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Queue as Q
import RIO.Fiber.Stream as S
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

  it "Stream.throttle paces emissions according to the virtual clock" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      source :: F.RIO () () (S.Stream () () Int)
      source = pure (S.fromArray [ 1, 2 ])

      prog :: F.RIO () () (Array Int)
      prog = Clock.withClock (TestClock.clock tc) do
        src <- source
        consumer <- F.fork
          (S.runCollect (S.throttle (Milliseconds 100.0) src))
        -- give the consumer a tick to emit the first element and
        -- park inside throttle's sleep
        F.sleep (Milliseconds 0.0)
        TestClock.advance tc (Milliseconds 100.0)
        F.join consumer
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "Stream.timeoutPerPull yields Nothing when the virtual clock crosses the timeout" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    q <- liftEffect (Q.make 4 :: _ (Q.Queue Int))
    let
      source :: S.Stream () () Int
      source = S.fromQueue q

      pullStep :: S.Stream () () (Maybe Int) -> F.RIO () () (S.Step () () (Maybe Int))
      pullStep (S.Stream pull) = pull

      prog :: F.RIO () () (Array (Maybe Int))
      prog = Clock.withClock (TestClock.clock tc) do
        Q.offer q 42
        let timed = S.timeoutPerPull (Milliseconds 50.0) source
        step1 <- pullStep timed
        case step1 of
          S.Yield m1 rest -> do
            -- pull 2 blocks on the race; let it set up, then advance
            -- the test clock past the timeout to make the sleep win.
            blocker <- F.fork (pullStep rest)
            F.sleep (Milliseconds 0.0)
            F.sleep (Milliseconds 0.0)
            TestClock.advance tc (Milliseconds 50.0)
            step2 <- F.join blocker
            case step2 of
              S.Yield m2 _ -> pure [ m1, m2 ]
              S.Done -> F.die (Effect.Exception.error "unexpected Done")
          S.Done -> F.die (Effect.Exception.error "unexpected Done on pull 1")
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ Just 42, Nothing ]
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
