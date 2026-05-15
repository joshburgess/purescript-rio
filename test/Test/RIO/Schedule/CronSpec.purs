module Test.RIO.Schedule.CronSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Clock (Clock)
import RIO.Core (RIO, provideAll, runRIO')
import RIO.Schedule (Schedule, Step(..), dayOfWeek, hourOfDay, minuteOfHour, step)
import RIO.Test.Clock (newTestClock)

spec :: Spec Unit
spec = describe "RIO.Schedule (cron-shaped)" do

  describe "minuteOfHour" do
    it "emits the time until the next minute boundary at t=0" do
      tc <- newTestClock
      let
        sched :: Schedule (clock :: Clock) Unit Int
        sched = minuteOfHour

        program
          :: RIO (clock :: Clock) ()
               { delay :: Milliseconds, out :: Int }
        program = do
          s <- step sched unit
          case s of
            Done -> pure { delay: Milliseconds (-1.0), out: -1 }
            Continue o d _ -> pure { delay: d, out: o }
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result.delay `shouldEqual` Milliseconds 60000.0
      result.out `shouldEqual` 1

    it "emits the remaining ms until the next minute mid-period" do
      tc <- newTestClock
      let
        sched :: Schedule (clock :: Clock) Unit Int
        sched = minuteOfHour

        program :: RIO (clock :: Clock) () Milliseconds
        program = do
          liftAff (tc.advance (Milliseconds 45000.0))
          s <- step sched unit
          case s of
            Done -> pure (Milliseconds (-1.0))
            Continue _ d _ -> pure d
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result `shouldEqual` Milliseconds 15000.0

    it "advances exactly one minute between consecutive steps" do
      tc <- newTestClock
      let
        sched :: Schedule (clock :: Clock) Unit Int
        sched = minuteOfHour

        program :: RIO (clock :: Clock) () { o1 :: Int, o2 :: Int }
        program = do
          s1 <- step sched unit
          case s1 of
            Done -> pure { o1: -1, o2: -1 }
            Continue o1 d1 next -> do
              liftAff (tc.advance d1)
              s2 <- step next unit
              case s2 of
                Done -> pure { o1, o2: -1 }
                Continue o2 _ _ -> pure { o1, o2 }
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result.o1 `shouldEqual` 1
      result.o2 `shouldEqual` 2

  describe "hourOfDay" do
    it "emits one hour delay and output 1 at t=0" do
      tc <- newTestClock
      let
        sched :: Schedule (clock :: Clock) Unit Int
        sched = hourOfDay

        program
          :: RIO (clock :: Clock) ()
               { delay :: Milliseconds, out :: Int }
        program = do
          s <- step sched unit
          case s of
            Done -> pure { delay: Milliseconds (-1.0), out: -1 }
            Continue o d _ -> pure { delay: d, out: o }
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result.delay `shouldEqual` Milliseconds 3600000.0
      result.out `shouldEqual` 1

  describe "dayOfWeek" do
    it "emits one day delay and output Friday (5) at t=0 (Thursday)" do
      tc <- newTestClock
      let
        sched :: Schedule (clock :: Clock) Unit Int
        sched = dayOfWeek

        program
          :: RIO (clock :: Clock) ()
               { delay :: Milliseconds, out :: Int }
        program = do
          s <- step sched unit
          case s of
            Done -> pure { delay: Milliseconds (-1.0), out: -1 }
            Continue o d _ -> pure { delay: d, out: o }
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result.delay `shouldEqual` Milliseconds 86400000.0
      result.out `shouldEqual` 5
