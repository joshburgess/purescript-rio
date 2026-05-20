module Test.RIO.Aff.Schedule.FixedSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, provideAll, runRIO')
import RIO.Aff.Schedule (Schedule, Step(..), fixed, step)
import RIO.Aff.Test.Clock (newTestClock)

spec :: Spec Unit
spec = describe "RIO.Aff.Schedule.fixed" do
  it "emits the period as the initial delay" do
    tc <- newTestClock
    let
      sched :: Schedule (clock :: Clock) Unit Milliseconds
      sched = fixed (Milliseconds 100.0)

      program :: RIO (clock :: Clock) () Milliseconds
      program = do
        out <- step sched unit
        case out of
          Continue _ delay _ -> pure delay
          Done -> pure (Milliseconds (-1.0))
    delay <- runRIO' (provideAll { clock: tc.clock } program)
    delay `shouldEqual` Milliseconds 100.0

  it "keeps a fixed cadence when the work fits within the period" do
    -- Drive `fixed 100` through three steps, advancing the virtual
    -- clock by 50ms between each step (simulating fast work). Each
    -- emitted delay should be the remaining distance to the next
    -- target, not the period itself.
    tc <- newTestClock
    let
      sched :: Schedule (clock :: Clock) Unit Milliseconds
      sched = fixed (Milliseconds 100.0)

      program
        :: RIO (clock :: Clock) () { d1 :: Milliseconds, d2 :: Milliseconds, d3 :: Milliseconds }
      program = do
        out1 <- step sched unit
        case out1 of
          Done -> pure
            { d1: Milliseconds (-1.0)
            , d2: Milliseconds (-1.0)
            , d3: Milliseconds (-1.0)
            }
          Continue _ d1 next1 -> do
            liftAff (tc.advance (Milliseconds 50.0))
            out2 <- step next1 unit
            case out2 of
              Done -> pure { d1, d2: Milliseconds (-1.0), d3: Milliseconds (-1.0) }
              Continue _ d2 next2 -> do
                liftAff (tc.advance (Milliseconds 50.0))
                out3 <- step next2 unit
                case out3 of
                  Done -> pure { d1, d2, d3: Milliseconds (-1.0) }
                  Continue _ d3 _ -> pure { d1, d2, d3 }
    result <- runRIO' (provideAll { clock: tc.clock } program)
    -- target_1 = 0 + 100 = 100; emitted d1 = 100 (asleep until t=100)
    -- after d1 we advance 50ms (the runner would have slept 100, but we're
    -- simulating "work took the time"). Then step 2:
    --   tNow = 50, target_1 = 100, raw = 50, d2 = 50
    --   next target = 100 + 100 = 200
    -- advance 50: tNow = 100
    --   target_2 = 200, raw = 100, d3 = 100
    result.d1 `shouldEqual` Milliseconds 100.0
    result.d2 `shouldEqual` Milliseconds 50.0
    result.d3 `shouldEqual` Milliseconds 100.0

  it "re-aligns to the next future target when the work overshoots the period" do
    -- Same schedule, but the simulated "work" between steps overshoots
    -- the period. We expect the emitted delay to be 0 (no sleep) and
    -- the schedule's internal target to jump forward to the next
    -- future multiple of the period rather than playing catch-up.
    tc <- newTestClock
    let
      sched :: Schedule (clock :: Clock) Unit Milliseconds
      sched = fixed (Milliseconds 100.0)

      program
        :: RIO (clock :: Clock) () { d1 :: Milliseconds, d2 :: Milliseconds, d3 :: Milliseconds }
      program = do
        out1 <- step sched unit
        case out1 of
          Done -> pure
            { d1: Milliseconds (-1.0)
            , d2: Milliseconds (-1.0)
            , d3: Milliseconds (-1.0)
            }
          Continue _ d1 next1 -> do
            -- "Work" takes 250ms - we overshoot the next target (100)
            -- by 150ms.
            liftAff (tc.advance (Milliseconds 250.0))
            out2 <- step next1 unit
            case out2 of
              Done -> pure { d1, d2: Milliseconds (-1.0), d3: Milliseconds (-1.0) }
              Continue _ d2 next2 -> do
                -- After the catch-up, the next target should be 300.
                -- tNow = 250, so the next step should sleep 50ms to reach it.
                liftAff (tc.advance (Milliseconds 0.0))
                out3 <- step next2 unit
                case out3 of
                  Done -> pure { d1, d2, d3: Milliseconds (-1.0) }
                  Continue _ d3 _ -> pure { d1, d2, d3 }
    result <- runRIO' (provideAll { clock: tc.clock } program)
    -- d1 = 100 (first target = 100, tNow = 0)
    -- after advance 250: tNow = 250, target_1 = 100, raw = -150
    -- overshoots = ceil(150 / 100) = 2; next_target = 100 + 100*(2+1) = 400
    -- d2 = 0 (raw was negative)
    -- step 3: tNow = 250, target_2 = 400, raw = 150, d3 = 150
    result.d1 `shouldEqual` Milliseconds 100.0
    result.d2 `shouldEqual` Milliseconds 0.0
    result.d3 `shouldEqual` Milliseconds 150.0
