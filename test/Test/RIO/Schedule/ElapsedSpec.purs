module Test.RIO.Schedule.ElapsedSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Clock (Clock)
import RIO.Core (RIO, provideAll, runRIO')
import RIO.Schedule (Schedule, Step(..), elapsed, step)
import RIO.Test.Clock (newTestClock)

spec :: Spec Unit
spec = describe "RIO.Schedule.elapsed" do
  it "emits zero on the first step" do
    tc <- newTestClock
    let
      sched :: Schedule (clock :: Clock) Unit Milliseconds
      sched = elapsed

      program :: RIO (clock :: Clock) () Milliseconds
      program = do
        out <- step sched unit
        case out of
          Continue ms _ _ -> pure ms
          Done -> pure (Milliseconds (-1.0))
    ms <- runRIO' (provideAll { clock: tc.clock } program)
    ms `shouldEqual` Milliseconds 0.0

  it "tracks total elapsed wall-clock time across steps" do
    tc <- newTestClock
    let
      sched :: Schedule (clock :: Clock) Unit Milliseconds
      sched = elapsed

      program
        :: RIO (clock :: Clock) ()
             { m1 :: Milliseconds, m2 :: Milliseconds, m3 :: Milliseconds }
      program = do
        out1 <- step sched unit
        case out1 of
          Done -> pure
            { m1: Milliseconds (-1.0)
            , m2: Milliseconds (-1.0)
            , m3: Milliseconds (-1.0)
            }
          Continue m1 _ next1 -> do
            liftAff (tc.advance (Milliseconds 75.0))
            out2 <- step next1 unit
            case out2 of
              Done -> pure
                { m1, m2: Milliseconds (-1.0), m3: Milliseconds (-1.0) }
              Continue m2 _ next2 -> do
                liftAff (tc.advance (Milliseconds 125.0))
                out3 <- step next2 unit
                case out3 of
                  Done -> pure { m1, m2, m3: Milliseconds (-1.0) }
                  Continue m3 _ _ -> pure { m1, m2, m3 }
    result <- runRIO' (provideAll { clock: tc.clock } program)
    result.m1 `shouldEqual` Milliseconds 0.0
    result.m2 `shouldEqual` Milliseconds 75.0
    result.m3 `shouldEqual` Milliseconds 200.0

  it "emits zero delay on every step" do
    tc <- newTestClock
    let
      sched :: Schedule (clock :: Clock) Unit Milliseconds
      sched = elapsed

      program
        :: RIO (clock :: Clock) ()
             { d1 :: Milliseconds, d2 :: Milliseconds }
      program = do
        out1 <- step sched unit
        case out1 of
          Done -> pure
            { d1: Milliseconds (-1.0), d2: Milliseconds (-1.0) }
          Continue _ d1 next1 -> do
            liftAff (tc.advance (Milliseconds 10.0))
            out2 <- step next1 unit
            case out2 of
              Done -> pure { d1, d2: Milliseconds (-1.0) }
              Continue _ d2 _ -> pure { d1, d2 }
    result <- runRIO' (provideAll { clock: tc.clock } program)
    result.d1 `shouldEqual` Milliseconds 0.0
    result.d2 `shouldEqual` Milliseconds 0.0
