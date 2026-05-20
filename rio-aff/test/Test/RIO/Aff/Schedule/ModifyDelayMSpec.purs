module Test.RIO.Aff.Schedule.ModifyDelayMSpec (spec) where

import Prelude

import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Internal (mkRIO) as Internal
import RIO.Aff.Schedule
  ( Schedule
  , Step(..)
  , addDelayM
  , modifyDelayM
  , recurs
  , spaced
  , step
  )

walk3
  :: forall i o
   . Schedule () i o
  -> i
  -> RIO () ()
       { os :: Array o
       , ds :: Array Milliseconds
       , terminated :: Boolean
       }
walk3 s0 i = do
  out1 <- step s0 i
  case out1 of
    Done -> pure { os: [], ds: [], terminated: true }
    Continue o1 d1 s1 -> do
      out2 <- step s1 i
      case out2 of
        Done -> pure { os: [ o1 ], ds: [ d1 ], terminated: true }
        Continue o2 d2 s2 -> do
          out3 <- step s2 i
          case out3 of
            Done ->
              pure
                { os: [ o1, o2 ]
                , ds: [ d1, d2 ]
                , terminated: true
                }
            Continue o3 d3 _ ->
              pure
                { os: [ o1, o2, o3 ]
                , ds: [ d1, d2, d3 ]
                , terminated: false
                }

-- | Lift a pure function into the schedule's effect row `()`.
pureRIO :: forall a. a -> RIO () () a
pureRIO = pure

spec :: Spec Unit
spec = describe "RIO.Aff.Schedule (modifyDelayM / addDelayM)" do

  describe "modifyDelayM" do
    it "rewrites each per-step delay via a pure-shaped RIO" do
      let
        sched :: Schedule () Unit Int
        sched = modifyDelayM
          (\(Milliseconds n) -> pureRIO (Milliseconds (n * 2.0)))
          (spaced (Milliseconds 50.0))

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      result `shouldEqual`
        [ Milliseconds 100.0
        , Milliseconds 100.0
        , Milliseconds 100.0
        ]

    it "can read effectful state to compute the new delay" do
      counter <- liftEffect (Ref.new 0.0)
      let
        -- each step bumps the cap, so successive delays are
        -- monotonically increasing.
        adjust :: Milliseconds -> RIO () () Milliseconds
        adjust _ = Internal.mkRIO \_ -> do
          n <- liftEffect (Ref.modify (_ + 10.0) counter)
          pure (Milliseconds n)

        sched :: Schedule () Unit Int
        sched = modifyDelayM adjust (spaced (Milliseconds 1.0))

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      result `shouldEqual`
        [ Milliseconds 10.0
        , Milliseconds 20.0
        , Milliseconds 30.0
        ]

    it "preserves outputs and termination of the underlying schedule" do
      let
        sched :: Schedule () Unit Int
        sched = modifyDelayM
          (\d -> pureRIO d)
          (recurs 2)

        program :: RIO () () (Array Int)
        program = do
          r <- walk3 sched unit
          pure r.os
      result <- runRIO' program
      result `shouldEqual` [ 1, 2 ]

  describe "addDelayM" do
    it "adds a constant computed delta to each step" do
      let
        sched :: Schedule () Unit Int
        sched = addDelayM
          (\_ -> pureRIO (Milliseconds 25.0))
          (spaced (Milliseconds 50.0))

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      result `shouldEqual`
        [ Milliseconds 75.0
        , Milliseconds 75.0
        , Milliseconds 75.0
        ]

    it "can compute the delta from the schedule's output" do
      let
        sched :: Schedule () Unit Int
        sched = addDelayM
          (\n -> pureRIO (Milliseconds (10.0 * (toNumber n))))
          (spaced (Milliseconds 5.0))

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      -- spaced emits a recurrence count via its output; addDelayM
      -- here adds (10 * count) ms on top of the 5 ms base.
      result `shouldEqual`
        [ Milliseconds 15.0
        , Milliseconds 25.0
        , Milliseconds 35.0
        ]

    it "preserves outputs and termination of the underlying schedule" do
      let
        sched :: Schedule () Unit Int
        sched = addDelayM
          (\_ -> pureRIO (Milliseconds 1.0))
          (recurs 2)

        program
          :: RIO () ()
               { os :: Array Int, terminated :: Boolean }
        program = do
          r <- walk3 sched unit
          pure { os: r.os, terminated: r.terminated }
      result <- runRIO' program
      result.os `shouldEqual` [ 1, 2 ]
      result.terminated `shouldEqual` true
