module Test.RIO.Aff.Schedule.OutputsSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Schedule
  ( Schedule
  , Step(..)
  , asUnit
  , delayed
  , exponential
  , recurs
  , spaced
  , step
  , windowed
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
              pure { os: [ o1, o2 ], ds: [ d1, d2 ], terminated: true }
            Continue o3 d3 _ ->
              pure
                { os: [ o1, o2, o3 ]
                , ds: [ d1, d2, d3 ]
                , terminated: false
                }

spec :: Spec Unit
spec = describe "RIO.Aff.Schedule (outputs)" do

  describe "asUnit" do
    it "discards every output to Unit while preserving cadence" do
      let
        sched :: Schedule () Unit Unit
        sched = asUnit (recurs 3)

        program
          :: RIO () ()
               { os :: Array Unit, terminated :: Boolean }
        program = do
          r <- walk3 sched unit
          pure { os: r.os, terminated: r.terminated }
      result <- runRIO' program
      result.os `shouldEqual` [ unit, unit, unit ]

    it "preserves delays from the underlying schedule" do
      let
        sched :: Schedule () Unit Unit
        sched = asUnit (spaced (Milliseconds 50.0))

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      result `shouldEqual`
        [ Milliseconds 50.0
        , Milliseconds 50.0
        , Milliseconds 50.0
        ]

  describe "windowed" do
    it "caps delays that exceed the window" do
      let
        sched :: Schedule () Unit Milliseconds
        sched = windowed (Milliseconds 250.0)
          (exponential (Milliseconds 100.0) 2.0)

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      -- exponential emits 100, 200, 400; cap at 250 -> 100, 200, 250
      result `shouldEqual`
        [ Milliseconds 100.0
        , Milliseconds 200.0
        , Milliseconds 250.0
        ]

    it "leaves below-cap delays unchanged" do
      let
        sched :: Schedule () Unit Int
        sched = windowed (Milliseconds 9999.0)
          (spaced (Milliseconds 10.0))

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      result `shouldEqual`
        [ Milliseconds 10.0
        , Milliseconds 10.0
        , Milliseconds 10.0
        ]

    it "preserves outputs and termination" do
      let
        sched :: Schedule () Unit Int
        sched = windowed (Milliseconds 10.0) (recurs 2)

        program :: RIO () () { os :: Array Int, terminated :: Boolean }
        program = do
          r <- walk3 sched unit
          pure { os: r.os, terminated: r.terminated }
      result <- runRIO' program
      result.os `shouldEqual` [ 1, 2 ]
      result.terminated `shouldEqual` true

  describe "delayed" do
    it "adds the offset to the first step only" do
      let
        sched :: Schedule () Unit Int
        sched = delayed (Milliseconds 25.0) (spaced (Milliseconds 50.0))

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      -- first step delayed by 25; subsequent steps run at the
      -- underlying schedule's normal cadence.
      result `shouldEqual`
        [ Milliseconds 75.0
        , Milliseconds 50.0
        , Milliseconds 50.0
        ]

    it "passes a Done first step through unchanged" do
      let
        sched :: Schedule () Unit Int
        sched = delayed (Milliseconds 1000.0) (recurs 0)

        program :: RIO () () Boolean
        program = do
          out <- step sched unit
          case out of
            Done -> pure true
            Continue _ _ _ -> pure false
      result <- runRIO' program
      result `shouldEqual` true

    it "preserves outputs from the underlying schedule" do
      let
        sched :: Schedule () Unit Int
        sched = delayed (Milliseconds 5.0) (recurs 3)

        program :: RIO () () (Array Int)
        program = do
          r <- walk3 sched unit
          pure r.os
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]
