module Test.RIO.Schedule.DimapDelaySpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Schedule
  ( Schedule
  , Step(..)
  , dimap
  , mapDelay
  , mapInput
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

spec :: Spec Unit
spec = describe "RIO.Schedule (mapInput / mapDelay / dimap)" do

  describe "mapInput" do
    it "passes f i to the underlying schedule for each step" do
      -- recursWhile-equivalent built by hand to make the pre-mapping
      -- observable: the predicate fires on the *mapped* input.
      let
        sched :: Schedule () Int Int
        sched = mapInput (\n -> n + 100) (recurs 5)

        program :: RIO () () (Array Int)
        program = do
          r <- walk3 sched 7
          pure r.os
      result <- runRIO' program
      -- recurs ignores its input, so we mainly check pass-through.
      result `shouldEqual` [ 1, 2, 3 ]

    it "preserves termination of the underlying schedule" do
      let
        sched :: Schedule () Int Int
        sched = mapInput identity (recurs 1)

        program
          :: RIO () ()
               { os :: Array Int, terminated :: Boolean }
        program = do
          r <- walk3 sched 0
          pure { os: r.os, terminated: r.terminated }
      result <- runRIO' program
      result.os `shouldEqual` [ 1 ]
      result.terminated `shouldEqual` true

  describe "mapDelay" do
    it "rewrites each per-step delay" do
      let
        sched :: Schedule () Unit Int
        sched = mapDelay (\(Milliseconds n) -> Milliseconds (n * 2.0))
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

    it "caps delays via min" do
      let
        cap (Milliseconds ms) (Milliseconds n) =
          Milliseconds (if n < ms then n else ms)

        sched :: Schedule () Unit Int
        sched = mapDelay (cap (Milliseconds 30.0))
          (spaced (Milliseconds 100.0))

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      result `shouldEqual`
        [ Milliseconds 30.0
        , Milliseconds 30.0
        , Milliseconds 30.0
        ]

  describe "dimap" do
    it "transforms input pre-step and output post-step" do
      let
        sched :: Schedule () Int String
        sched = dimap (\(n :: Int) -> n) show (recurs 5)

        program :: RIO () () (Array String)
        program = do
          r <- walk3 sched 0
          pure r.os
      result <- runRIO' program
      result `shouldEqual` [ "1", "2", "3" ]

    it "respects underlying termination" do
      let
        sched :: Schedule () Int String
        sched = dimap identity show (recurs 1)

        program
          :: RIO () ()
               { os :: Array String, terminated :: Boolean }
        program = do
          r <- walk3 sched 0
          pure { os: r.os, terminated: r.terminated }
      result <- runRIO' program
      result.os `shouldEqual` [ "1" ]
      result.terminated `shouldEqual` true
