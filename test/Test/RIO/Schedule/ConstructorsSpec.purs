module Test.RIO.Schedule.ConstructorsSpec (spec) where

import Prelude

import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Schedule (Schedule, Step(..), fromFunction, step, unfold)

walk3
  :: forall i o
   . Schedule () i o
  -> i
  -> RIO () ()
       { os :: Array o
       , ds :: Array Milliseconds
       }
walk3 s0 i = do
  out1 <- step s0 i
  case out1 of
    Done -> pure { os: [], ds: [] }
    Continue o1 d1 s1 -> do
      out2 <- step s1 i
      case out2 of
        Done -> pure { os: [ o1 ], ds: [ d1 ] }
        Continue o2 d2 s2 -> do
          out3 <- step s2 i
          case out3 of
            Done -> pure { os: [ o1, o2 ], ds: [ d1, d2 ] }
            Continue o3 d3 _ ->
              pure { os: [ o1, o2, o3 ], ds: [ d1, d2, d3 ] }

spec :: Spec Unit
spec = describe "RIO.Schedule (constructors)" do

  describe "unfold" do
    it "threads state across iterations" do
      let
        sched :: Schedule () Int Int
        sched = unfold 0 \acc i ->
          let
            next = acc + i
          in
            { output: next, delay: Milliseconds 0.0, state: next }

        program :: RIO () () (Array Int)
        program = do
          r <- walk3 sched 5
          pure r.os
      result <- runRIO' program
      result `shouldEqual` [ 5, 10, 15 ]

    it "can emit different per-step delays from the state" do
      let
        sched :: Schedule () Unit Int
        sched = unfold 1 \n _ ->
          { output: n
          , delay: Milliseconds (100.0 * toNumber n)
          , state: n + 1
          }

        program :: RIO () () (Array Milliseconds)
        program = do
          r <- walk3 sched unit
          pure r.ds
      result <- runRIO' program
      result `shouldEqual`
        [ Milliseconds 100.0
        , Milliseconds 200.0
        , Milliseconds 300.0
        ]

    it "never terminates on its own" do
      let
        sched :: Schedule () Unit Int
        sched = unfold 0 \n _ ->
          { output: n, delay: Milliseconds 0.0, state: n + 1 }

        program :: RIO () () Int
        program = do
          out1 <- step sched unit
          case out1 of
            Done -> pure (-1)
            Continue _ _ next1 -> do
              out2 <- step next1 unit
              case out2 of
                Done -> pure (-1)
                Continue o _ _ -> pure o
      result <- runRIO' program
      result `shouldEqual` 1

  describe "fromFunction" do
    it "emits a stateless per-input decision" do
      let
        sched :: Schedule () Int Int
        sched = fromFunction \i ->
          { output: i + 100, delay: Milliseconds (toNumber i) }

        program :: RIO () () { os :: Array Int, ds :: Array Milliseconds }
        program = do
          out1 <- step sched 7
          case out1 of
            Done -> pure { os: [], ds: [] }
            Continue o1 d1 next1 -> do
              out2 <- step next1 9
              case out2 of
                Done -> pure { os: [ o1 ], ds: [ d1 ] }
                Continue o2 d2 _ ->
                  pure { os: [ o1, o2 ], ds: [ d1, d2 ] }
      result <- runRIO' program
      result.os `shouldEqual` [ 107, 109 ]
      result.ds `shouldEqual` [ Milliseconds 7.0, Milliseconds 9.0 ]

    it "has no memory across steps" do
      let
        sched :: Schedule () Int Int
        sched = fromFunction \i ->
          { output: i, delay: Milliseconds 0.0 }

        program :: RIO () () (Array Int)
        program = do
          r <- walk3 sched 42
          pure r.os
      result <- runRIO' program
      result `shouldEqual` [ 42, 42, 42 ]
