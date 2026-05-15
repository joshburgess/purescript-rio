module Test.RIO.Schedule.CollectRepetitionsTapSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Schedule
  ( Schedule
  , Step(..)
  , collectAll
  , recurs
  , repetitions
  , spaced
  , step
  , tapOutput
  )

-- Walk a schedule up to three Continue steps, returning the
-- outputs in order and whether the schedule terminated within
-- those steps. Lets us assert step-by-step output sequences
-- without paying the clock cost of `repeat`.
walk3
  :: forall o
   . Schedule () Unit o
  -> RIO () () { os :: Array o, terminated :: Boolean }
walk3 s0 = do
  out1 <- step s0 unit
  case out1 of
    Done -> pure { os: [], terminated: true }
    Continue o1 _ s1 -> do
      out2 <- step s1 unit
      case out2 of
        Done -> pure { os: [ o1 ], terminated: true }
        Continue o2 _ s2 -> do
          out3 <- step s2 unit
          case out3 of
            Done -> pure { os: [ o1, o2 ], terminated: true }
            Continue o3 _ _ -> pure { os: [ o1, o2, o3 ], terminated: false }

spec :: Spec Unit
spec = describe "RIO.Schedule (collectAll / repetitions / tapOutput)" do

  describe "collectAll" do
    it "each step emits the cumulative array of original outputs" do
      let
        sched :: Schedule () Unit (Array Int)
        sched = collectAll (recurs 5)

        program :: RIO () () { os :: Array (Array Int), terminated :: Boolean }
        program = walk3 sched
      result <- runRIO' program
      result.os `shouldEqual`
        [ [ 1 ]
        , [ 1, 2 ]
        , [ 1, 2, 3 ]
        ]
      result.terminated `shouldEqual` false

    it "terminates when the underlying schedule terminates" do
      let
        sched :: Schedule () Unit (Array Int)
        sched = collectAll (recurs 2)

        program :: RIO () () (Array (Array Int))
        program = do
          r <- walk3 sched
          pure r.os
      result <- runRIO' program
      result `shouldEqual` [ [ 1 ], [ 1, 2 ] ]

  describe "repetitions" do
    it "emits the 1-based iteration count instead of the original output" do
      let
        sched :: Schedule () Unit Int
        sched = repetitions (spaced (Milliseconds 0.0))

        program :: RIO () () (Array Int)
        program = do
          r <- walk3 sched
          pure r.os
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "respects the underlying schedule's termination" do
      let
        sched :: Schedule () Unit Int
        sched = repetitions (recurs 1)

        program :: RIO () () { os :: Array Int, terminated :: Boolean }
        program = walk3 sched
      result <- runRIO' program
      result.os `shouldEqual` [ 1 ]
      result.terminated `shouldEqual` true

  describe "tapOutput" do
    it "fires the handler for every Continue, passing outputs through unchanged" do
      seen <- liftEffect (Ref.new ([] :: Array Int))
      let
        sched :: Schedule () Unit Int
        sched = tapOutput
          (\o -> liftEffect (Ref.modify_ (\xs -> xs <> [ o ]) seen))
          (recurs 3)

        program :: RIO () () (Array Int)
        program = do
          r <- walk3 sched
          pure r.os
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]
      observed <- liftEffect (Ref.read seen)
      observed `shouldEqual` [ 1, 2, 3 ]

    it "does not fire when the schedule terminates" do
      seen <- liftEffect (Ref.new 0)
      let
        sched :: Schedule () Unit Int
        sched = tapOutput
          (\_ -> liftEffect (Ref.modify_ (_ + 1) seen))
          (recurs 0)

        program :: RIO () () { terminated :: Boolean }
        program = do
          r <- walk3 sched
          pure { terminated: r.terminated }
      result <- runRIO' program
      result.terminated `shouldEqual` true
      observed <- liftEffect (Ref.read seen)
      observed `shouldEqual` 0
