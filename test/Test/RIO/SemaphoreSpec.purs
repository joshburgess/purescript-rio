module Test.RIO.SemaphoreSpec (spec) where

import Prelude

import Effect.Aff (Milliseconds(..), delay, forkAff, joinFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Semaphore (available, make, withPermit, withPermits)

spec :: Spec Unit
spec = do
  describe "RIO.Semaphore" do

    describe "withPermit" do
      it "lets two non-overlapping holders pass when the count is 1" do
        sem <- liftEffect (make 1)
        let
          program :: RIO () () Int
          program = withPermit sem (pure 1) *> withPermit sem (pure 2)
        r <- runRIO' program
        r `shouldEqual` 2
        a <- liftEffect (available sem)
        a `shouldEqual` 1

      it "serializes overlapping holders so in/out never interleave" do
        sem <- liftEffect (make 1)
        log <- liftEffect (Ref.new ([] :: Array String))
        let
          record :: String -> RIO () () Unit
          record s = liftEffect
            (Ref.modify_ (\xs -> xs <> [ s ]) log)

          worker :: String -> RIO () () Unit
          worker name = withPermit sem do
            record (name <> "-in")
            liftAff (delay (Milliseconds 20.0))
            record (name <> "-out")

        f1 <- forkAff (runRIO' (worker "a"))
        f2 <- forkAff (runRIO' (worker "b"))
        joinFiber f1
        joinFiber f2
        result <- liftEffect (Ref.read log)
        -- Whichever worker entered first must completely exit
        -- before the other entered.
        let
          ok = case result of
            [ "a-in", "a-out", "b-in", "b-out" ] -> true
            [ "b-in", "b-out", "a-in", "a-out" ] -> true
            _ -> false
        ok `shouldEqual` true

    describe "withPermits" do
      it "blocks until enough permits are available" do
        sem <- liftEffect (make 2)
        flag <- liftEffect (Ref.new false)
        let
          holder :: RIO () () Unit
          holder = withPermits 2 sem (liftAff (delay (Milliseconds 30.0)))

          big :: RIO () () Unit
          big = withPermits 2 sem do
            liftEffect (Ref.write true flag)

        f1 <- forkAff (runRIO' holder)
        delay (Milliseconds 5.0)
        before <- liftEffect (Ref.read flag)
        f2 <- forkAff (runRIO' big)
        delay (Milliseconds 5.0)
        midway <- liftEffect (Ref.read flag)
        joinFiber f1
        joinFiber f2
        after <- liftEffect (Ref.read flag)
        before `shouldEqual` false
        midway `shouldEqual` false
        after `shouldEqual` true

    describe "available" do
      it "reflects make's initial count and clamps negatives to zero" do
        s1 <- liftEffect (make 3)
        a1 <- liftEffect (available s1)
        a1 `shouldEqual` 3
        s2 <- liftEffect (make (-5))
        a2 <- liftEffect (available s2)
        a2 `shouldEqual` 0
