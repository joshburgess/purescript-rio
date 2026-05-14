module Test.RIO.SemaphoreSpec (spec) where

import Prelude

import Effect.Aff
  ( Milliseconds(..)
  , attempt
  , delay
  , error
  , forkAff
  , joinFiber
  , killFiber
  )
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, die, fail, runRIO, runRIO')
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

    describe "release on every termination path" do
      it "withPermit returns the permit after a typed failure" do
        sem <- liftEffect (make 1)
        let
          program :: RIO () (boom :: Unit) Unit
          program = withPermit sem (fail (Proxy :: Proxy "boom") unit)
        _ <- runRIO program
        a <- liftEffect (available sem)
        a `shouldEqual` 1

      it "withPermit returns the permit after a defect" do
        sem <- liftEffect (make 1)
        let
          program :: RIO () () Unit
          program = withPermit sem (die (error "boom"))
        _ <- attempt (runRIO' program)
        a <- liftEffect (available sem)
        a `shouldEqual` 1

      it "withPermits returns all permits after a typed failure mid-body" do
        sem <- liftEffect (make 3)
        let
          program :: RIO () (boom :: Unit) Unit
          program = withPermits 3 sem (fail (Proxy :: Proxy "boom") unit)
        _ <- runRIO program
        a <- liftEffect (available sem)
        a `shouldEqual` 3

    describe "withPermits boundary cases" do
      it "withPermits 0 runs without waiting and without changing the count" do
        sem <- liftEffect (make 2)
        let
          program :: RIO () () Int
          program = withPermits 0 sem (pure 7)
        r <- runRIO' program
        r `shouldEqual` 7
        a <- liftEffect (available sem)
        a `shouldEqual` 2

      it "withPermit on a semaphore made with 0 permits blocks the waiter" do
        sem <- liftEffect (make 0)
        flag <- liftEffect (Ref.new false)
        let
          waiter :: RIO () () Unit
          waiter = withPermit sem (liftEffect (Ref.write true flag))
        f <- forkAff (runRIO' waiter)
        delay (Milliseconds 10.0)
        ran <- liftEffect (Ref.read flag)
        ran `shouldEqual` false
        killFiber (error "test-cleanup") f
