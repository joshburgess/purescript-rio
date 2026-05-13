module Test.RIO.ClockSpec (spec) where

import Prelude

import Data.Newtype (un)
import Effect.Aff (Milliseconds(..))
import Effect.Aff (delay, error, forkAff, killFiber) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Clock (Clock, now, sleep)
import RIO.Core (RIO, provideAll, runRIO')
import RIO.Test.Clock (newTestClock)

spec :: Spec Unit
spec = do
  describe "RIO.Clock + RIO.Test.Clock (Phase 7.1)" do
    describe "now" do
      it "returns the test clock's current virtual time" do
        tc <- newTestClock
        let
          program :: RIO (clock :: Clock) () Milliseconds
          program = now
        m <- runRIO' (provideAll { clock: tc.clock } program)
        un Milliseconds m `shouldEqual` 0.0

        tc.advance (Milliseconds 250.0)
        m' <- runRIO' (provideAll { clock: tc.clock } program)
        un Milliseconds m' `shouldEqual` 250.0

    describe "sleep" do
      it "does not return until advance has pushed past its deadline" do
        tc <- newTestClock
        flag <- liftEffect (Ref.new false)
        let
          program :: RIO (clock :: Clock) () Unit
          program = do
            sleep (Milliseconds 100.0)
            liftEffect (Ref.write true flag)

        _ <- Aff.forkAff (runRIO' (provideAll { clock: tc.clock } program))

        Aff.delay (Milliseconds 0.0)
        before <- liftEffect (Ref.read flag)
        before `shouldEqual` false

        tc.advance (Milliseconds 50.0)
        Aff.delay (Milliseconds 0.0)
        mid <- liftEffect (Ref.read flag)
        mid `shouldEqual` false

        tc.advance (Milliseconds 50.0)
        Aff.delay (Milliseconds 0.0)
        after <- liftEffect (Ref.read flag)
        after `shouldEqual` true

      it "two sleepers fire in deadline order within one advance" do
        tc <- newTestClock
        events <- liftEffect (Ref.new [])
        let
          push :: forall r. String -> RIO r () Unit
          push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

          first :: RIO (clock :: Clock) () Unit
          first = sleep (Milliseconds 200.0) *> push "first"

          second :: RIO (clock :: Clock) () Unit
          second = sleep (Milliseconds 100.0) *> push "second"

        _ <- Aff.forkAff (runRIO' (provideAll { clock: tc.clock } first))
        _ <- Aff.forkAff (runRIO' (provideAll { clock: tc.clock } second))
        Aff.delay (Milliseconds 0.0)

        tc.advance (Milliseconds 250.0)
        Aff.delay (Milliseconds 0.0)
        Aff.delay (Milliseconds 0.0)

        result <- liftEffect (Ref.read events)
        result `shouldEqual` [ "second", "first" ]

      it "an interrupted fiber's sleeper is canceled cleanly" do
        tc <- newTestClock
        flag <- liftEffect (Ref.new false)
        let
          program :: RIO (clock :: Clock) () Unit
          program = do
            sleep (Milliseconds 100.0)
            liftEffect (Ref.write true flag)
        fib <- Aff.forkAff
          (runRIO' (provideAll { clock: tc.clock } program))
        Aff.delay (Milliseconds 0.0)
        Aff.killFiber (Aff.error "interrupt-test") fib
        tc.advance (Milliseconds 1000.0)
        Aff.delay (Milliseconds 0.0)
        finalFlag <- liftEffect (Ref.read flag)
        finalFlag `shouldEqual` false
