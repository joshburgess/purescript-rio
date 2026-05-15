module Test.RIO.ClockSpec (spec) where

import Prelude

import Data.Newtype (un)
import Effect.Aff (Milliseconds(..))
import Effect.Aff (delay, error, forkAff, killFiber) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Tuple (Tuple(..))

import RIO.Clock (Clock, now, sleep, timed)
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

      it "a zero-duration sleep returns immediately without an advance" do
        -- The test clock's sleep takes a short-circuit branch
        -- when `deadlineMs <= current`, resuming the sleeper
        -- without parking it on the pending list. Pin that branch
        -- so a future refactor that uniformly parks every sleeper
        -- (and silently hangs zero-duration sleepers until the
        -- next advance) is caught.
        tc <- newTestClock
        flag <- liftEffect (Ref.new false)
        let
          program :: RIO (clock :: Clock) () Unit
          program = do
            sleep (Milliseconds 0.0)
            liftEffect (Ref.write true flag)
        runRIO' (provideAll { clock: tc.clock } program)
        ran <- liftEffect (Ref.read flag)
        ran `shouldEqual` true

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

    describe "timed" do
      it "reports zero duration when the action does not advance the clock" do
        -- Without any sleep / advance in the body, both `now`
        -- samples return the same virtual time and the elapsed
        -- duration is exactly zero.
        tc <- newTestClock
        let
          program :: RIO (clock :: Clock) () (Tuple Milliseconds Int)
          program = timed (pure 7)
        Tuple elapsed value <- runRIO'
          (provideAll { clock: tc.clock } program)
        un Milliseconds elapsed `shouldEqual` 0.0
        value `shouldEqual` 7

      it "reports the advance between the before- and after-samples" do
        -- The body advances the test clock between the two `now`
        -- samples taken inside `timed`. The reported elapsed is
        -- whatever virtual time accrued between the two samples.
        tc <- newTestClock
        _ <- Aff.forkAff do
          Aff.delay (Milliseconds 5.0)
          tc.advance (Milliseconds 125.0)
        let
          program :: RIO (clock :: Clock) () (Tuple Milliseconds Unit)
          program = timed (sleep (Milliseconds 1.0))
        Tuple elapsed _ <- runRIO'
          (provideAll { clock: tc.clock } program)
        -- The fork advanced the clock by 125ms in between the
        -- two `now` samples that `timed` brackets the action with.
        un Milliseconds elapsed `shouldEqual` 125.0

      it "preserves the action's return value alongside the duration" do
        tc <- newTestClock
        let
          program :: RIO (clock :: Clock) () (Tuple Milliseconds String)
          program = timed (pure "result")
        Tuple _ value <- runRIO'
          (provideAll { clock: tc.clock } program)
        value `shouldEqual` "result"
