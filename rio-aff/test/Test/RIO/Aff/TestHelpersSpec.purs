module Test.RIO.Aff.TestHelpersSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, provideAll, runRIO)
import RIO.Aff.Test (Recording, mockService, recording)

-- A tiny service whose only operation pushes a message somewhere.
type Notifier =
  { send :: String -> Aff Unit
  }

send :: forall r e. String -> RIO (notifier :: Notifier | r) e Unit
send msg = do
  n <- ask (Proxy :: Proxy "notifier")
  liftAff (n.send msg)

-- A program that sends three notifications.
program :: forall r e. RIO (notifier :: Notifier | r) e Unit
program = do
  send "started"
  send "midway"
  send "done"

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Test (Phase 2.6)" do
    describe "mockService" do
      it "is an alias for provide that reads as 'mock this service'" do
        rec <- recording
        let
          fakeNotifier :: Notifier
          fakeNotifier = { send: rec.record }

          runnable :: RIO () () Unit
          runnable = mockService (Proxy :: Proxy "notifier") fakeNotifier program
        result <- runRIO runnable
        result `shouldEqual` Right unit

    describe "recording" do
      it "captures each call in order, observable after the run" do
        rec <- recording
        let
          fakeNotifier :: Notifier
          fakeNotifier = { send: rec.record }

          runnable :: RIO () () Unit
          runnable = provideAll { notifier: fakeNotifier } program
        _ <- runRIO runnable
        calls <- rec.calls
        calls `shouldEqual` [ "started", "midway", "done" ]

      it "starts empty" do
        rec <- recording :: Aff (Recording String)
        calls <- rec.calls
        calls `shouldEqual` []
