module Test.RIO.Aff.Clock.PartsSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Clock (Clock, ClockParts, nowParts, partsFromMs)
import RIO.Aff.Core (RIO, provideAll, runRIO')
import RIO.Aff.Test.Clock (newTestClock)

spec :: Spec Unit
spec = describe "RIO.Aff.Clock (parts)" do

  describe "partsFromMs" do
    it "decomposes the Unix epoch as Jan 1 1970 00:00:00 UTC, Thursday" do
      case partsFromMs (Milliseconds 0.0) of
        Just p -> do
          p.year `shouldEqual` 1970
          p.month `shouldEqual` 1
          p.day `shouldEqual` 1
          p.hour `shouldEqual` 0
          p.minute `shouldEqual` 0
          p.second `shouldEqual` 0
          p.millisecond `shouldEqual` 0
          p.dayOfWeek `shouldEqual` 4
        Nothing -> 0 `shouldEqual` 1

    it "decomposes Jan 2 1970 00:00:00 UTC as a Friday" do
      case partsFromMs (Milliseconds 86400000.0) of
        Just p -> do
          p.day `shouldEqual` 2
          p.dayOfWeek `shouldEqual` 5
        Nothing -> 0 `shouldEqual` 1

    it "decomposes 1700000000000 ms as Nov 14 2023 22:13:20 UTC, Tuesday" do
      case partsFromMs (Milliseconds 1700000000000.0) of
        Just p -> do
          p.year `shouldEqual` 2023
          p.month `shouldEqual` 11
          p.day `shouldEqual` 14
          p.hour `shouldEqual` 22
          p.minute `shouldEqual` 13
          p.second `shouldEqual` 20
          p.dayOfWeek `shouldEqual` 2
        Nothing -> 0 `shouldEqual` 1

  describe "nowParts" do
    it "reads the test clock at t=0 as the Unix epoch" do
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () ClockParts
        program = nowParts
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result.year `shouldEqual` 1970
      result.month `shouldEqual` 1
      result.day `shouldEqual` 1
      result.hour `shouldEqual` 0
      result.dayOfWeek `shouldEqual` 4

    it "advances with the test clock" do
      tc <- newTestClock
      let
        -- 30 minutes 15 seconds 100 ms after epoch
        offset = Milliseconds (30.0 * 60000.0 + 15.0 * 1000.0 + 100.0)

        program :: RIO (clock :: Clock) () ClockParts
        program = do
          liftAff (tc.advance offset)
          nowParts
      result <- runRIO' (provideAll { clock: tc.clock } program)
      result.minute `shouldEqual` 30
      result.second `shouldEqual` 15
      result.millisecond `shouldEqual` 100
