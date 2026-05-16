module Test.RIO.TimeSpec (spec) where

import Prelude hiding (between)

import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Clock (Clock)
import RIO.Core (RIO, provideAll, runRIO')
import RIO.Test.Clock (newTestClock)
import RIO.Time
  ( Instant(..)
  , addDuration
  , between
  , days
  , diffMs
  , epoch
  , formatISO8601
  , fromMilliseconds
  , hours
  , humanize
  , minutes
  , nowInstant
  , parseISO8601
  , seconds
  , subDuration
  , toMilliseconds
  )

spec :: Spec Unit
spec = describe "RIO.Time" do
  describe "Duration constructors" do
    it "seconds / minutes / hours / days scale milliseconds" do
      un Milliseconds (seconds 1.0) `shouldEqual` 1000.0
      un Milliseconds (minutes 1.0) `shouldEqual` 60_000.0
      un Milliseconds (hours 1.0) `shouldEqual` 3_600_000.0
      un Milliseconds (days 1.0) `shouldEqual` 86_400_000.0

  describe "Instant arithmetic" do
    it "addDuration / subDuration are inverses" do
      let
        i = fromMilliseconds (Milliseconds 1000.0)
        d = seconds 5.0
      addDuration d (subDuration d i) `shouldEqual` i

    it "between returns a signed delta" do
      let
        a = fromMilliseconds (Milliseconds 1000.0)
        b = fromMilliseconds (Milliseconds 3500.0)
      un Milliseconds (between a b) `shouldEqual` 2500.0
      un Milliseconds (between b a) `shouldEqual` (-2500.0)

    it "diffMs agrees with between" do
      let
        a = fromMilliseconds (Milliseconds 0.0)
        b = fromMilliseconds (Milliseconds 9999.0)
      diffMs a b `shouldEqual` 9999.0

    it "epoch is 1970-01-01T00:00:00.000Z" do
      un Milliseconds (toMilliseconds epoch) `shouldEqual` 0.0

  describe "nowInstant via the test clock" do
    it "reads the test clock's virtual time as an Instant" do
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () Instant
        program = nowInstant
      Instant ms0 <- runRIO' (provideAll { clock: tc.clock } program)
      un Milliseconds ms0 `shouldEqual` 0.0

      tc.advance (Milliseconds 12_345.0)
      Instant ms1 <- runRIO' (provideAll { clock: tc.clock } program)
      un Milliseconds ms1 `shouldEqual` 12_345.0

  describe "ISO 8601 round-trip" do
    it "epoch renders as 1970-01-01T00:00:00.000Z" do
      s <- runRIO' (formatISO8601 epoch :: RIO () () String)
      s `shouldEqual` "1970-01-01T00:00:00.000Z"

    it "parseISO8601 inverts formatISO8601 for an arbitrary instant" do
      let
        original = fromMilliseconds (Milliseconds 1_750_000_000_123.0)
      rendered <- runRIO' (formatISO8601 original :: RIO () () String)
      parseISO8601 rendered `shouldEqual` Just original

    it "parseISO8601 returns Nothing for nonsense input" do
      parseISO8601 "not a date" `shouldEqual` Nothing

    it "parseISO8601 accepts the canonical Z form" do
      isJust (parseISO8601 "2026-05-15T12:34:56.789Z")
        `shouldEqual` true

  describe "humanize" do
    it "renders sub-second values in ms" do
      humanize (Milliseconds 0.0) `shouldEqual` "0ms"
      humanize (Milliseconds 250.0) `shouldEqual` "250ms"

    it "drops zero leading and trailing components" do
      humanize (seconds 5.0) `shouldEqual` "5s"
      humanize (minutes 1.0) `shouldEqual` "1m"
      humanize (hours 2.0) `shouldEqual` "2h"

    it "joins non-zero components with single spaces" do
      let d = hours 1.0 <> seconds 30.0
      humanize d `shouldEqual` "1h 30s"

    it "covers d / h / m / s in one call" do
      let d = days 1.0 <> hours 2.0 <> minutes 3.0 <> seconds 4.0
      humanize d `shouldEqual` "1d 2h 3m 4s"

    it "prefixes negative durations with a minus sign" do
      humanize (Milliseconds (-1500.0)) `shouldEqual` "-1s"
