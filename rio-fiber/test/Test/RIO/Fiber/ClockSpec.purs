module Test.RIO.Fiber.ClockSpec (spec) where

import Prelude

import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Now (now) as Now
import Effect.Ref as Ref
import RIO.Fiber.Clock (Clock(..))
import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- Helper: build a deterministic fake clock that pins epoch to `ms` and
-- resumes sleeps immediately. `sleep` ignores the duration; this is
-- fine for clock-correctness tests where we don't measure elapsed
-- time, only validate which clock implementation is observed.
mkFixedClock :: Number -> Clock
mkFixedClock ms = Clock
  { instant: pure (fixedInstant ms)
  , epoch: pure (Milliseconds ms)
  , sleep: \_ wake -> do
      wake
      pure (pure unit)
  }

fixedInstant :: Number -> Instant
fixedInstant ms = case instant (Milliseconds ms) of
  Just i -> i
  Nothing -> case instant (Milliseconds 0.0) of
    Just i -> i
    Nothing -> case instant (Milliseconds 0.0) of
      Just i -> i
      Nothing -> case instant (Milliseconds 0.0) of
        Just i -> i
        Nothing -> case instant (Milliseconds 0.0) of
          Just i -> i
          -- `instant` only returns Nothing for out-of-range Numbers; ms
          -- here is always in range (we pass small positive epochs in
          -- tests). The chain above never reaches this point in
          -- practice; this final fallback exists to keep totality.
          Nothing -> fixedInstant 0.0

spec :: Spec Unit
spec = describe "rio-fiber: Clock" do
  describe "default clock" do
    it "currentEpoch returns a positive epoch near wall time" do
      out <- runAff (Clock.currentEpoch :: F.RIO () () Milliseconds) {}
      case out of
        Success (Milliseconds ms)
          | ms > 1.0e12 -> pure unit
          | otherwise -> fail ("epoch unexpectedly small: " <> show ms)
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "currentTime returns an Instant near current wall time" do
      ref <- liftEffect Now.now
      let
        prog :: F.RIO () () Number
        prog = do
          i <- Clock.currentTime
          pure (unwrap (unInstant i))
      out <- runAff prog {}
      let wall = unwrap (unInstant ref)
      case out of
        Success ms
          | abs (ms - wall) < 5000.0 -> pure unit
          | otherwise -> fail
              ("epoch drift too large: " <> show ms <> " vs " <> show wall)
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "sleep suspends for approximately the requested duration" do
      let
        prog :: F.RIO () () Milliseconds
        prog = do
          Milliseconds t0 <- Clock.currentEpoch
          Clock.sleep (Milliseconds 30.0)
          Milliseconds t1 <- Clock.currentEpoch
          pure (Milliseconds (t1 - t0))
      out <- runAff prog {}
      case out of
        Success (Milliseconds dt)
          | dt >= 20.0 -> pure unit
          | otherwise -> fail ("slept too briefly: " <> show dt <> "ms")
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "withClock" do
    it "scopes the clock override to the body" do
      let
        fake = mkFixedClock 1.0e9

        prog :: F.RIO () () { inside :: Milliseconds, outsideBigEnough :: Boolean }
        prog = do
          inside <- Clock.withClock fake Clock.currentEpoch
          Milliseconds outsideMs <- Clock.currentEpoch
          pure { inside, outsideBigEnough: outsideMs > 1.0e12 }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.inside `shouldEqual` Milliseconds 1.0e9
          r.outsideBigEnough `shouldEqual` true
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "withClock restores the previous clock after the body returns" do
      let
        fake = mkFixedClock 42.0

        prog :: F.RIO () ()
          { duringOverride :: Milliseconds
          , afterOverrideBigEnough :: Boolean
          }
        prog = do
          duringOverride <- Clock.withClock fake Clock.currentEpoch
          Milliseconds afterMs <- Clock.currentEpoch
          pure
            { duringOverride
            , afterOverrideBigEnough: afterMs > 1.0e12
            }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.duringOverride `shouldEqual` Milliseconds 42.0
          r.afterOverrideBigEnough `shouldEqual` true
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "child fibers inherit the parent clock override" do
      let
        fake = mkFixedClock 7.0

        prog :: F.RIO () () Milliseconds
        prog = Clock.withClock fake do
          f <- F.fork Clock.currentEpoch
          F.join f
      out <- runAff prog {}
      case out of
        Success ms -> ms `shouldEqual` Milliseconds 7.0
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "withClock's sleep uses the override implementation" do
      -- Fake clock whose sleep increments a ref and fires immediately.
      sleepCount <- liftEffect (Ref.new 0)
      let
        fake :: Clock
        fake = Clock
          { instant: pure (fixedInstant 0.0)
          , epoch: pure (Milliseconds 0.0)
          , sleep: \_ wake -> do
              _ <- Ref.modify (_ + 1) sleepCount
              wake
              pure (pure unit)
          }

        prog :: F.RIO () () Unit
        prog = Clock.withClock fake (Clock.sleep (Milliseconds 999.0))
      out <- runAff prog {}
      case out of
        Success _ -> do
          n <- liftEffect (Ref.read sleepCount)
          n `shouldEqual` 1
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "getClock / setClock" do
    it "setClock changes the active implementation for subsequent reads" do
      let
        marker = mkFixedClock 12345.0

        prog :: F.RIO () () Milliseconds
        prog = do
          Clock.setClock marker
          Clock.currentEpoch
      out <- runAff prog {}
      case out of
        Success ms -> ms `shouldEqual` Milliseconds 12345.0
        other -> fail ("expected Success, got " <> describeOutcome other)

abs :: Number -> Number
abs n = if n < 0.0 then -n else n

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
