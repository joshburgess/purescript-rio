module Test.RIO.Fiber.CircuitBreakerSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Fiber.CircuitBreaker
  ( CircuitBreaker
  , Phase(..)
  , make
  , reset
  , snapshot
  , tryWithBreaker
  , withBreaker
  )
import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (Outcome(..), RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.TestClock as TestClock

type Errs = (boom :: Unit, circuitOpen :: Unit)

boomTag :: Proxy "boom"
boomTag = Proxy

ok :: forall r. Int -> RIO r Errs Int
ok n = pure n

boom :: forall r a. RIO r Errs a
boom = F.fail (Variant.inj boomTag unit)

-- | Run an inner program through the breaker but discard any typed
-- | failure it raises; the state-change side-effect on the breaker
-- | is still recorded.
runWithBreakerSwallow
  :: forall r. CircuitBreaker -> RIO r Errs Int -> RIO r Errs Unit
runWithBreakerSwallow cb action =
  F.catchAll (\_ -> pure unit) (void (withBreaker cb action))

spec :: Spec Unit
spec = describe "rio-fiber: CircuitBreaker" do

  describe "snapshot" do
    it "starts Closed with 0 failures" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () () _
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 3, resetTimeout: Milliseconds 1000.0 }
          snapshot cb
      out <- runAff prog {}
      case out of
        Success s -> do
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 0
          s.openedAt `shouldEqual` Nothing
        _ -> fail "expected Success"

  describe "withBreaker (Closed phase)" do
    it "passes successful calls through and clears any pending failures" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs _
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 3, resetTimeout: Milliseconds 1000.0 }
          n <- withBreaker cb (ok 7)
          s <- snapshot cb
          pure { n, s }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.n `shouldEqual` 7
          r.s.phase `shouldEqual` Closed
          r.s.failures `shouldEqual` 0
        _ -> fail "expected Success"

    it "counts a typed failure without tripping below the threshold" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs _
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 3, resetTimeout: Milliseconds 1000.0 }
          runWithBreakerSwallow cb boom
          runWithBreakerSwallow cb boom
          snapshot cb
      out <- runAff prog {}
      case out of
        Success s -> do
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 2
        _ -> fail "expected Success"

    it "trips to Open when failure count reaches maxFailures" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs _
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 2, resetTimeout: Milliseconds 1000.0 }
          runWithBreakerSwallow cb boom
          runWithBreakerSwallow cb boom
          snapshot cb
      out <- runAff prog {}
      case out of
        Success s -> do
          s.phase `shouldEqual` Open
          s.failures `shouldEqual` 2
        _ -> fail "expected Success"

    it "a successful call clears the failure counter" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs _
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 3, resetTimeout: Milliseconds 1000.0 }
          runWithBreakerSwallow cb boom
          runWithBreakerSwallow cb boom
          _ <- withBreaker cb (ok 1)
          snapshot cb
      out <- runAff prog {}
      case out of
        Success s -> do
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 0
        _ -> fail "expected Success"

  describe "Open phase" do
    it "fails fast with circuitOpen without invoking the action" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs Int
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 1, resetTimeout: Milliseconds 1000.0 }
          runWithBreakerSwallow cb boom
          withBreaker cb (ok 99)
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        _ -> fail "expected typed Fail (circuitOpen)"

    it "tryWithBreaker returns Nothing when Open instead of failing" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs (Maybe Int)
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 1, resetTimeout: Milliseconds 1000.0 }
          runWithBreakerSwallow cb boom
          tryWithBreaker cb (ok 99)
      out <- runAff prog {}
      case out of
        Success Nothing -> pure unit
        _ -> fail "expected Success Nothing"

  describe "HalfOpen phase (after resetTimeout)" do
    it "transitions Open -> HalfOpen after the timeout elapses, allowing a trial" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs _
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 1, resetTimeout: Milliseconds 500.0 }
          runWithBreakerSwallow cb boom
          sOpen <- snapshot cb
          TestClock.advance tc (Milliseconds 600.0)
          n <- withBreaker cb (ok 42)
          sAfter <- snapshot cb
          pure { sOpen, n, sAfter }
      out <- runAff prog {}
      case out of
        Success r -> do
          r.sOpen.phase `shouldEqual` Open
          r.n `shouldEqual` 42
          r.sAfter.phase `shouldEqual` Closed
          r.sAfter.failures `shouldEqual` 0
        _ -> fail "expected Success"

    it "a failing trial re-opens the breaker" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs _
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 1, resetTimeout: Milliseconds 500.0 }
          runWithBreakerSwallow cb boom
          TestClock.advance tc (Milliseconds 600.0)
          runWithBreakerSwallow cb boom
          snapshot cb
      out <- runAff prog {}
      case out of
        Success s -> s.phase `shouldEqual` Open
        _ -> fail "expected Success"

  describe "reset" do
    it "force-closes the breaker and clears the failure counter" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      let
        prog :: RIO () Errs _
        prog = Clock.withClock (TestClock.clock tc) do
          cb <- make { maxFailures: 1, resetTimeout: Milliseconds 1000.0 }
          runWithBreakerSwallow cb boom
          reset cb
          snapshot cb
      out <- runAff prog {}
      case out of
        Success s -> do
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 0
          s.openedAt `shouldEqual` Nothing
        _ -> fail "expected Success"
