module Test.RIO.CircuitBreakerSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Clock (Clock)
import RIO.CircuitBreaker
  ( CircuitBreaker
  , Phase(..)
  , make
  , reset
  , snapshot
  , tryWithBreaker
  , withBreaker
  )
import RIO.Core (RIO, catchAll, fail, provideAll, runRIO)
import RIO.Test.Clock (newTestClock)

type Env = { clock :: Clock }

runE
  :: forall e a
   . Env
  -> RIO (clock :: Clock) e a
  -> Aff (Either (Variant e) a)
runE env p = runRIO (provideAll env p)

type Errs = (boom :: Unit, circuitOpen :: Unit)

ok :: forall r. Int -> RIO r Errs Int
ok n = pure n

boom :: forall r a. RIO r Errs a
boom = fail (Proxy :: Proxy "boom") unit

-- | Run an inner program through the breaker but discard any
-- | typed failure it raises; the state-change side-effect on the
-- | breaker is still recorded.
runWithBreakerSwallow
  :: forall r
   . CircuitBreaker
  -> RIO (clock :: Clock | r) Errs Int
  -> RIO (clock :: Clock | r) Errs Unit
runWithBreakerSwallow cb action =
  catchAll (\_ -> pure unit) (void (withBreaker cb action))

spec :: Spec Unit
spec = describe "RIO.CircuitBreaker" do
  describe "snapshot" do
    it "starts Closed with 0 failures" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      result <- runE env do
        cb <- make { maxFailures: 3, resetTimeout: Milliseconds 1000.0 }
        snapshot cb
      case result of
        Right s -> do
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 0
          s.openedAt `shouldEqual` Nothing
        Left _ -> 1 `shouldEqual` 0

  describe "withBreaker (Closed phase)" do
    it "passes successful calls through and clears any pending failures" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      result <- runE env do
        cb <- make { maxFailures: 3, resetTimeout: Milliseconds 1000.0 }
        n <- withBreaker cb (ok 7)
        s <- snapshot cb
        pure { n, s }
      case result of
        Right { n, s } -> do
          n `shouldEqual` 7
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 0
        Left _ -> 1 `shouldEqual` 0

    it "counts a typed failure without tripping below the threshold" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      result <- runE env do
        cb <- make { maxFailures: 3, resetTimeout: Milliseconds 1000.0 }
        runWithBreakerSwallow cb boom
        runWithBreakerSwallow cb boom
        snapshot cb
      case result of
        Right s -> do
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 2
        Left _ -> 1 `shouldEqual` 0

    it "trips to Open when failure count reaches maxFailures" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      result <- runE env do
        cb <- make { maxFailures: 2, resetTimeout: Milliseconds 1000.0 }
        runWithBreakerSwallow cb boom
        runWithBreakerSwallow cb boom
        snapshot cb
      case result of
        Right s -> do
          s.phase `shouldEqual` Open
          s.failures `shouldEqual` 2
        Left _ -> 1 `shouldEqual` 0

    it "a successful call clears the failure counter" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      result <- runE env do
        cb <- make { maxFailures: 3, resetTimeout: Milliseconds 1000.0 }
        runWithBreakerSwallow cb boom
        runWithBreakerSwallow cb boom
        _ <- withBreaker cb (ok 1)
        snapshot cb
      case result of
        Right s -> do
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 0
        Left _ -> 1 `shouldEqual` 0

  describe "Open phase" do
    it "fails fast with circuitOpen without invoking the action" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      cbE <- runE env (make { maxFailures: 1, resetTimeout: Milliseconds 1000.0 })
      case cbE of
        Right breaker -> do
          -- Trip it first.
          _ <- runE env (runWithBreakerSwallow breaker boom)
          -- Now any call fails fast: the action below should NOT
          -- run (otherwise we'd see Right 99).
          r <- runE env (withBreaker breaker (ok 99))
          case r of
            Left _ -> pure unit -- expected: circuitOpen
            Right _ -> 1 `shouldEqual` 0
        Left _ -> 1 `shouldEqual` 0

    it "tryWithBreaker returns Nothing when Open instead of failing" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      result <- runE env do
        cb <- make { maxFailures: 1, resetTimeout: Milliseconds 1000.0 }
        runWithBreakerSwallow cb boom
        tryWithBreaker cb (ok 99)
      case result of
        Right Nothing -> pure unit
        _ -> 1 `shouldEqual` 0

  describe "HalfOpen phase (after resetTimeout)" do
    it "transitions Open -> HalfOpen after the timeout elapses, allowing a trial" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      cbE <- runE env (make { maxFailures: 1, resetTimeout: Milliseconds 500.0 })
      case cbE of
        Right cb -> do
          -- Trip the breaker.
          _ <- runE env (runWithBreakerSwallow cb boom)
          sOpen <- runE env (snapshot cb)
          case sOpen of
            Right s -> s.phase `shouldEqual` Open
            Left _ -> 1 `shouldEqual` 0

          -- Advance past the timeout.
          tc.advance (Milliseconds 600.0)
          -- A successful trial closes the breaker.
          ok' <- runE env (withBreaker cb (ok 42))
          ok' `shouldEqual` Right 42

          sAfter <- runE env (snapshot cb)
          case sAfter of
            Right s -> do
              s.phase `shouldEqual` Closed
              s.failures `shouldEqual` 0
            Left _ -> 1 `shouldEqual` 0
        Left _ -> 1 `shouldEqual` 0

    it "a failing trial re-opens the breaker" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      cbE <- runE env (make { maxFailures: 1, resetTimeout: Milliseconds 500.0 })
      case cbE of
        Right cb -> do
          _ <- runE env (runWithBreakerSwallow cb boom)
          tc.advance (Milliseconds 600.0)
          -- Trial fails; breaker should trip back to Open.
          _ <- runE env (runWithBreakerSwallow cb boom)
          sAfter <- runE env (snapshot cb)
          case sAfter of
            Right s -> s.phase `shouldEqual` Open
            Left _ -> 1 `shouldEqual` 0
        Left _ -> 1 `shouldEqual` 0

  describe "reset" do
    it "force-closes the breaker and clears the failure counter" do
      tc <- newTestClock
      let env = { clock: tc.clock }
      result <- runE env do
        cb <- make { maxFailures: 1, resetTimeout: Milliseconds 1000.0 }
        runWithBreakerSwallow cb boom
        reset cb
        snapshot cb
      case result of
        Right s -> do
          s.phase `shouldEqual` Closed
          s.failures `shouldEqual` 0
          s.openedAt `shouldEqual` Nothing
        Left _ -> 1 `shouldEqual` 0
