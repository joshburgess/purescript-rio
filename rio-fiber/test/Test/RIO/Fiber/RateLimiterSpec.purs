module Test.RIO.Fiber.RateLimiterSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (Outcome(..), RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.RateLimiter
  ( acquire
  , available
  , make
  , tryAcquire
  , withPermit
  )
import RIO.Fiber.TestClock as TestClock

spec :: Spec Unit
spec = describe "rio-fiber: RateLimiter (token bucket)" do

  it "tryAcquire drains the burst then refuses" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      prog :: RIO () () (Array Boolean)
      prog = Clock.withClock (TestClock.clock tc) do
        rl <- make { permitsPerSecond: 1.0, burst: 3 }
        a <- tryAcquire rl
        b <- tryAcquire rl
        c <- tryAcquire rl
        d <- tryAcquire rl
        pure [ a, b, c, d ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ true, true, true, false ]
      _ -> fail "expected Success"

  it "tryAcquire succeeds after a refill" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      prog :: RIO () () { firstDrain :: Boolean, emptyBefore :: Boolean, refilled :: Boolean }
      prog = Clock.withClock (TestClock.clock tc) do
        rl <- make { permitsPerSecond: 4.0, burst: 1 }
        firstDrain <- tryAcquire rl
        emptyBefore <- tryAcquire rl
        TestClock.advance tc (Milliseconds 250.0)
        refilled <- tryAcquire rl
        pure { firstDrain, emptyBefore, refilled }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.firstDrain `shouldEqual` true
        r.emptyBefore `shouldEqual` false
        r.refilled `shouldEqual` true
      _ -> fail "expected Success"

  it "available reflects the current bucket reading" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      prog :: RIO () () { a0 :: Number, a1 :: Number }
      prog = Clock.withClock (TestClock.clock tc) do
        rl <- make { permitsPerSecond: 1.0, burst: 5 }
        a0 <- available rl
        _ <- tryAcquire rl
        _ <- tryAcquire rl
        a1 <- available rl
        pure { a0, a1 }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.a0 `shouldEqual` 5.0
        r.a1 `shouldEqual` 3.0
      _ -> fail "expected Success"

  it "blocking acquire suspends until time advances enough" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    flag <- liftEffect (Ref.new false)
    let
      prog :: RIO () () { before :: Boolean, mid :: Boolean, finalFlag :: Boolean }
      prog = Clock.withClock (TestClock.clock tc) do
        rl <- make { permitsPerSecond: 2.0, burst: 1 }
        _ <- tryAcquire rl
        fib <- F.fork do
          acquire rl
          F.liftEffect (Ref.write true flag)
        -- yield so the worker parks
        F.sleep (Milliseconds 0.0)
        before <- F.liftEffect (Ref.read flag)

        -- 250ms isn't enough at 2 permits/s; need 500ms.
        TestClock.advance tc (Milliseconds 250.0)
        F.sleep (Milliseconds 0.0)
        mid <- F.liftEffect (Ref.read flag)

        TestClock.advance tc (Milliseconds 250.0)
        _ <- F.join fib
        finalFlag <- F.liftEffect (Ref.read flag)
        pure { before, mid, finalFlag }
    -- The fork's sleep uses the test clock; we don't need wall-time
    -- waits in the test thread itself. Still, give the Aff scheduler
    -- a beat to drain microtasks before reading the outcome.
    out <- runAff prog {}
    Aff.delay (Milliseconds 0.0)
    case out of
      Success r -> do
        r.before `shouldEqual` false
        r.mid `shouldEqual` false
        r.finalFlag `shouldEqual` true
      _ -> fail "expected Success"

  it "withPermit gates a body on acquiring a token" do
    tc <- liftEffect (TestClock.make (Milliseconds 0.0))
    let
      prog :: RIO () () { out :: Int, avail :: Number }
      prog = Clock.withClock (TestClock.clock tc) do
        rl <- make { permitsPerSecond: 1.0, burst: 2 }
        out <- withPermit rl (pure 42)
        avail <- available rl
        pure { out, avail }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.out `shouldEqual` 42
        r.avail `shouldEqual` 1.0
      _ -> fail "expected Success"
