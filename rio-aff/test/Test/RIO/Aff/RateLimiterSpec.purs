module Test.RIO.Aff.RateLimiterSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)
import Effect.Aff (delay, forkAff) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (RIO, provideAll, runRIO')
import RIO.Aff.RateLimiter
  ( acquire
  , available
  , make
  , tryAcquire
  , withPermit
  )
import RIO.Aff.Test.Clock (newTestClock)

runC
  :: forall a
   . { clock :: Clock }
  -> RIO (clock :: Clock) () a
  -> Aff a
runC env p = runRIO' (provideAll env p)

spec :: Spec Unit
spec = describe "RIO.Aff.RateLimiter (token bucket)" do
  it "tryAcquire drains the burst then refuses" do
    tc <- newTestClock
    let env = { clock: tc.clock }
    result <- runC env do
      rl <- make { permitsPerSecond: 1.0, burst: 3 }
      a <- tryAcquire rl
      b <- tryAcquire rl
      c <- tryAcquire rl
      d <- tryAcquire rl
      pure [ a, b, c, d ]
    result `shouldEqual` [ true, true, true, false ]

  it "tryAcquire succeeds after a refill" do
    tc <- newTestClock
    let env = { clock: tc.clock }
    rl <- runC env $ make { permitsPerSecond: 4.0, burst: 1 }
    -- Drain the burst.
    firstDrain <- runC env (tryAcquire rl)
    firstDrain `shouldEqual` true
    -- Immediately after, the bucket is empty.
    emptyBefore <- runC env (tryAcquire rl)
    emptyBefore `shouldEqual` false
    -- Advance virtual time by 250ms: at 4 permits/s the bucket
    -- has accrued exactly one fresh token.
    tc.advance (Milliseconds 250.0)
    refilled <- runC env (tryAcquire rl)
    refilled `shouldEqual` true

  it "available reflects the current bucket reading" do
    tc <- newTestClock
    let env = { clock: tc.clock }
    rl <- runC env $ make { permitsPerSecond: 1.0, burst: 5 }
    a0 <- runC env (available rl)
    a0 `shouldEqual` 5.0
    _ <- runC env (tryAcquire rl)
    _ <- runC env (tryAcquire rl)
    a1 <- runC env (available rl)
    a1 `shouldEqual` 3.0

  it "blocking acquire suspends until time advances enough" do
    tc <- newTestClock
    let env = { clock: tc.clock }
    rl <- runC env $ make { permitsPerSecond: 2.0, burst: 1 }
    -- Drain the bucket.
    _ <- runC env (tryAcquire rl)

    flag <- liftEffect (Ref.new false)
    let
      worker :: RIO (clock :: Clock) () Unit
      worker = do
        acquire rl
        liftEffect (Ref.write true flag)

    _ <- Aff.forkAff (runC env worker)
    -- Let the fork park.
    Aff.delay (Milliseconds 0.0)
    before <- liftEffect (Ref.read flag)
    before `shouldEqual` false

    -- 250ms isn't enough at 2 permits/s; need 500ms.
    tc.advance (Milliseconds 250.0)
    Aff.delay (Milliseconds 0.0)
    mid <- liftEffect (Ref.read flag)
    mid `shouldEqual` false

    tc.advance (Milliseconds 250.0)
    Aff.delay (Milliseconds 0.0)
    Aff.delay (Milliseconds 0.0)
    finalFlag <- liftEffect (Ref.read flag)
    finalFlag `shouldEqual` true

  it "withPermit gates a body on acquiring a token" do
    tc <- newTestClock
    let env = { clock: tc.clock }
    rl <- runC env $ make { permitsPerSecond: 1.0, burst: 2 }
    out <- runC env $ withPermit rl (pure 42)
    out `shouldEqual` 42
    -- One permit left after the first withPermit spent a token.
    avail <- runC env (available rl)
    avail `shouldEqual` 1.0
