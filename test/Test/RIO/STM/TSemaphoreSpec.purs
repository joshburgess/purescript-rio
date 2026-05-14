module Test.RIO.STM.TSemaphoreSpec (spec) where

import Prelude

import Data.Array (range)
import Data.Traversable (traverse)
import Effect.Aff (Milliseconds(..), attempt, delay, error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, die, fail, fork, join, runRIO, runRIO')
import RIO.STM (atomically)
import RIO.STM.TSemaphore
  ( TSemaphore
  , acquireN
  , availableTSemaphore
  , newTSemaphore
  , releaseN
  , releaseTSemaphore
  , withTSemaphore
  )

spec :: Spec Unit
spec = describe "RIO.STM.TSemaphore" do
  it "acquire-then-release leaves the count unchanged" do
    let
      program :: RIO () () Int
      program = do
        sem <- atomically (newTSemaphore 3)
        withTSemaphore sem (pure unit)
        atomically (availableTSemaphore sem)
    result <- runRIO' program
    result `shouldEqual` 3

  it "acquire blocks until a release happens" do
    events <- liftEffect (Ref.new [])
    let
      push :: forall r e. String -> RIO r e Unit
      push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

      program :: RIO () () Unit
      program = do
        sem <- atomically (newTSemaphore 0)
        push "before-fork"
        waiter <- fork do
          withTSemaphore sem (push "inside-permit")
          push "after-permit"
        liftAff (delay (Milliseconds 20.0))
        push "before-release"
        atomically (releaseTSemaphore sem)
        join waiter
    runRIO' program
    order <- liftEffect (Ref.read events)
    order `shouldEqual`
      [ "before-fork"
      , "before-release"
      , "inside-permit"
      , "after-permit"
      ]

  it "withTSemaphore bounds maximum observed concurrency" do
    inFlightRef <- liftEffect (Ref.new 0)
    peakRef <- liftEffect (Ref.new 0)
    let
      enter :: forall r e. RIO r e Unit
      enter = liftEffect do
        n <- Ref.modify (_ + 1) inFlightRef
        peak <- Ref.read peakRef
        Ref.write (max peak n) peakRef

      leave :: forall r e. RIO r e Unit
      leave = liftEffect (Ref.modify_ (_ - 1) inFlightRef)

      worker :: forall r e. TSemaphore -> RIO r e Unit
      worker sem = withTSemaphore sem do
        enter
        liftAff (delay (Milliseconds 10.0))
        leave

      n = 12

      program :: RIO () () Int
      program = do
        sem <- atomically (newTSemaphore 3)
        fibers <- traverse (\_ -> fork (worker sem)) (range 1 n)
        _ <- traverse join fibers
        liftEffect (Ref.read peakRef)
    peak <- runRIO' program
    (peak <= 3) `shouldEqual` true
    (peak >= 1) `shouldEqual` true

  it "acquireN / releaseN deduct and restore the full count atomically" do
    let
      program
        :: RIO () () { afterAcquire :: Int, afterRelease :: Int }
      program = do
        sem <- atomically (newTSemaphore 5)
        atomically (acquireN 3 sem)
        afterAcquire <- atomically (availableTSemaphore sem)
        atomically (releaseN 3 sem)
        afterRelease <- atomically (availableTSemaphore sem)
        pure { afterAcquire, afterRelease }
    result <- runRIO' program
    result `shouldEqual` { afterAcquire: 2, afterRelease: 5 }

  it "availableTSemaphore reflects the initial count and each step" do
    let
      program
        :: RIO ()
             ()
             { fresh :: Int, afterAcquire :: Int, afterExtraRelease :: Int }
      program = do
        sem <- atomically (newTSemaphore 1)
        fresh <- atomically (availableTSemaphore sem)
        atomically (acquireN 1 sem)
        afterAcquire <- atomically (availableTSemaphore sem)
        atomically (releaseTSemaphore sem)
        atomically (releaseTSemaphore sem)
        afterExtraRelease <- atomically (availableTSemaphore sem)
        pure { fresh, afterAcquire, afterExtraRelease }
    result <- runRIO' program
    result `shouldEqual`
      { fresh: 1, afterAcquire: 0, afterExtraRelease: 2 }

  it "withTSemaphore releases the permit after a typed failure" do
    sem <- runRIO' (atomically (newTSemaphore 1) :: RIO () () TSemaphore)
    let
      program :: RIO () (boom :: Unit) Unit
      program = withTSemaphore sem (fail (Proxy :: Proxy "boom") unit)
    _ <- runRIO program
    a <- runRIO' (atomically (availableTSemaphore sem) :: RIO () () Int)
    a `shouldEqual` 1

  it "withTSemaphore releases the permit after a defect" do
    sem <- runRIO' (atomically (newTSemaphore 1) :: RIO () () TSemaphore)
    let
      program :: RIO () () Unit
      program = withTSemaphore sem (die (error "boom"))
    _ <- attempt (runRIO' program)
    a <- runRIO' (atomically (availableTSemaphore sem) :: RIO () () Int)
    a `shouldEqual` 1

  it "withTSemaphore releases the permit after a fiber kill" do
    -- The docstring promises the permit is released on every
    -- termination path: "success, typed failure, defect, kill".
    -- Success, typed failure, and defect are pinned above; this
    -- test pins the kill case so the full bracket contract is
    -- documented.
    sem <- runRIO' (atomically (newTSemaphore 1) :: RIO () () TSemaphore)
    let
      program :: RIO () () Unit
      program = withTSemaphore sem (liftAff (delay (Milliseconds 50.0)))
    f <- forkAff (runRIO' program)
    liftAff (delay (Milliseconds 5.0))
    killFiber (error "test-cancel") f
    liftAff (delay (Milliseconds 10.0))
    a <- runRIO' (atomically (availableTSemaphore sem) :: RIO () () Int)
    a `shouldEqual` 1
