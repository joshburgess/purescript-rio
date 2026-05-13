module Test.RIO.STM.TSemaphoreSpec (spec) where

import Prelude

import Data.Array (range)
import Data.Traversable (traverse)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, fork, join, runRIO')
import RIO.STM (atomically)
import RIO.STM.TSemaphore
  ( TSemaphore
  , availableTSemaphore
  , newTSemaphore
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
