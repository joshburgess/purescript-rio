module Test.RIO.Aff.LatchSpec (spec) where

import Prelude hiding (join)

import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, fork, join, runRIO')
import RIO.Aff.Latch as Latch

spec :: Spec Unit
spec = describe "RIO.Aff.Latch" do
  it "await returns immediately when the latch starts at zero" do
    let
      program :: RIO () () Unit
      program = do
        l <- liftEffect (Latch.make 0)
        Latch.await l
    runRIO' program

  it "await suspends until count reaches zero" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: String -> RIO () () Unit
      record m = liftEffect (Ref.modify_ (\xs -> xs <> [ m ]) log)

      program :: RIO () () Unit
      program = do
        l <- liftEffect (Latch.make 3)
        waiter <- fork do
          Latch.await l
          record "released"
        liftAff (delay (Milliseconds 5.0))
        record "before-1"
        Latch.countDown l
        record "before-2"
        Latch.countDown l
        record "before-3"
        Latch.countDown l
        _ <- join waiter
        pure unit
    _ <- runRIO' program
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual`
      [ "before-1", "before-2", "before-3", "released" ]

  it "count reports remaining and isOpen flips at zero" do
    let
      program
        :: RIO ()
             ()
             { c0 :: Int
             , open0 :: Boolean
             , c1 :: Int
             , c2 :: Int
             , open2 :: Boolean
             }
      program = do
        l <- liftEffect (Latch.make 2)
        c0 <- Latch.count l
        open0 <- Latch.isOpen l
        Latch.countDown l
        c1 <- Latch.count l
        Latch.countDown l
        c2 <- Latch.count l
        open2 <- Latch.isOpen l
        pure { c0, open0, c1, c2, open2 }
    result <- runRIO' program
    result `shouldEqual`
      { c0: 2, open0: false, c1: 1, c2: 0, open2: true }

  it "countDown past zero is a no-op" do
    let
      program :: RIO () () Int
      program = do
        l <- liftEffect (Latch.make 1)
        Latch.countDown l
        Latch.countDown l
        Latch.countDown l
        Latch.count l
    result <- runRIO' program
    result `shouldEqual` 0

  it "many concurrent waiters all fire on the final countDown" do
    fires <- liftEffect (Ref.new 0)
    let
      waiter :: Latch.Latch -> RIO () () Unit
      waiter l = do
        Latch.await l
        liftEffect (Ref.modify_ (_ + 1) fires)

      program :: RIO () () Unit
      program = do
        l <- liftEffect (Latch.make 1)
        f1 <- fork (waiter l)
        f2 <- fork (waiter l)
        f3 <- fork (waiter l)
        liftAff (delay (Milliseconds 5.0))
        Latch.countDown l
        _ <- join f1
        _ <- join f2
        _ <- join f3
        pure unit
    _ <- runRIO' program
    n <- liftEffect (Ref.read fires)
    n `shouldEqual` 3
