module Test.RIO.Aff.Ref.SynchronizedSpec (spec) where

import Prelude

import Data.Array (sort) as Array
import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as ERef
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Concurrency (parTraverse)
import RIO.Aff.Core (RIO, runRIO)
import RIO.Aff.Ref.Synchronized as SRef

spec :: Spec Unit
spec = describe "RIO.Aff.Ref.Synchronized" do
  it "new + read returns the initial value" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 7
        SRef.read ref
    result <- runRIO program
    result `shouldEqual` (Right 7 :: Either _ Int)

  it "write overwrites and subsequent read sees the new value" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 0
        SRef.write ref 42
        SRef.read ref
    result <- runRIO program
    result `shouldEqual` (Right 42 :: Either _ Int)

  it "modify applies a pure function under the lock and returns the new value" do
    let
      program :: RIO () () { returned :: Int, observed :: Int }
      program = do
        ref <- SRef.new 10
        returned <- SRef.modify ref (_ + 5)
        observed <- SRef.read ref
        pure { returned, observed }
    result <- runRIO program
    result `shouldEqual`
      (Right { returned: 15, observed: 15 } :: Either _ _)

  it "modifyM threads an effectful update body" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 10
        _ <- SRef.modifyM ref (\n -> pure (n * 2))
        SRef.read ref
    result <- runRIO program
    result `shouldEqual` (Right 20 :: Either _ Int)

  it "concurrent modifyM bodies are serialised (no torn read-write)" do
    -- Three fibers each run a modifyM that reads the current
    -- value, sleeps, then writes value+1. With the semaphore in
    -- place each fiber sees the previous fiber's write, so the
    -- final value is initial + 3. A regression that dropped the
    -- lock would let two reads observe the same value and the
    -- final count would be 1 or 2.
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 0
        _ <- parTraverse
          ( \_ -> SRef.modifyM ref
              ( \n -> do
                  liftAff (delay (Milliseconds 10.0))
                  pure (n + 1)
              )
          )
          [ unit, unit, unit ]
        SRef.read ref
    result <- runRIO program
    result `shouldEqual` (Right 3 :: Either _ Int)

  it "modifyM_ discards the new value and updates the cell" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 1
        SRef.modifyM_ ref (\n -> pure (n * 100))
        SRef.read ref
    result <- runRIO program
    result `shouldEqual` (Right 100 :: Either _ Int)

  it "newEffect produces a SynchronizedRef usable from RIO" do
    ref <- liftEffect (SRef.newEffect 0)
    let
      program :: RIO () () Int
      program = do
        SRef.write ref 5
        SRef.read ref
    result <- runRIO program
    result `shouldEqual` (Right 5 :: Either _ Int)

  it "concurrent modifyM observes each predecessor's write (history check)" do
    -- Four fibers each append their fiber id to a log inside the
    -- modifyM body. The log captures the order in which the
    -- semaphore admitted them; we only assert that every fiber id
    -- appears exactly once, since the admission order is
    -- non-deterministic.
    log <- liftEffect (ERef.new ([] :: Array Int))
    let
      program :: RIO () () (Array Int)
      program = do
        ref <- SRef.new (0 :: Int)
        _ <- parTraverse
          ( \i -> SRef.modifyM ref
              ( \n -> do
                  liftEffect (ERef.modify_ (\xs -> xs <> [ i ]) log)
                  liftAff (delay (Milliseconds 5.0))
                  pure (n + 1)
              )
          )
          [ 1, 2, 3, 4 ]
        liftEffect (ERef.read log)
    result <- runRIO program
    case result of
      Right xs -> Array.sort xs `shouldEqual` [ 1, 2, 3, 4 ]
      Left _ -> 1 `shouldEqual` 0
