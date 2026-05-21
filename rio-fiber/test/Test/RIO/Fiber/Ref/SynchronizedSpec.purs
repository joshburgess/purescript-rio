module Test.RIO.Fiber.Ref.SynchronizedSpec (spec) where

import Prelude

import Data.Array (sort) as Array
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as ERef
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Fiber.Core (Outcome(..), RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Ref.Synchronized as SRef

spec :: Spec Unit
spec = describe "rio-fiber: Ref.Synchronized" do

  it "new + read returns the initial value" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 7
        SRef.read ref
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 7
      _ -> fail "expected Success"

  it "write overwrites and subsequent read sees the new value" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 0
        SRef.write ref 42
        SRef.read ref
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 42
      _ -> fail "expected Success"

  it "modify applies a pure function under the lock and returns the new value" do
    let
      program :: RIO () () { returned :: Int, observed :: Int }
      program = do
        ref <- SRef.new 10
        returned <- SRef.modify ref (_ + 5)
        observed <- SRef.read ref
        pure { returned, observed }
    out <- runAff program {}
    case out of
      Success r -> do
        r.returned `shouldEqual` 15
        r.observed `shouldEqual` 15
      _ -> fail "expected Success"

  it "modifyM threads an effectful update body" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 10
        _ <- SRef.modifyM ref (\n -> pure (n * 2))
        SRef.read ref
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 20
      _ -> fail "expected Success"

  it "concurrent modifyM bodies are serialised (no torn read-write)" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 0
        _ <- F.parTraverse
          ( \_ -> SRef.modifyM ref
              ( \n -> do
                  F.sleep (Milliseconds 10.0)
                  pure (n + 1)
              )
          )
          [ unit, unit, unit ]
        SRef.read ref
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 3
      _ -> fail "expected Success"

  it "modifyM_ discards the new value and updates the cell" do
    let
      program :: RIO () () Int
      program = do
        ref <- SRef.new 1
        SRef.modifyM_ ref (\n -> pure (n * 100))
        SRef.read ref
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 100
      _ -> fail "expected Success"

  it "newEffect produces a SynchronizedRef usable from RIO" do
    ref <- liftEffect (SRef.newEffect 0)
    let
      program :: RIO () () Int
      program = do
        SRef.write ref 5
        SRef.read ref
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 5
      _ -> fail "expected Success"

  it "concurrent modifyM observes each predecessor's write (history check)" do
    log <- liftEffect (ERef.new ([] :: Array Int))
    let
      program :: RIO () () (Array Int)
      program = do
        ref <- SRef.new (0 :: Int)
        _ <- F.parTraverse
          ( \i -> SRef.modifyM ref
              ( \n -> do
                  F.liftEffect (ERef.modify_ (\xs -> xs <> [ i ]) log)
                  F.sleep (Milliseconds 5.0)
                  pure (n + 1)
              )
          )
          [ 1, 2, 3, 4 ]
        F.liftEffect (ERef.read log)
    out <- runAff program {}
    case out of
      Success xs -> Array.sort xs `shouldEqual` [ 1, 2, 3, 4 ]
      _ -> fail "expected Success"
