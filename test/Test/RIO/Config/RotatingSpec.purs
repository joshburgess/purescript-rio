module Test.RIO.Config.RotatingSpec (spec) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Config.Rotating
  ( newRotating
  , readRotating
  , withRotation
  , writeRotating
  )
import RIO.Core (runRIO')

spec :: Spec Unit
spec = describe "RIO.Config.Rotating" do
  it "readRotating returns the initial value" do
    r <- runRIO' do
      cell <- liftEffect (newRotating 42)
      readRotating cell
    r `shouldEqual` 42

  it "writeRotating updates what subsequent reads see" do
    r <- runRIO' do
      cell <- liftEffect (newRotating "v1")
      writeRotating cell "v2"
      readRotating cell
    r `shouldEqual` "v2"

  it "withRotation runs the loader once and stores its result" do
    counter <- liftEffect (Ref.new 0)
    let
      loader = do
        n <- liftEffect (Ref.modify (_ + 1) counter)
        pure ("load-" <> show n)
    Tuple value calls <- runRIO' do
      Tuple cell _ <- withRotation loader
      v <- readRotating cell
      n <- liftEffect (Ref.read counter)
      pure (Tuple v n)
    value `shouldEqual` "load-1"
    calls `shouldEqual` 1

  it "refresh re-runs the loader and overwrites the cell" do
    counter <- liftEffect (Ref.new 0)
    let
      loader = do
        n <- liftEffect (Ref.modify (_ + 1) counter)
        pure ("load-" <> show n)
    Tuple before after <- runRIO' do
      Tuple cell refresh <- withRotation loader
      b <- readRotating cell
      refresh
      a <- readRotating cell
      pure (Tuple b a)
    before `shouldEqual` "load-1"
    after `shouldEqual` "load-2"

  it "the same cell handle stays valid across rotations" do
    counter <- liftEffect (Ref.new 0)
    let
      loader = liftEffect (Ref.modify (_ + 1) counter)
    values <- runRIO' do
      Tuple cell refresh <- withRotation loader
      v1 <- readRotating cell
      refresh
      v2 <- readRotating cell
      refresh
      v3 <- readRotating cell
      pure [ v1, v2, v3 ]
    values `shouldEqual` [ 1, 2, 3 ]
