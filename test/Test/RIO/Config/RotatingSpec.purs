module Test.RIO.Config.RotatingSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Config.Rotating
  ( newRotating
  , readRotating
  , withRotation
  , writeRotating
  )
import RIO.Core (RIO, fail, runRIO, runRIO')

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

  it "refresh failure surfaces on the error row" do
    -- Docstring promise: "the failure propagates from `refresh` on the
    -- chosen error row."
    counter <- liftEffect (Ref.new 0)
    let
      loader :: RIO () (boom :: Unit) Int
      loader = do
        n <- liftEffect (Ref.modify (_ + 1) counter)
        if n >= 2 then fail (Proxy :: Proxy "boom") unit
        else pure n

      program :: RIO () (boom :: Unit) Unit
      program = do
        Tuple _ refresh <- withRotation loader
        refresh
    result <- runRIO program
    case result of
      Left _ -> pure unit
      Right _ -> 1 `shouldEqual` 0

  it "loader failure on refresh keeps the cell's last successful value" do
    -- Docstring promise: "the cell keeps the last successful value".
    -- The first loader call (which populates the cell) succeeds at
    -- counter = 1; refresh trips counter = 2 which is where the loader
    -- fails. We assert that after the failing refresh, the cell still
    -- reads back the initial value.
    counter <- liftEffect (Ref.new 0)
    let
      loader :: RIO () (boom :: Unit) Int
      loader = do
        n <- liftEffect (Ref.modify (_ + 1) counter)
        if n >= 2 then fail (Proxy :: Proxy "boom") unit
        else pure n

    -- Step 1: build the rotating cell. Loader runs once and succeeds.
    setupResult <- runRIO (withRotation loader)
    case setupResult of
      Left _ -> 1 `shouldEqual` 0
      Right (Tuple cell refresh) -> do
        initial <- runRIO' (readRotating cell)
        initial `shouldEqual` 1

        -- Step 2: call refresh; loader fails this time.
        refreshOutcome <- runRIO refresh
        case refreshOutcome of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

        -- Step 3: cell still reads back the last successful value.
        after <- runRIO' (readRotating cell)
        after `shouldEqual` 1
