module Test.RIO.Fiber.Config.RotatingSpec (spec) where

import Prelude

import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Fiber.Config.Rotating
  ( newRotating
  , readRotating
  , withRotation
  , writeRotating
  )
import RIO.Fiber.Core (Outcome(..), RIO)
import RIO.Fiber.Core as F

spec :: Spec Unit
spec = describe "rio-fiber: Config.Rotating" do

  it "readRotating returns the initial value" do
    let
      program :: RIO () () Int
      program = do
        cell <- F.liftEffect (newRotating 42)
        readRotating cell
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 42
      _ -> fail "expected Success"

  it "writeRotating updates what subsequent reads see" do
    let
      program :: RIO () () String
      program = do
        cell <- F.liftEffect (newRotating "v1")
        writeRotating cell "v2"
        readRotating cell
    out <- runAff program {}
    case out of
      Success s -> s `shouldEqual` "v2"
      _ -> fail "expected Success"

  it "withRotation runs the loader once and stores its result" do
    counter <- liftEffect (Ref.new 0)
    let
      loader :: RIO () () String
      loader = do
        n <- F.liftEffect (Ref.modify (_ + 1) counter)
        pure ("load-" <> show n)

      program :: RIO () () (Tuple String Int)
      program = do
        Tuple cell _ <- withRotation loader
        v <- readRotating cell
        n <- F.liftEffect (Ref.read counter)
        pure (Tuple v n)
    out <- runAff program {}
    case out of
      Success (Tuple value calls) -> do
        value `shouldEqual` "load-1"
        calls `shouldEqual` 1
      _ -> fail "expected Success"

  it "refresh re-runs the loader and overwrites the cell" do
    counter <- liftEffect (Ref.new 0)
    let
      loader :: RIO () () String
      loader = do
        n <- F.liftEffect (Ref.modify (_ + 1) counter)
        pure ("load-" <> show n)

      program :: RIO () () (Tuple String String)
      program = do
        Tuple cell refresh <- withRotation loader
        b <- readRotating cell
        refresh
        a <- readRotating cell
        pure (Tuple b a)
    out <- runAff program {}
    case out of
      Success (Tuple before after) -> do
        before `shouldEqual` "load-1"
        after `shouldEqual` "load-2"
      _ -> fail "expected Success"

  it "the same cell handle stays valid across rotations" do
    counter <- liftEffect (Ref.new 0)
    let
      loader :: RIO () () Int
      loader = F.liftEffect (Ref.modify (_ + 1) counter)

      program :: RIO () () (Array Int)
      program = do
        Tuple cell refresh <- withRotation loader
        v1 <- readRotating cell
        refresh
        v2 <- readRotating cell
        refresh
        v3 <- readRotating cell
        pure [ v1, v2, v3 ]
    out <- runAff program {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      _ -> fail "expected Success"

  it "refresh failure surfaces on the error row" do
    counter <- liftEffect (Ref.new 0)
    let
      boomTag :: Proxy "boom"
      boomTag = Proxy

      loader :: RIO () (boom :: Unit) Int
      loader = do
        n <- F.liftEffect (Ref.modify (_ + 1) counter)
        if n >= 2 then F.fail (Variant.inj boomTag unit)
        else pure n

      program :: RIO () (boom :: Unit) Unit
      program = do
        Tuple _ refresh <- withRotation loader
        refresh
    out <- runAff program {}
    case out of
      Fail _ -> pure unit
      _ -> fail "expected Fail"

  it "loader failure on refresh keeps the cell's last successful value" do
    counter <- liftEffect (Ref.new 0)
    let
      boomTag :: Proxy "boom"
      boomTag = Proxy

      loader :: RIO () (boom :: Unit) Int
      loader = do
        n <- F.liftEffect (Ref.modify (_ + 1) counter)
        if n >= 2 then F.fail (Variant.inj boomTag unit)
        else pure n

      -- Returns the value the cell holds after a failed refresh,
      -- by recovering the refresh failure on the error row and then
      -- reading the cell back out.
      program :: RIO () () Int
      program = do
        Tuple cell refresh <- F.catchAll
          ( \_ -> do
              -- Loader is expected to succeed on first call, so this
              -- branch should be unreachable. Allocate a dummy cell.
              c <- F.liftEffect (newRotating 0)
              pure (Tuple c (pure unit))
          )
          (withRotation loader)
        _ <- readRotating cell
        _ <- F.catchAll (\_ -> pure unit) refresh
        readRotating cell
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 1
      _ -> fail "expected Success"
