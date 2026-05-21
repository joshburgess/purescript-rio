module Test.RIO.Fiber.MemoSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Fiber.Core (Outcome(..), RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Memo (memoize)

spec :: Spec Unit
spec = describe "rio-fiber: Memo" do

  it "runs the underlying action exactly once across repeated calls" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () (Array Int)
      program = do
        getOnce <- memoize
          ( do
              _ <- F.liftEffect (Ref.modify_ (_ + 1) counter)
              pure 42
          )
        a <- getOnce
        b <- getOnce
        c <- getOnce
        pure [ a, b, c ]
    out <- runAff program {}
    case out of
      Success xs -> xs `shouldEqual` [ 42, 42, 42 ]
      _ -> fail "expected Success"
    runs <- liftEffect (Ref.read counter)
    runs `shouldEqual` 1

  it "single-flights concurrent first calls (only one underlying run)" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () (Array Int)
      program = do
        getOnce <- memoize
          ( do
              _ <- F.liftEffect (Ref.modify_ (_ + 1) counter)
              -- a real async boundary so the racing callers all
              -- observe the in-flight Deferred
              F.sleep (Milliseconds 10.0)
              pure 7
          )
        F.parTraverse (\_ -> getOnce) [ unit, unit, unit, unit ]
    out <- runAff program {}
    case out of
      Success xs -> xs `shouldEqual` [ 7, 7, 7, 7 ]
      _ -> fail "expected Success"
    runs <- liftEffect (Ref.read counter)
    runs `shouldEqual` 1

  it "caches a typed failure: subsequent calls see the same failure" do
    counter <- liftEffect (Ref.new 0)
    let
      boom :: Proxy "boom"
      boom = Proxy

      program :: RIO () (boom :: Int) Int
      program = do
        getOnce <- memoize
          ( do
              _ <- F.liftEffect (Ref.modify_ (_ + 1) counter)
              F.fail (Variant.inj boom 99)
          )
        _ <- getOnce
        getOnce
    out <- runAff program {}
    case out of
      Fail v ->
        (Variant.case_ # Variant.on boom identity) v `shouldEqual` 99
      _ -> fail "expected typed Fail"
    runs <- liftEffect (Ref.read counter)
    runs `shouldEqual` 1

  it "independent memo cells from the same template run independently" do
    counter <- liftEffect (Ref.new 0)
    let
      template :: RIO () () Int
      template = do
        _ <- F.liftEffect (Ref.modify_ (_ + 1) counter)
        pure 1

      program :: RIO () () Int
      program = do
        cellA <- memoize template
        cellB <- memoize template
        _ <- cellA
        _ <- cellA
        _ <- cellB
        _ <- cellB
        pure 0
    _ <- runAff program {}
    runs <- liftEffect (Ref.read counter)
    runs `shouldEqual` 2
