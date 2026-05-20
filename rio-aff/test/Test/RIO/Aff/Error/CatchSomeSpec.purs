module Test.RIO.Aff.Error.CatchSomeSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, fail, runRIO, runRIO')
import RIO.Aff.Error (catchSome, orElseFail, orElseSucceed)

type Errs = (notFound :: Int, parse :: String)

renderErrs :: Variant Errs -> String
renderErrs =
  Variant.case_
    # Variant.on (Proxy :: Proxy "notFound") (\n -> "notFound:" <> show n)
    # Variant.on (Proxy :: Proxy "parse") (\s -> "parse:" <> s)

spec :: Spec Unit
spec = describe "RIO.Aff.Error (catchSome / orElseSucceed / orElseFail)" do

  describe "catchSome" do
    it "handles a matching failure and discharges it" do
      let
        handleNotFound :: Variant Errs -> Maybe (RIO () Errs Int)
        handleNotFound = Variant.default Nothing
          # Variant.on (Proxy :: Proxy "notFound") (\_ -> Just (pure 0))

        program :: RIO () Errs Int
        program = catchSome handleNotFound
          (fail (Proxy :: Proxy "notFound") 7)
      result <- runRIO program
      result `shouldEqual` (Right 0 :: Either _ _)

    it "lets a non-matching failure propagate on the same row" do
      let
        handleNotFound :: Variant Errs -> Maybe (RIO () Errs Int)
        handleNotFound = Variant.default Nothing
          # Variant.on (Proxy :: Proxy "notFound") (\_ -> Just (pure 0))

        program :: RIO () Errs Int
        program = catchSome handleNotFound
          (fail (Proxy :: Proxy "parse") "bad")
      result <- runRIO program
      case result of
        Left v -> renderErrs v `shouldEqual` "parse:bad"
        Right _ -> 1 `shouldEqual` 0

    it "passes a success through untouched" do
      let
        handleAll :: Variant Errs -> Maybe (RIO () Errs Int)
        handleAll _ = Just (pure 0)

        program :: RIO () Errs Int
        program = catchSome handleAll (pure 9)
      result <- runRIO program
      result `shouldEqual` (Right 9 :: Either _ _)

  describe "orElseSucceed" do
    it "replaces any typed failure with the supplied value" do
      let
        program :: RIO () () Int
        program = orElseSucceed 42
          (fail (Proxy :: Proxy "notFound") 0 :: RIO () Errs Int)
      result <- runRIO' program
      result `shouldEqual` 42

    it "passes a success through unchanged" do
      let
        program :: RIO () () Int
        program = orElseSucceed 42
          (pure 5 :: RIO () Errs Int)
      result <- runRIO' program
      result `shouldEqual` 5

  describe "orElseFail" do
    it "replaces any typed failure with the supplied Variant" do
      let
        program :: RIO () (mapped :: Unit) Int
        program = orElseFail
          (Variant.inj (Proxy :: Proxy "mapped") unit)
          (fail (Proxy :: Proxy "notFound") 0 :: RIO () Errs Int)
      result <- runRIO program
      case result of
        Left v ->
          ( Variant.case_
              # Variant.on (Proxy :: Proxy "mapped") (\_ -> "ok")
              $ v
          ) `shouldEqual` "ok"
        Right _ -> 1 `shouldEqual` 0

    it "passes a success through unchanged" do
      let
        program :: RIO () (mapped :: Unit) Int
        program = orElseFail
          (Variant.inj (Proxy :: Proxy "mapped") unit)
          (pure 5 :: RIO () Errs Int)
      result <- runRIO program
      result `shouldEqual` (Right 5 :: Either _ _)
