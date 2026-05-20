module Test.RIO.Aff.Error.RefineSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Exception as Exception
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, fail, runRIO, runRIO', sandbox)
import RIO.Aff.Error (either, refineOrDie, refineOrDieWith) as Error

type Wide = (notFound :: Int, parse :: String)
type Narrow = (notFound :: Int)

keepNotFound :: Variant Wide -> Maybe (Variant Narrow)
keepNotFound = Variant.default Nothing
  # Variant.on (Proxy :: Proxy "notFound")
      (Just <<< Variant.inj (Proxy :: Proxy "notFound"))

renderNotFound :: Variant Narrow -> Int
renderNotFound =
  Variant.case_ # Variant.on (Proxy :: Proxy "notFound") identity

spec :: Spec Unit
spec = describe "RIO.Aff.Error (refineOrDie / refineOrDieWith)" do

  describe "refineOrDie" do
    it "passes a refined typed failure through unchanged" do
      let
        source :: RIO () Wide Int
        source = fail (Proxy :: Proxy "notFound") 42

        program :: RIO () Narrow Int
        program = Error.refineOrDie keepNotFound source
      result <- runRIO program
      case result of
        Left v -> renderNotFound v `shouldEqual` 42
        Right _ -> 1 `shouldEqual` 0

    it "defects a failure that doesn't match the classifier" do
      let
        source :: RIO () Wide Int
        source = fail (Proxy :: Proxy "parse") "bad"

        program :: RIO () () (Either _ (Either (Variant Narrow) Int))
        program = sandbox (Error.either (Error.refineOrDie keepNotFound source))
      result <- runRIO' program
      case result of
        Left err ->
          Exception.message err `shouldEqual`
            "RIO.refineOrDie: unrefined failure"
        Right _ -> 1 `shouldEqual` 0

    it "passes a success through untouched" do
      let
        source :: RIO () Wide Int
        source = pure 7

        program :: RIO () Narrow Int
        program = Error.refineOrDie keepNotFound source
      result <- runRIO program
      result `shouldEqual` (Right 7 :: Either _ _)

  describe "refineOrDieWith" do
    it "uses the supplied translator for the defected Error" do
      let
        source :: RIO () Wide Int
        source = fail (Proxy :: Proxy "parse") "tokens"

        translate v = Exception.error
          ( "unrefined: "
              <>
                ( Variant.case_
                    # Variant.on (Proxy :: Proxy "notFound") show
                    # Variant.on (Proxy :: Proxy "parse") identity
                    $ v
                )
          )

        program :: RIO () () (Either _ (Either (Variant Narrow) Int))
        program = sandbox
          ( Error.either
              (Error.refineOrDieWith keepNotFound translate source)
          )
      result <- runRIO' program
      case result of
        Left err ->
          Exception.message err `shouldEqual` "unrefined: tokens"
        Right _ -> 1 `shouldEqual` 0
