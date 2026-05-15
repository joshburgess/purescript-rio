module Test.RIO.ValidationSpec (spec) where

import Prelude

import Data.Array.NonEmpty as NEArray
import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, runRIO)
import RIO.Validation
  ( Validation(..)
  , collectAll
  , failure
  , fromEither
  , success
  , toEither
  , toRIO
  )

type Errs = (badName :: String, badAge :: String)

readName :: Variant Errs -> String
readName =
  Variant.case_
    # Variant.on (Proxy :: Proxy "badName") identity
    # Variant.on (Proxy :: Proxy "badAge") identity

badName :: forall a. String -> Validation Errs a
badName msg = failure (Variant.inj (Proxy :: Proxy "badName") msg)

badAge :: forall a. String -> Validation Errs a
badAge msg = failure (Variant.inj (Proxy :: Proxy "badAge") msg)

spec :: Spec Unit
spec = describe "RIO.Validation" do

  describe "constructors and conversions" do
    it "success wraps a value" do
      case success 42 :: Validation Errs Int of
        Success a -> a `shouldEqual` 42
        Failure _ -> 0 `shouldEqual` 1

    it "failure wraps a single variant" do
      case badName "empty" :: Validation Errs Unit of
        Failure es -> readName (NEArray.head es) `shouldEqual` "empty"
        Success _ -> 0 `shouldEqual` 1

    it "fromEither Right is Success" do
      case fromEither (Right 9) :: Validation Errs Int of
        Success a -> a `shouldEqual` 9
        Failure _ -> 0 `shouldEqual` 1

    it "fromEither Left is single-error Failure" do
      let v = Variant.inj (Proxy :: Proxy "badAge") "neg" :: Variant Errs
      case fromEither (Left v) :: Validation Errs Int of
        Failure es ->
          NEArray.length es `shouldEqual` 1
        Success _ -> 0 `shouldEqual` 1

    it "toEither Success is Right" do
      case toEither (success 5 :: Validation Errs Int) of
        Right a -> a `shouldEqual` 5
        Left _ -> 0 `shouldEqual` 1

  describe "applicative accumulation" do
    it "Apply concatenates failures from both sides" do
      let
        v :: Validation Errs Int
        v = (\a b -> a + b) <$> badName "n" <*> badAge "a"
      case v of
        Failure es -> NEArray.length es `shouldEqual` 2
        Success _ -> 0 `shouldEqual` 1

    it "single-side failure is preserved" do
      let
        v :: Validation Errs Int
        v = (\a b -> a + b) <$> success 1 <*> badAge "a"
      case v of
        Failure es ->
          readName (NEArray.head es) `shouldEqual` "a"
        Success _ -> 0 `shouldEqual` 1

    it "all-success applies the function" do
      let
        v :: Validation Errs Int
        v = (\a b -> a + b) <$> success 1 <*> success 2
      case v of
        Success n -> n `shouldEqual` 3
        Failure _ -> 0 `shouldEqual` 1

    it "preserves accumulation order across <*>" do
      let
        v :: Validation Errs Int
        v = (\a b c -> a + b + c)
          <$> badName "first"
          <*> badAge "second"
          <*> badName "third"
      case v of
        Failure es -> do
          NEArray.length es `shouldEqual` 3
          map readName (NEArray.toArray es)
            `shouldEqual` [ "first", "second", "third" ]
        Success _ -> 0 `shouldEqual` 1

  describe "collectAll" do
    it "returns the array of values when every entry succeeds" do
      let
        vs :: Array (Validation Errs Int)
        vs = [ success 1, success 2, success 3 ]
      case collectAll vs of
        Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
        Failure _ -> 0 `shouldEqual` 1

    it "accumulates every failure across the array" do
      let
        vs :: Array (Validation Errs Int)
        vs =
          [ success 1
          , badName "a"
          , badAge "b"
          , success 2
          , badName "c"
          ]
      case collectAll vs of
        Failure es ->
          map readName (NEArray.toArray es)
            `shouldEqual` [ "a", "b", "c" ]
        Success _ -> 0 `shouldEqual` 1

  describe "toRIO" do
    it "passes Success through" do
      let
        program :: RIO () Errs Int
        program = toRIO (success 100 :: Validation Errs Int)
      result <- runRIO program
      case result of
        Right a -> a `shouldEqual` 100
        Left _ -> 0 `shouldEqual` 1

    it "rethrows the first accumulated failure on Failure" do
      let
        program :: RIO () Errs Int
        program = toRIO
          ( (\a b -> a + b) <$> badName "n1" <*> badAge "a1"
              :: Validation Errs Int
          )
      result <- runRIO program
      case result of
        Left v -> readName v `shouldEqual` "n1"
        Right _ -> 0 `shouldEqual` 1
