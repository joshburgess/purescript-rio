module Test.RIO.Aff.Cause.InspectionSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Exception (error, message)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Cause
  ( Cause(..)
  , containsFailure
  , defects
  , failures
  , find
  , hasDefect
  , hasFailure
  , isFailure
  )

type Errs = (notFound :: Int, parse :: String)

mkNotFound :: Int -> Variant Errs
mkNotFound = Variant.inj (Proxy :: Proxy "notFound")

mkParse :: String -> Variant Errs
mkParse = Variant.inj (Proxy :: Proxy "parse")

renderErr :: Variant Errs -> String
renderErr =
  Variant.case_
    # Variant.on (Proxy :: Proxy "notFound") (\n -> "notFound:" <> show n)
    # Variant.on (Proxy :: Proxy "parse") (\s -> "parse:" <> s)

spec :: Spec Unit
spec = describe "RIO.Aff.Cause (introspection)" do

  describe "failures" do
    it "extracts a single Fail leaf" do
      let c = Fail (mkNotFound 7) :: Cause Errs
      map renderErr (failures c) `shouldEqual` [ "notFound:7" ]

    it "returns [] for a defect-only cause" do
      let c = Die (error "boom") :: Cause Errs
      failures c `shouldEqual` ([] :: Array (Variant Errs))

    it "flattens a parallel tree depth-first, left first" do
      let
        c :: Cause Errs
        c = Parallel
          (Fail (mkNotFound 1))
          (Parallel (Fail (mkParse "x")) (Fail (mkNotFound 2)))
      map renderErr (failures c)
        `shouldEqual` [ "notFound:1", "parse:x", "notFound:2" ]

    it "skips defects in mixed trees" do
      let
        c :: Cause Errs
        c = Sequential
          (Die (error "bad"))
          (Fail (mkNotFound 9))
      map renderErr (failures c) `shouldEqual` [ "notFound:9" ]

  describe "defects" do
    it "extracts a single Die leaf" do
      let c = Die (error "boom") :: Cause Errs
      map message (defects c) `shouldEqual` [ "boom" ]

    it "returns [] for a Fail-only cause" do
      let c = Fail (mkNotFound 1) :: Cause Errs
      Array.length (defects c) `shouldEqual` 0

    it "flattens a sequential tree depth-first" do
      let
        c :: Cause Errs
        c = Sequential
          (Die (error "first"))
          (Sequential (Die (error "second")) (Fail (mkParse "p")))
      map message (defects c) `shouldEqual` [ "first", "second" ]

  describe "hasFailure / hasDefect / isFailure" do
    it "hasFailure detects a buried Fail" do
      let
        c :: Cause Errs
        c = Parallel (Die (error "x")) (Sequential (Die (error "y")) (Fail (mkParse "p")))
      hasFailure c `shouldEqual` true

    it "hasFailure returns false for defect-only" do
      let c = Sequential (Die (error "a")) (Die (error "b")) :: Cause Errs
      hasFailure c `shouldEqual` false

    it "hasDefect detects a buried Die" do
      let
        c :: Cause Errs
        c = Parallel (Fail (mkParse "x")) (Sequential (Fail (mkParse "y")) (Die (error "ouch")))
      hasDefect c `shouldEqual` true

    it "hasDefect returns false for failure-only" do
      let c = Parallel (Fail (mkNotFound 1)) (Fail (mkNotFound 2)) :: Cause Errs
      hasDefect c `shouldEqual` false

    it "isFailure is true exactly when typed failures exist and no defects do" do
      let
        pure_ = Parallel (Fail (mkNotFound 1)) (Fail (mkParse "p")) :: Cause Errs
        mixed = Parallel (Fail (mkParse "p")) (Die (error "x")) :: Cause Errs
        defectOnly = Die (error "x") :: Cause Errs
      isFailure pure_ `shouldEqual` true
      isFailure mixed `shouldEqual` false
      isFailure defectOnly `shouldEqual` false

  describe "containsFailure" do
    it "matches when the predicate hits any Fail leaf" do
      let
        c :: Cause Errs
        c = Parallel
          (Die (error "x"))
          (Sequential (Fail (mkNotFound 1)) (Fail (mkParse "ok")))
        isParseOk = Variant.default false
          # Variant.on (Proxy :: Proxy "parse") (\s -> s == "ok")
      containsFailure isParseOk c `shouldEqual` true

    it "returns false when no Fail leaf matches" do
      let
        c :: Cause Errs
        c = Sequential (Fail (mkNotFound 1)) (Fail (mkNotFound 2))
        isParse = Variant.default false
          # Variant.on (Proxy :: Proxy "parse") (\_ -> true)
      containsFailure isParse c `shouldEqual` false

  describe "find" do
    it "returns the first match in depth-first order" do
      let
        c :: Cause Errs
        c = Parallel
          (Sequential (Fail (mkNotFound 1)) (Fail (mkNotFound 2)))
          (Fail (mkParse "right-wins?"))
        firstNotFound =
          ( case _ of
              Fail v ->
                Variant.default Nothing
                  # Variant.on (Proxy :: Proxy "notFound") Just
                  $ v
              _ -> Nothing
          )
      find firstNotFound c `shouldEqual` Just 1

    it "returns Nothing when nothing matches" do
      let
        c :: Cause Errs
        c = Sequential (Die (error "a")) (Die (error "b"))
        firstFail = case _ of
          Fail _ -> Just unit
          _ -> Nothing
      find firstFail c `shouldEqual` Nothing
