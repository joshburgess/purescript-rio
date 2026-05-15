module Test.RIO.Cause.FoldLinearizeSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Exception (Error, error, message)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Cause (Cause(..), foldCause, linearize)

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
spec = describe "RIO.Cause (foldCause / linearize)" do

  describe "foldCause" do
    it "calls onFail for a typed-failure leaf" do
      let
        c = Fail (mkNotFound 42) :: Cause Errs
        result = foldCause renderErr (\_ -> "die") (\_ _ -> "par") (\_ _ -> "seq") c
      result `shouldEqual` "notFound:42"

    it "calls onDie for a defect leaf" do
      let
        c = Die (error "boom") :: Cause Errs
        result = foldCause (\_ -> "fail") message (\_ _ -> "par") (\_ _ -> "seq") c
      result `shouldEqual` "boom"

    it "combines parallel branches with onPar" do
      let
        c :: Cause Errs
        c = Parallel (Fail (mkNotFound 1)) (Fail (mkParse "x"))

        result = foldCause
          renderErr
          (\_ -> "die")
          (\a b -> "par(" <> a <> "," <> b <> ")")
          (\a b -> "seq(" <> a <> "," <> b <> ")")
          c
      result `shouldEqual` "par(notFound:1,parse:x)"

    it "combines sequential branches with onSeq" do
      let
        c :: Cause Errs
        c = Sequential (Fail (mkParse "p")) (Die (error "after"))

        result = foldCause
          renderErr
          message
          (\a b -> "par(" <> a <> "," <> b <> ")")
          (\a b -> "seq(" <> a <> "," <> b <> ")")
          c
      result `shouldEqual` "seq(parse:p,after)"

    it "is bottom-up: branches are folded before the combiner runs" do
      let
        c :: Cause Errs
        c = Parallel
          (Sequential (Fail (mkNotFound 1)) (Fail (mkNotFound 2)))
          (Fail (mkParse "x"))

        result = foldCause
          renderErr
          (\_ -> "die")
          (\a b -> "[" <> a <> "|" <> b <> "]")
          (\a b -> "{" <> a <> ";" <> b <> "}")
          c
      result `shouldEqual` "[{notFound:1;notFound:2}|parse:x]"

    it "is equivalent to failures for a typed-only accumulator" do
      let
        c :: Cause Errs
        c = Parallel
          (Fail (mkNotFound 1))
          (Sequential (Fail (mkParse "x")) (Fail (mkNotFound 2)))

        viaFold = foldCause (\v -> [ v ]) (\_ -> []) (<>) (<>) c
      map renderErr viaFold
        `shouldEqual` [ "notFound:1", "parse:x", "notFound:2" ]

  describe "linearize" do
    let
      render :: Either Error (Variant Errs) -> String
      render = case _ of
        Right v -> "F:" <> renderErr v
        Left err -> "D:" <> message err

    it "flattens a single Fail to a single Right leaf" do
      let c = Fail (mkNotFound 7) :: Cause Errs
      map render (linearize c) `shouldEqual` [ "F:notFound:7" ]

    it "flattens a single Die to a single Left leaf" do
      let c = Die (error "kaboom") :: Cause Errs
      map render (linearize c) `shouldEqual` [ "D:kaboom" ]

    it "discards structure: Parallel and Sequential flatten the same" do
      let
        par :: Cause Errs
        par = Parallel (Fail (mkNotFound 1)) (Fail (mkParse "x"))

        sequ :: Cause Errs
        sequ = Sequential (Fail (mkNotFound 1)) (Fail (mkParse "x"))
      map render (linearize par)
        `shouldEqual` [ "F:notFound:1", "F:parse:x" ]
      map render (linearize sequ)
        `shouldEqual` [ "F:notFound:1", "F:parse:x" ]

    it "preserves left-to-right order across nested composites" do
      let
        c :: Cause Errs
        c = Parallel
          (Sequential (Fail (mkNotFound 1)) (Fail (mkParse "x")))
          (Parallel (Fail (mkNotFound 2)) (Fail (mkParse "y")))
      map render (linearize c)
        `shouldEqual`
          [ "F:notFound:1"
          , "F:parse:x"
          , "F:notFound:2"
          , "F:parse:y"
          ]

    it "interleaves defects and failures in tree order" do
      let
        c :: Cause Errs
        c = Sequential
          (Fail (mkNotFound 1))
          (Parallel (Die (error "d1")) (Fail (mkParse "x")))
      map render (linearize c)
        `shouldEqual` [ "F:notFound:1", "D:d1", "F:parse:x" ]
