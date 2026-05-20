module Test.RIO.Aff.ExitSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Exception (error, message)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Cause (Cause(..))
import RIO.Aff.Core (RIO)
import RIO.Aff.Error (die, fail)
import RIO.Aff.Exit
  ( Exit(..)
  , foldExit
  , fromEither
  , isFailure
  , isSuccess
  , map_
  , runRIOExit
  , succeed
  , toEither
  )
import RIO.Aff.Exit (fail) as Exit
import RIO.Aff.Exit (die) as Exit

type Errs = (boom :: String)

readBoom :: Variant Errs -> String
readBoom = Variant.case_ # Variant.on (Proxy :: Proxy "boom") identity

spec :: Spec Unit
spec = describe "RIO.Aff.Exit" do

  describe "constructors and predicates" do
    it "succeed builds a Success" do
      let e = succeed 42 :: Exit () Int
      isSuccess e `shouldEqual` true
      isFailure e `shouldEqual` false

    it "Exit.fail wraps a Variant in Failure (Fail _)" do
      let v = Variant.inj (Proxy :: Proxy "boom") "oops" :: Variant Errs
      case Exit.fail v :: Exit Errs Int of
        Failure (Fail v') -> readBoom v' `shouldEqual` "oops"
        _ -> 1 `shouldEqual` 2

    it "Exit.die wraps an Error in Failure (Die _)" do
      let err = error "kapow"
      case Exit.die err :: Exit () Int of
        Failure (Die e) -> message e `shouldEqual` "kapow"
        _ -> 1 `shouldEqual` 2

  describe "folding and conversions" do
    it "foldExit picks the success branch on Success" do
      let e = succeed 7 :: Exit () Int
      foldExit (\_ -> -1) (\a -> a) e `shouldEqual` 7

    it "foldExit picks the failure branch on Failure" do
      let e = Exit.die (error "bang") :: Exit () Int
      foldExit (\_ -> -1) (\a -> a) e `shouldEqual` (-1)

    it "map_ transforms only the success" do
      let
        s = succeed 3 :: Exit () Int
        f = Exit.die (error "no") :: Exit () Int
      foldExit (\_ -> -1) identity (map_ (_ + 100) s) `shouldEqual` 103
      foldExit (\_ -> -1) identity (map_ (_ + 100) f) `shouldEqual` (-1)

    it "toEither / fromEither round-trip on Success" do
      let e = succeed 9 :: Exit () Int
      foldExit (\_ -> -1) identity (fromEither (toEither e)) `shouldEqual` 9

    it "toEither sends Failure to Left" do
      let e = Exit.die (error "nope") :: Exit () Int
      case toEither e of
        Left (Die err) -> message err `shouldEqual` "nope"
        _ -> 1 `shouldEqual` 2

  describe "runRIOExit" do
    it "captures a successful program as Success" do
      let
        program :: RIO () () Int
        program = pure 100
      exit <- runRIOExit program
      foldExit (\_ -> -1) identity exit `shouldEqual` 100

    it "captures a typed failure as Failure (Fail _)" do
      let
        program :: RIO () Errs Int
        program = fail (Proxy :: Proxy "boom") "bad"
      exit <- runRIOExit program
      case exit of
        Failure (Fail v) -> readBoom v `shouldEqual` "bad"
        _ -> 1 `shouldEqual` 2

    it "captures a defect as Failure (Die _) rather than crashing" do
      let
        program :: RIO () () Int
        program = die (error "kaboom")
      exit <- runRIOExit program
      case exit of
        Failure (Die err) -> message err `shouldEqual` "kaboom"
        _ -> 1 `shouldEqual` 2
