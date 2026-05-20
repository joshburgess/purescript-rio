module Test.RIO.Aff.Stream.FilterMapCollectSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, fail, runRIO, runRIO')
import RIO.Aff.Stream (Stream)
import RIO.Aff.Stream as Stream

type Errs = (boom :: String)

spec :: Spec Unit
spec = describe "RIO.Aff.Stream (filterM / collectSome / tapError)" do

  describe "filterM" do
    it "keeps only elements where the effectful predicate returns true" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3, 4, 5, 6 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.filterM (\n -> pure (n `mod` 2 == 0)) s)
      result <- runRIO' program
      result `shouldEqual` [ 2, 4, 6 ]

    it "the predicate runs in RIO and can read shared state" do
      threshold <- liftEffect (Ref.new 3)
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3, 4, 5 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect
          ( Stream.filterM
              (\n -> liftEffect (Ref.read threshold) <#> \t -> n > t)
              s
          )
      result <- runRIO' program
      result `shouldEqual` [ 4, 5 ]

    it "yields an empty stream when nothing passes" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.filterM (\_ -> pure false) s)
      result <- runRIO' program
      result `shouldEqual` []

  describe "collectSome" do
    it "keeps Just values and drops Nothings, with a type change" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3, 4, 5 ]

        program :: RIO () () (Array String)
        program = Stream.runCollect
          ( Stream.collectSome
              (\n -> if n `mod` 2 == 0 then Just (show n <> "!") else Nothing)
              s
          )
      result <- runRIO' program
      result `shouldEqual` [ "2!", "4!" ]

    it "is the identity when the function always returns Just" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 10, 20, 30 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.collectSome Just s)
      result <- runRIO' program
      result `shouldEqual` [ 10, 20, 30 ]

    it "yields an empty stream when the function always returns Nothing" do
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Array Int)
        program = Stream.runCollect
          (Stream.collectSome (\_ -> Nothing :: Maybe Int) s)
      result <- runRIO' program
      result `shouldEqual` []

  describe "tapError" do
    it "fires the handler when the underlying pull fails, then re-raises" do
      seen <- liftEffect (Ref.new (Nothing :: Maybe String))
      let
        failing :: Stream () Errs Int
        failing = Stream.repeatM (fail (Proxy :: Proxy "boom") "kaboom")

        handle :: Variant Errs -> RIO () Errs Unit
        handle v =
          ( Variant.case_
              # Variant.on (Proxy :: Proxy "boom")
                  (\s -> liftEffect (Ref.write (Just s) seen))
          ) v

        program :: RIO () Errs (Array Int)
        program = Stream.runCollect (Stream.tapError handle failing)
      result <- runRIO program
      observed <- liftEffect (Ref.read seen)
      observed `shouldEqual` Just "kaboom"
      case result of
        Left v ->
          ( Variant.case_
              # Variant.on (Proxy :: Proxy "boom") identity
          ) v `shouldEqual` "kaboom"
        Right _ -> 1 `shouldEqual` 0

    it "passes successful streams through untouched (handler never fires)" do
      seen <- liftEffect (Ref.new 0)
      let
        s :: Stream () Errs Int
        s = Stream.fromArray [ 1, 2, 3 ]

        bump :: Variant Errs -> RIO () Errs Unit
        bump _ = liftEffect (Ref.modify_ (_ + 1) seen)

        program :: RIO () Errs (Array Int)
        program = Stream.runCollect (Stream.tapError bump s)
      result <- runRIO program
      case result of
        Right xs -> xs `shouldEqual` [ 1, 2, 3 ]
        Left _ -> 1 `shouldEqual` 0
      fires <- liftEffect (Ref.read seen)
      fires `shouldEqual` 0
