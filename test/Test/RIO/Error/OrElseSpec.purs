module Test.RIO.Error.OrElseSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant as Variant
import Effect.Exception as Exception
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO, runRIO', sandbox)
import RIO.Error (mapBoth, option, orDie, orElse)

spec :: Spec Unit
spec = describe "RIO.Error (orElse / option / orDie / mapBoth)" do

  describe "orElse" do
    it "returns the first action's value when it succeeds" do
      let
        program :: RIO () () Int
        program =
          orElse
            (pure 1 :: RIO () (boom :: Int) Int)
            (pure 2)
      result <- runRIO' program
      result `shouldEqual` 1

    it "falls back when the first action fails" do
      let
        program :: RIO () () Int
        program =
          orElse
            (fail (Proxy :: Proxy "boom") 99 :: RIO () (boom :: Int) Int)
            (pure 42)
      result <- runRIO' program
      result `shouldEqual` 42

  describe "option" do
    it "wraps a success as Just" do
      let
        program :: RIO () () (Maybe Int)
        program = option (pure 7 :: RIO () (boom :: Int) Int)
      result <- runRIO' program
      result `shouldEqual` Just 7

    it "wraps a typed failure as Nothing" do
      let
        program :: RIO () () (Maybe Int)
        program = option
          (fail (Proxy :: Proxy "boom") 1 :: RIO () (boom :: Int) Int)
      result <- runRIO' program
      result `shouldEqual` Nothing

  describe "orDie" do
    it "passes a success through unchanged" do
      let
        program :: RIO () () Int
        program = orDie
          (\_ -> Exception.error "n/a")
          (pure 5 :: RIO () (boom :: Int) Int)
      result <- runRIO' program
      result `shouldEqual` 5

    it "converts a typed failure into a defect (observable via sandbox)" do
      let
        program :: RIO () () (Either _ Int)
        program = sandbox
          ( orDie
              ( \v ->
                  let
                    payload =
                      Variant.case_
                        # Variant.on (Proxy :: Proxy "boom") identity
                        $ v
                  in
                    Exception.error ("died: " <> show payload)
              )
              (fail (Proxy :: Proxy "boom") 7 :: RIO () (boom :: Int) Int)
          )
      result <- runRIO' program
      case result of
        Left err -> Exception.message err `shouldEqual` "died: 7"
        Right _ -> 1 `shouldEqual` 0

  describe "mapBoth" do
    it "maps the success arm and leaves the error untouched on success" do
      let
        program :: RIO () (boom :: Int) String
        program = mapBoth
          identity
          (\n -> "got " <> show n)
          (pure 3 :: RIO () (boom :: Int) Int)
      result <- runRIO program
      result `shouldEqual` (Right "got 3" :: Either _ _)

    it "maps the error arm when the input fails, leaving the value type alone" do
      let
        rename
          :: Variant.Variant (boom :: Int)
          -> Variant.Variant (renamed :: String)
        rename v =
          Variant.case_
            # Variant.on (Proxy :: Proxy "boom")
                ( \n ->
                    Variant.inj (Proxy :: Proxy "renamed") ("E" <> show n)
                )
            $ v

        program :: RIO () (renamed :: String) Int
        program = mapBoth
          rename
          identity
          (fail (Proxy :: Proxy "boom") 42 :: RIO () (boom :: Int) Int)
      result <- runRIO program
      case result of
        Left v ->
          let
            payload =
              Variant.case_
                # Variant.on (Proxy :: Proxy "renamed") identity
                $ v
          in
            payload `shouldEqual` "E42"
        Right _ -> 1 `shouldEqual` 0
