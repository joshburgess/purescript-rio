module Test.RIO.ConfigSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant as Variant
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Config
  ( Config
  , ConfigError(..)
  , Source
  , boolean
  , int
  , load
  , mapSource
  , nested
  , optional
  , prettyConfigError
  , secret
  , string
  , unSecret
  , withDefault
  )
import RIO.Core (RIO, runRIO)

cfgTag :: Proxy "config"
cfgTag = Proxy

type Err = (config :: ConfigError)

runWith
  :: forall a
   . Source
  -> Config a
  -> Aff (Either (Variant.Variant Err) a)
runWith src c = runRIO (load cfgTag src c :: RIO () Err a)

type AppConfig =
  { port :: Int
  , dbUrl :: String
  , debug :: Boolean
  , apiKey :: String
  }

appConfig :: Config AppConfig
appConfig = { port: _, dbUrl: _, debug: _, apiKey: _ }
  <$> withDefault 8080 (int "PORT")
  <*> string "DATABASE_URL"
  <*> withDefault false (boolean "DEBUG")
  <*> map unSecret (secret "API_KEY")

spec :: Spec Unit
spec = do
  describe "RIO.Config" do

    describe "primitives" do
      it "loads a present string" do
        let src = mapSource (Map.singleton "FOO" "bar")
        r <- runWith src (string "FOO")
        r `shouldEqual` Right "bar"

      it "loads a present int" do
        let src = mapSource (Map.singleton "N" "42")
        r <- runWith src (int "N")
        r `shouldEqual` Right 42

      it "rejects an unparseable int" do
        let src = mapSource (Map.singleton "N" "notanumber")
        r <- runWith src (int "N")
        case r of
          Left v -> case Variant.case_ # Variant.on cfgTag identity $ v of
            ParseError [] "N" _ -> pure unit
            other -> fail
              ("expected ParseError, got: " <> show other)
          Right _ -> fail "expected a failure"

      it "loads a boolean (accepting truthy synonyms)" do
        let src = mapSource (Map.singleton "B" "yes")
        r <- runWith src (boolean "B")
        r `shouldEqual` Right true

    describe "optional / withDefault" do
      it "optional yields Nothing when the key is missing" do
        let src = mapSource Map.empty
        r <- runWith src (optional (string "X"))
        r `shouldEqual` (Right Nothing :: Either _ (Maybe String))

      it "optional yields Just on a present key" do
        let src = mapSource (Map.singleton "X" "hello")
        r <- runWith src (optional (string "X"))
        r `shouldEqual` Right (Just "hello")

      it "withDefault substitutes for a missing key" do
        let src = mapSource Map.empty
        r <- runWith src (withDefault 99 (int "PORT"))
        r `shouldEqual` Right 99

      it "withDefault does not mask a ParseError" do
        let src = mapSource (Map.singleton "PORT" "abc")
        r <- runWith src (withDefault 99 (int "PORT"))
        case r of
          Left _ -> pure unit
          Right _ -> fail "expected ParseError to propagate"

    describe "nested" do
      it "prefixes the key with the namespace" do
        let
          src = mapSource
            ( Map.fromFoldable
                [ "DB_URL" /\ "postgres://localhost"
                ]
            )
          c = nested "DB" (string "URL")
        r <- runWith src c
        r `shouldEqual` Right "postgres://localhost"

    describe "accumulation" do
      it "reports both failures from a record-shaped descriptor" do
        let
          src = mapSource
            ( Map.fromFoldable
                [ "PORT" /\ "abc"
                ]
            )
        -- missing DATABASE_URL + unparseable PORT
        r <- runWith src appConfig
        case r of
          Left v ->
            case Variant.case_ # Variant.on cfgTag identity $ v of
              Multi _ -> pure unit
              other -> fail
                ("expected Multi error, got: " <> show other)
          Right _ -> fail "expected a failure"

    describe "secret" do
      it "redacts the value in its Show instance" do
        let src = mapSource (Map.singleton "K" "supersecret")
        r <- runWith src (secret "K")
        case r of
          Right s -> show s `shouldEqual` "<redacted>"
          Left _ -> fail "expected the secret to load"

    describe "prettyConfigError" do
      it "renders MissingKey on a single line with the bare key" do
        prettyConfigError (MissingKey [] "DATABASE_URL")
          `shouldEqual` "missing required config key: DATABASE_URL"

      it "renders MissingKey with a namespace path joined by dots" do
        prettyConfigError (MissingKey [ "DB" ] "URL")
          `shouldEqual` "missing required config key: DB.URL"

      it "renders ParseError with the message after the colon" do
        prettyConfigError (ParseError [] "PORT" "not a number")
          `shouldEqual` "could not parse config key PORT: not a number"

      it "renders Multi as a header plus an indented bullet per child" do
        let
          err =
            Multi
              ( NEL.cons' (MissingKey [] "DATABASE_URL")
                  (ParseError [] "PORT" "not a number" : Nil)
              )
        prettyConfigError err `shouldEqual`
          ( "config failed to load:\n"
              <> "  - missing required config key: DATABASE_URL\n"
              <> "  - could not parse config key PORT: not a number"
          )
