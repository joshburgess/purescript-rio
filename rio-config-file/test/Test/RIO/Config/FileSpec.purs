module Test.RIO.Config.FileSpec (spec) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..), isLeft)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Config.File
  ( DotenvError(..)
  , JsonShapeError(..)
  , flattenJson
  , parseDotenv
  )

spec :: Spec Unit
spec = do
  describe "RIO.Config.File" do
    dotenvSpec
    jsonSpec

dotenvSpec :: Spec Unit
dotenvSpec = describe "parseDotenv" do
  it "parses bare KEY=value entries" do
    parseDotenv "PORT=8080\nDATABASE_URL=postgres://localhost\n"
      `shouldYield`
        ( Map.fromFoldable
            [ "PORT" /\ "8080"
            , "DATABASE_URL" /\ "postgres://localhost"
            ]
        )

  it "skips blank lines and comments" do
    parseDotenv "# header\n\nPORT=8080\n# another\n"
      `shouldYield` Map.singleton "PORT" "8080"

  it "supports double-quoted values with escapes" do
    parseDotenv "GREETING=\"hello\\nworld\"\n"
      `shouldYield` Map.singleton "GREETING" "hello\nworld"

  it "supports single-quoted values without escape processing" do
    parseDotenv "PATTERN='a\\nb'\n"
      `shouldYield` Map.singleton "PATTERN" "a\\nb"

  it "strips `export ` prefixes" do
    parseDotenv "export AWS_REGION=us-east-1\n"
      `shouldYield` Map.singleton "AWS_REGION" "us-east-1"

  it "strips trailing comments outside quotes" do
    parseDotenv "PORT=8080 # the http port\n"
      `shouldYield` Map.singleton "PORT" "8080"

  it "keeps `#` literal inside double quotes" do
    parseDotenv "RAW=\"a # b\"\n"
      `shouldYield` Map.singleton "RAW" "a # b"

  it "reports a 1-based line number on parse failure" do
    case parseDotenv "OK=1\nBROKEN\nOK2=2\n" of
      Left (DotenvError ln _) -> ln `shouldEqual` 2
      Right _ -> fail "expected parse error"

  it "rejects empty keys" do
    isLeft (parseDotenv "=value\n") `shouldEqual` true

  it "rejects unterminated double quotes" do
    isLeft (parseDotenv "OOPS=\"never closed\n") `shouldEqual` true

jsonSpec :: Spec Unit
jsonSpec = describe "flattenJson" do
  it "flattens nested objects with `_` joins" do
    flattenJson
      ( parseJsonOrCrash
          """{ "DB": { "URL": "postgres://localhost", "POOL": 8 } }"""
      )
      `shouldYield`
        ( Map.fromFoldable
            [ "DB_URL" /\ "postgres://localhost"
            , "DB_POOL" /\ "8"
            ]
        )

  it "renders booleans as true/false" do
    flattenJson (parseJsonOrCrash """{ "DEBUG": true, "QUIET": false }""")
      `shouldYield`
        ( Map.fromFoldable
            [ "DEBUG" /\ "true"
            , "QUIET" /\ "false"
            ]
        )

  it "drops null values" do
    flattenJson (parseJsonOrCrash """{ "PORT": null, "HOST": "x" }""")
      `shouldYield` Map.singleton "HOST" "x"

  it "rejects array values with their path" do
    case flattenJson (parseJsonOrCrash """{ "TAGS": [1, 2, 3] }""") of
      Left (JsonShapeError path _) -> path `shouldEqual` [ "TAGS" ]
      Right _ -> fail "expected JsonShapeError"

  it "rejects non-object top-level values" do
    isLeft (flattenJson (parseJsonOrCrash "42")) `shouldEqual` true
    isLeft (flattenJson (parseJsonOrCrash "\"oops\"")) `shouldEqual` true

shouldYield
  :: forall e a
   . Show e
  => Show a
  => Eq a
  => Either e a
  -> a
  -> Aff Unit
shouldYield actual expected = case actual of
  Right v -> v `shouldEqual` expected
  Left err -> fail (show err)

parseJsonOrCrash :: String -> Json
parseJsonOrCrash s = case jsonParser s of
  Right j -> j
  Left err -> unsafeCrashWith ("test JSON did not parse: " <> err)
