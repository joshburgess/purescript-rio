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
  , mkSource
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

      it "accepts every documented truthy and falsy synonym, case-insensitive" do
        -- The docstring promises `true`/`false`, `yes`/`no`,
        -- `on`/`off`, `1`/`0`, case-insensitive. The previous
        -- test only covered "yes"; pin the rest of the contract
        -- so any silent narrowing of the accepted set is caught.
        let
          check raw expected = do
            let src = mapSource (Map.singleton "B" raw)
            r <- runWith src (boolean "B")
            r `shouldEqual` Right expected
        check "true" true
        check "TRUE" true
        check "yes" true
        check "On" true
        check "1" true
        check "false" false
        check "FALSE" false
        check "no" false
        check "off" false
        check "0" false

      it "rejects values outside the accepted synonym set" do
        let src = mapSource (Map.singleton "B" "maybe")
        r <- runWith src (boolean "B")
        case r of
          Left v ->
            case Variant.case_ # Variant.on cfgTag identity $ v of
              ParseError [] "B" _ -> pure unit
              other -> fail
                ("expected ParseError, got: " <> show other)
          Right _ -> fail "expected a failure"

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

      it "optional does not mask a ParseError" do
        -- The docstring promises only `MissingKey` is softened to
        -- `Nothing`; "ParseError and other failures still propagate".
        -- Pin this so a future refactor that widens `optional` to
        -- swallow parse failures is caught.
        let src = mapSource (Map.singleton "PORT" "abc")
        r <- runWith src (optional (int "PORT"))
        case r of
          Left v ->
            case Variant.case_ # Variant.on cfgTag identity $ v of
              ParseError [] "PORT" _ -> pure unit
              other -> fail
                ("expected ParseError, got: " <> show other)
          Right _ -> fail "expected ParseError to propagate through optional"

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

      it "composes: two nested layers produce an `OUTER_INNER_K` key" do
        -- The docstring only shows the one-level form. Pin
        -- composition explicitly so a future refactor that
        -- joins the path differently is caught.
        let
          src = mapSource
            ( Map.fromFoldable [ "APP_DB_URL" /\ "x" ]
            )
          c = nested "APP" (nested "DB" (string "URL"))
        r <- runWith src c
        r `shouldEqual` Right "x"

      it "carries the namespace into the failure path" do
        -- A MissingKey under `nested "DB"` should report the
        -- path so prettyConfigError can render `DB.URL`.
        let
          src = mapSource Map.empty
          c = nested "DB" (string "URL")
        r <- runWith src c
        case r of
          Left v ->
            case Variant.case_ # Variant.on cfgTag identity $ v of
              MissingKey path "URL" -> path `shouldEqual` [ "DB" ]
              other -> fail
                ("expected MissingKey [\"DB\"] \"URL\", got: " <> show other)
          Right _ -> fail "expected a failure"

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

      it "flattens Multi so three accumulated errors come back at one level" do
        -- The `combine` docstring promises "Flattens `Multi` so
        -- nesting stays shallow regardless of how the descriptor
        -- tree was assembled." The existing two-failure test
        -- cannot distinguish a flat `Multi [a, b]` from any
        -- alternative two-element layout. Pin the flatten promise
        -- with three independent failures from a record-shaped
        -- descriptor (3 missing keys) and assert the resulting
        -- `Multi` carries exactly three children at the top level.
        let
          threeFieldConfig
            :: Config { a :: String, b :: String, c :: String }
          threeFieldConfig = { a: _, b: _, c: _ }
            <$> string "A"
            <*> string "B"
            <*> string "C"

          src = mapSource Map.empty
        r <- runWith src threeFieldConfig
        case r of
          Left v ->
            case Variant.case_ # Variant.on cfgTag identity $ v of
              Multi children ->
                NEL.length children `shouldEqual` 3
              other -> fail
                ("expected flat Multi of length 3, got: " <> show other)
          Right _ -> fail "expected a failure"

    describe "mkSource" do
      -- The whole suite uses `mapSource` to build sources for
      -- tests. `mkSource` is the more general escape hatch: it
      -- takes an arbitrary `String -> Maybe String` lookup. Pin
      -- that a Config descriptor reads through a custom function
      -- in the same way it would read through a Map.
      it "reads values through a custom lookup function" do
        let
          lookup k = case k of
            "PORT" -> Just "9090"
            _ -> Nothing
          src = mkSource lookup
        r <- runWith src (int "PORT")
        r `shouldEqual` Right 9090

      it "produces a MissingKey error when the lookup returns Nothing" do
        let src = mkSource (\_ -> Nothing)
        r <- runWith src (string "ABSENT")
        case r of
          Left v ->
            case Variant.case_ # Variant.on cfgTag identity $ v of
              MissingKey _ "ABSENT" -> pure unit
              other -> fail ("expected MissingKey, got: " <> show other)
          Right _ -> fail "expected MissingKey failure"

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
