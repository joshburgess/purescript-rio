module Test.RIO.OpenApiSpec (spec) where

import Prelude

import Data.Argonaut.Core as Json
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.HttpClient (Method(..))
import RIO.OpenApi
  ( ParameterIn(..)
  , defaultDoc
  , emit
  , jsonContent
  , operation
  , pathParam
  , queryParam
  , response
  )
import RIO.Schema as S

spec :: Spec Unit
spec = describe "RIO.OpenApi" do
  describe "emit" do
    it "emits openapi=3.1.0 with title and version" do
      let
        doc = defaultDoc "Users API" "1.0.0"
        j = emit doc
      Json.stringify j `shouldEqual`
        "{\"openapi\":\"3.1.0\",\"info\":{\"title\":\"Users API\",\"version\":\"1.0.0\"},\"paths\":{}}"

    it "omits empty servers" do
      let
        doc = (defaultDoc "Demo" "0.1.0") { servers = [] }
        j = emit doc
      Json.stringify j `shouldEqual`
        "{\"openapi\":\"3.1.0\",\"info\":{\"title\":\"Demo\",\"version\":\"0.1.0\"},\"paths\":{}}"

    it "includes servers when present" do
      let
        doc = (defaultDoc "Demo" "0.1.0")
          { servers =
              [ { url: "https://api.example.com"
                , description: Just "prod"
                }
              ]
          }
        j = emit doc
      Json.stringify j `shouldEqual`
        ( "{\"openapi\":\"3.1.0\""
            <> ",\"info\":{\"title\":\"Demo\",\"version\":\"0.1.0\"}"
            <> ",\"servers\":[{\"url\":\"https://api.example.com\",\"description\":\"prod\"}]"
            <> ",\"paths\":{}}"
        )

    it "renders a single GET operation with a 200 JSON response" do
      let
        op =
          (operation GET)
            { summary = Just "List users"
            , operationId = Just "listUsers"
            , tags = [ "users" ]
            , responses =
                [ response "200" "ok"
                    (Just (jsonContent (S.toJsonSchema S.string)))
                ]
            }

        doc = (defaultDoc "Demo" "0.1.0")
          { paths =
              [ { path: "/users"
                , operations: [ op ]
                }
              ]
          }
        j = emit doc

      let
        obj = case Json.toObject j of
          Just o -> o
          Nothing -> Object.empty

      case Object.lookup "paths" obj of
        Just pathsJ -> case Json.toObject pathsJ of
          Just pathsObj -> case Object.lookup "/users" pathsObj of
            Just _ -> pure unit
            Nothing -> 1 `shouldEqual` 0
          Nothing -> 1 `shouldEqual` 0
        Nothing -> 1 `shouldEqual` 0

    it "emits methods as lowercase keys" do
      let
        op = (operation POST)
          { responses =
              [ response "201" "created" Nothing ]
          }
        doc = (defaultDoc "Demo" "0.1.0")
          { paths =
              [ { path: "/x", operations: [ op ] } ]
          }
        s = Json.stringify (emit doc)
      -- the method key must be lowercase
      (s `containsString` "\"post\":") `shouldEqual` true

    it "includes path parameters with required=true" do
      let
        param = pathParam "id" (S.toJsonSchema S.string)
        op = (operation GET)
          { parameters = [ param ]
          , responses = [ response "200" "ok" Nothing ]
          }
        doc = (defaultDoc "Demo" "0.1.0")
          { paths =
              [ { path: "/users/{id}", operations: [ op ] } ]
          }
        s = Json.stringify (emit doc)
      (s `containsString` "\"in\":\"path\"") `shouldEqual` true
      (s `containsString` "\"required\":true") `shouldEqual` true
      (s `containsString` "\"name\":\"id\"") `shouldEqual` true

    it "query parameters default to required=false" do
      let
        param = queryParam "q" (S.toJsonSchema S.string)
        op = (operation GET)
          { parameters = [ param ]
          , responses = [ response "200" "ok" Nothing ]
          }
        doc = (defaultDoc "Demo" "0.1.0")
          { paths =
              [ { path: "/search", operations: [ op ] } ]
          }
        s = Json.stringify (emit doc)
      (s `containsString` "\"in\":\"query\"") `shouldEqual` true
      (s `containsString` "\"required\":false") `shouldEqual` true

    it "includes a requestBody when present" do
      let
        op = (operation POST)
          { requestBody = Just
              { description: Just "create user"
              , required: true
              , content: jsonContent (S.toJsonSchema S.string)
              }
          , responses = [ response "201" "created" Nothing ]
          }
        doc = (defaultDoc "Demo" "0.1.0")
          { paths =
              [ { path: "/users", operations: [ op ] } ]
          }
        s = Json.stringify (emit doc)
      (s `containsString` "\"requestBody\"") `shouldEqual` true
      (s `containsString` "\"application/json\"") `shouldEqual` true

    it "emits components.schemas when present" do
      let
        doc = (defaultDoc "Demo" "0.1.0")
          { components =
              { schemas:
                  [ Tuple "User" (S.toJsonSchema S.string) ]
              }
          }
        s = Json.stringify (emit doc)
      (s `containsString` "\"components\"") `shouldEqual` true
      (s `containsString` "\"schemas\"") `shouldEqual` true
      (s `containsString` "\"User\"") `shouldEqual` true

    it "omits components when no schemas are declared" do
      let
        doc = defaultDoc "Demo" "0.1.0"
        s = Json.stringify (emit doc)
      (s `containsString` "\"components\"") `shouldEqual` false

  describe "ParameterIn show" do
    it "renders InPath as 'path'" do
      show InPath `shouldEqual` "path"
    it "renders InQuery as 'query'" do
      show InQuery `shouldEqual` "query"
    it "renders InHeader as 'header'" do
      show InHeader `shouldEqual` "header"
    it "renders InCookie as 'cookie'" do
      show InCookie `shouldEqual` "cookie"

containsString :: String -> String -> Boolean
containsString haystack needle = contains (Pattern needle) haystack

