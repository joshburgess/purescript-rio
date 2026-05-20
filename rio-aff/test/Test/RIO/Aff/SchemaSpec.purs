module Test.RIO.Aff.SchemaSpec (spec) where

import Prelude

import Data.Argonaut.Core as Json
import Data.Array (length) as Array
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))

import RIO.Aff.Schema (DecodeError(..))
import RIO.Aff.Schema as S

data Status = Active | Inactive

derive instance eqStatus :: Eq Status
instance showStatus :: Show Status where
  show Active = "Active"
  show Inactive = "Inactive"

statusSchema :: S.Schema Status
statusSchema = S.enum
  [ Tuple "active" Active, Tuple "inactive" Inactive ]
  ( case _ of
      Active -> "active"
      Inactive -> "inactive"
  )

type User =
  { name :: String
  , age :: Int
  , email :: Maybe String
  , status :: Status
  }

userSchema :: S.Schema User
userSchema = S.recordOf $
  { name: _, age: _, email: _, status: _ }
    <$> S.field "name" _.name S.string
    <*> S.field "age" _.age S.int
    <*> S.fieldOpt "email" _.email S.string
    <*> S.fieldDefault "status" _.status Active statusSchema

spec :: Spec Unit
spec = describe "RIO.Aff.Schema" do
  describe "primitives" do
    it "decodes strings" do
      S.parseJson S.string "\"hi\"" `shouldEqual` Right "hi"

    it "rejects non-strings with TypeMismatch" do
      S.parseJson S.string "42"
        `shouldEqual`
          Left (TypeMismatch { expected: "string", got: "number" })

    it "decodes ints from JSON numbers" do
      S.parseJson S.int "7" `shouldEqual` Right 7

    it "rejects non-integral numbers" do
      S.parseJson S.int "1.5" `shouldSatisfy` isLeft

    it "decodes booleans and null" do
      S.parseJson S.boolean "true" `shouldEqual` Right true
      S.parseJson S.null_ "null" `shouldEqual` Right unit

    it "parseJson surfaces parser errors" do
      case S.parseJson S.string "{ bad" of
        Left (ParseError _) -> pure unit
        other -> fail ("expected ParseError, got: " <> show other)

  describe "array" do
    it "decodes element-by-element" do
      S.parseJson (S.array S.int) "[1,2,3]"
        `shouldEqual` Right [ 1, 2, 3 ]

    it "tags element failures with InvalidIndex" do
      S.parseJson (S.array S.int) "[1,\"oops\",3]"
        `shouldEqual`
          Left
            ( InvalidIndex 1
                ( TypeMismatch { expected: "int", got: "string" }
                )
            )

  describe "nullable" do
    it "accepts null as Nothing" do
      S.parseJson (S.nullable S.int) "null"
        `shouldEqual` Right Nothing

    it "accepts values as Just" do
      S.parseJson (S.nullable S.int) "5"
        `shouldEqual` Right (Just 5)

  describe "refine" do
    it "rejects values failing the predicate" do
      let
        positive = S.refine
          (\n -> if n > 0 then Nothing else Just "must be positive")
          S.int
      S.parseJson positive "5" `shouldEqual` Right 5
      S.parseJson positive "-1"
        `shouldEqual` Left (RefinementFailed "must be positive")

  describe "transform" do
    it "maps decoded values through a function" do
      let doubled = S.transform (_ * 2) (_ / 2) S.int
      S.parseJson doubled "3" `shouldEqual` Right 6
      Json.stringify (S.encode doubled 10) `shouldEqual` "5"

  describe "union" do
    it "succeeds on the first branch that matches" do
      let
        intAsString = S.transform (show :: Int -> String) (\_ -> 0) S.int
        stringOrInt = S.union [ S.string, intAsString ]
      S.parseJson stringOrInt "\"hi\"" `shouldEqual` Right "hi"
      S.parseJson stringOrInt "42" `shouldEqual` Right "42"

    it "collects all branch errors on total failure" do
      let
        intAsString = S.transform (show :: Int -> String) (\_ -> 0) S.int
        boolAsString = S.transform (show :: Boolean -> String) (\_ -> false) S.boolean
        only = S.union [ intAsString, boolAsString ]
      case S.parseJson only "\"oops\"" of
        Left (UnionMismatch errs) -> Array.length errs `shouldEqual` 2
        other -> fail ("expected UnionMismatch, got: " <> show other)

  describe "enum" do
    it "round-trips through the lookup table" do
      S.parseJson statusSchema "\"active\""
        `shouldEqual` Right Active
      Json.stringify (S.encode statusSchema Inactive)
        `shouldEqual` "\"inactive\""

    it "rejects unknown tags" do
      case S.parseJson statusSchema "\"frozen\"" of
        Left (RefinementFailed _) -> pure unit
        other -> fail ("expected RefinementFailed, got: " <> show other)

  describe "records" do
    it "decodes a full record" do
      S.parseJson userSchema
        "{\"name\":\"ada\",\"age\":36,\"email\":\"ada@example.com\",\"status\":\"active\"}"
        `shouldEqual` Right
          { name: "ada"
          , age: 36
          , email: Just "ada@example.com"
          , status: Active
          }

    it "supplies defaults for missing fields and omits Nothing on encode" do
      S.parseJson userSchema "{\"name\":\"linus\",\"age\":54}"
        `shouldEqual` Right
          { name: "linus", age: 54, email: Nothing, status: Active }
      Json.stringify
        ( S.encode userSchema
            { name: "linus"
            , age: 54
            , email: Nothing
            , status: Active
            }
        )
        `shouldEqual`
          "{\"name\":\"linus\",\"age\":54,\"status\":\"active\"}"

    it "wraps field decode failures in InvalidField" do
      S.parseJson userSchema "{\"name\":\"x\",\"age\":\"old\"}"
        `shouldEqual`
          Left
            ( InvalidField "age"
                (TypeMismatch { expected: "int", got: "string" })
            )

    it "reports missing required fields" do
      S.parseJson userSchema "{\"age\":1}"
        `shouldEqual` Left (MissingField "name")

  describe "renderError" do
    it "renders a path-and-reason summary" do
      S.renderError
        ( InvalidField "user"
            ( InvalidIndex 2
                (TypeMismatch { expected: "string", got: "number" })
            )
        )
        `shouldEqual` "expected string at $.user[2], got number"

  describe "toJsonSchema" do
    it "describes primitives" do
      Json.stringify (S.toJsonSchema S.string)
        `shouldEqual` "{\"type\":\"string\"}"
      Json.stringify (S.toJsonSchema S.int)
        `shouldEqual` "{\"type\":\"integer\"}"
      Json.stringify (S.toJsonSchema S.number)
        `shouldEqual` "{\"type\":\"number\"}"
      Json.stringify (S.toJsonSchema S.boolean)
        `shouldEqual` "{\"type\":\"boolean\"}"
      Json.stringify (S.toJsonSchema S.null_)
        `shouldEqual` "{\"type\":\"null\"}"

    it "describes arrays with items" do
      Json.stringify (S.toJsonSchema (S.array S.int))
        `shouldEqual` "{\"type\":\"array\",\"items\":{\"type\":\"integer\"}}"

    it "describes nullable as anyOf" do
      Json.stringify (S.toJsonSchema (S.nullable S.string))
        `shouldEqual`
          "{\"anyOf\":[{\"type\":\"string\"},{\"type\":\"null\"}]}"

    it "describes enums with string + enum array" do
      Json.stringify (S.toJsonSchema statusSchema)
        `shouldEqual`
          "{\"type\":\"string\",\"enum\":[\"active\",\"inactive\"]}"

    it "describes records with properties and required keys" do
      Json.stringify (S.toJsonSchema userSchema)
        `shouldEqual`
          ( "{\"type\":\"object\","
              <> "\"properties\":{"
              <> "\"name\":{\"type\":\"string\"},"
              <> "\"age\":{\"type\":\"integer\"},"
              <> "\"email\":{\"type\":\"string\"},"
              <> "\"status\":{\"type\":\"string\","
              <> "\"enum\":[\"active\",\"inactive\"]}},"
              <> "\"required\":[\"name\",\"age\"]}"
          )

    it "refine inherits the inner describe" do
      let
        positive = S.refine
          (\n -> if n > 0 then Nothing else Just "must be positive")
          S.int
      Json.stringify (S.toJsonSchema positive)
        `shouldEqual` "{\"type\":\"integer\"}"

  describe "brand" do
    it "decodes and unbrands cleanly" do
      let
        userIdSchema :: S.Schema (S.Branded "UserId" Int)
        userIdSchema = S.brand (Proxy :: Proxy "UserId") S.int
      case S.parseJson userIdSchema "42" of
        Right b -> S.unbrand b `shouldEqual` 42
        other -> fail ("expected Right, got: " <> show other)

    it "encodes the branded value transparently" do
      let
        userIdSchema :: S.Schema (S.Branded "UserId" Int)
        userIdSchema = S.brand (Proxy :: Proxy "UserId") S.int
      case S.parseJson userIdSchema "99" of
        Right b ->
          Json.stringify (S.encode userIdSchema b) `shouldEqual` "99"
        other -> fail ("expected Right, got: " <> show other)

    it "adds title to the JSON Schema fragment" do
      let
        userIdSchema :: S.Schema (S.Branded "UserId" Int)
        userIdSchema = S.brand (Proxy :: Proxy "UserId") S.int
      Json.stringify (S.toJsonSchema userIdSchema)
        `shouldEqual`
          "{\"type\":\"integer\",\"title\":\"UserId\"}"
