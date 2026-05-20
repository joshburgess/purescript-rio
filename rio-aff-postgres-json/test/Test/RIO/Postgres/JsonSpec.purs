-- | Integration tests for `RIO.Aff.Postgres.Json`. Exercises the
-- | round-trip of typed values through a real `jsonb` column.
module Test.RIO.Aff.Postgres.JsonSpec (spec) where

import Prelude

import Data.Argonaut.Core (Json, fromString, stringify)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, provideLayer, runRIO)
import RIO.Aff.Postgres
  ( PgError
  , Postgres
  , exec
  , execParams
  , pgErrorMessage
  , query
  , queryParams
  )
import RIO.Aff.Postgres.Json (JsonB(..))
import RIO.Aff.Postgres.Layer (postgresLayer)

dbTag :: Proxy "db"
dbTag = Proxy

type DbErr = (db :: PgError)

newtype Event = Event { kind :: String, count :: Int }

derive newtype instance Show Event
derive newtype instance Eq Event

instance EncodeJson Event where
  encodeJson (Event r) = encodeJson r

instance DecodeJson Event where
  decodeJson j = map Event (decodeJson j)

runWith
  :: forall e a
   . String
  -> RIO (postgres :: Postgres) e a
  -> Aff (Either (Variant e) a)
runWith conn program =
  runRIO (provideLayer (postgresLayer { connectionString: conn }) program)

resetTable :: RIO (postgres :: Postgres) DbErr Unit
resetTable = do
  _ <- exec dbTag ("drop table if exists rio_json_test" :: String)
  _ <- exec dbTag
    ( "create table rio_json_test \
      \(id int primary key, payload jsonb not null)" :: String
    )
  pure unit

expectRight
  :: forall a
   . Show a
  => Eq a
  => a
  -> Either (Variant DbErr) a
  -> Aff Unit
expectRight expected = case _ of
  Right a -> a `shouldEqual` expected
  Left v -> fail
    ( "expected Right, got typed failure: "
        <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
    )

spec :: String -> Spec Unit
spec conn = do
  describe "RIO.Aff.Postgres.Json (integration)" do

    it "round-trips a typed value through a jsonb column" do
      let
        evt = Event { kind: "login", count: 7 }

        program :: RIO (postgres :: Postgres) DbErr (Maybe (JsonB Event))
        program = do
          resetTable
          _ <- execParams dbTag
            "insert into rio_json_test (id, payload) values ($1, $2)"
            (1 /\ JsonB evt)
          queryParams dbTag
            "select payload from rio_json_test where id = $1"
            1
      result <- runWith conn program
      expectRight (Just (JsonB evt)) result

    it "round-trips an untyped Json value" do
      let
        raw :: Json
        raw = fromString "hello"

        program :: RIO (postgres :: Postgres) DbErr (Maybe (JsonB Json))
        program = do
          resetTable
          _ <- execParams dbTag
            "insert into rio_json_test (id, payload) values ($1, $2)"
            (1 /\ JsonB raw)
          queryParams dbTag
            "select payload from rio_json_test where id = $1"
            1
      result <- runWith conn program
      case result of
        Right (Just (JsonB j)) -> stringify j `shouldEqual` stringify raw
        Right Nothing -> fail "expected a row, got Nothing"
        Left v -> fail
          ( "expected Right, got typed failure: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )

    it "raises a typed failure when the column doesn't decode into the requested shape" do
      let
        program :: RIO (postgres :: Postgres) DbErr (Maybe (JsonB Event))
        program = do
          resetTable
          _ <- execParams dbTag
            "insert into rio_json_test (id, payload) values ($1, $2::jsonb)"
            (1 /\ "{\"unrelated\":true}")
          queryParams dbTag
            "select payload from rio_json_test where id = $1"
            1
      result <- runWith conn program
      case result of
        Right _ -> fail
          "expected the decode mismatch to raise a typed failure"
        Left _ -> pure unit

    it "decodes an array of jsonb rows" do
      let
        a = Event { kind: "a", count: 1 }
        b = Event { kind: "b", count: 2 }

        program :: RIO (postgres :: Postgres) DbErr (Array (JsonB Event))
        program = do
          resetTable
          _ <- execParams dbTag
            "insert into rio_json_test (id, payload) values ($1, $2)"
            (1 /\ JsonB a)
          _ <- execParams dbTag
            "insert into rio_json_test (id, payload) values ($1, $2)"
            (2 /\ JsonB b)
          query dbTag
            ( "select payload from rio_json_test order by id" :: String
            )
      result <- runWith conn program
      expectRight [ JsonB a, JsonB b ] result
