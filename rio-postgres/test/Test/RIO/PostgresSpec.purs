-- | Integration tests for `RIO.Postgres` against a real Postgres
-- | instance. Driven by `Test.Main`, which only invokes `spec`
-- | when a `PG_CONNECTION_STRING` is available.
-- |
-- | Each `it` block builds and tears down its own pool via
-- | `postgresLayer`, so finalizer behavior is exercised
-- | implicitly: a leaked / never-drained pool would eventually
-- | exhaust connections and fail later cases.
module Test.RIO.PostgresSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, provideLayer, runRIO)
import RIO.Error (catchTag, fail) as RIO
import Data.Variant as Variant
import RIO.Postgres
  ( PgError
  , Postgres
  , exec
  , execUsing
  , pgErrorMessage
  , query
  , withClient
  , withTransaction
  )
import RIO.Postgres.Layer (postgresLayer)

dbTag :: Proxy "db"
dbTag = Proxy

forcedTag :: Proxy "forced"
forcedTag = Proxy

type DbErr = (db :: PgError)
type DbErrPlus = (db :: PgError, forced :: Unit)

-- | Build a layer, run `program`, drain the pool on exit.
runWithLayer
  :: forall e a
   . String
  -> RIO (postgres :: Postgres) e a
  -> Aff (Either (Variant e) a)
runWithLayer conn program =
  runRIO (provideLayer (postgresLayer { connectionString: conn }) program)

-- | Drop and recreate the test table inside the same RIO scope so
-- | each `it` starts from a known state.
resetTable :: RIO (postgres :: Postgres) DbErr Unit
resetTable = do
  _ <- exec dbTag ("drop table if exists rio_test_items" :: String)
  _ <- exec dbTag
    ( "create table rio_test_items (id int primary key, label text not null)"
        :: String
    )
  pure unit

-- | Pattern-match on a `Right` result and run an assertion on the
-- | inner value. The typed-failure variant doesn't have `Show` /
-- | `Eq` (the underlying `Effect.Exception.Error` lacks both), so
-- | we can't compare the whole `Either` directly.
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
  describe "RIO.Postgres (integration)" do

    describe "query / exec round-trip" do
      it "exec returns the affected-row count and query reads the inserted rows back" do
        let
          program :: RIO (postgres :: Postgres) DbErr (Int /\ Array (Int /\ String))
          program = do
            resetTable
            inserted <- exec dbTag
              ( "insert into rio_test_items (id, label) values (1, 'one'), (2, 'two')"
                  :: String
              )
            rows <- query dbTag
              ( "select id, label from rio_test_items order by id"
                  :: String
              )
            pure (inserted /\ rows)
        result <- runWithLayer conn program
        expectRight (2 /\ [ 1 /\ "one", 2 /\ "two" ]) result

      it "query decodes a single column into Array Int" do
        let
          program :: RIO (postgres :: Postgres) DbErr (Array Int)
          program = do
            resetTable
            _ <- exec dbTag
              ( "insert into rio_test_items values (10, 'a'), (20, 'b'), (30, 'c')"
                  :: String
              )
            query dbTag
              ("select id from rio_test_items order by id" :: String)
        result <- runWithLayer conn program
        expectRight [ 10, 20, 30 ] result

    describe "withClient" do
      it "returns the client to the pool so subsequent calls succeed on a max=1 pool" do
        let
          -- max=1 makes the test sharp: if release didn't run, the
          -- second `withClient` would block forever waiting on a
          -- connection that never returns.
          program :: RIO (postgres :: Postgres) DbErr (Int /\ Int)
          program = do
            resetTable
            a <- withClient dbTag \_ -> pure 1
            b <- withClient dbTag \_ -> pure 2
            pure (a /\ b)
        result <- runRIO
          ( provideLayer
              ( postgresLayer
                  { connectionString: conn
                  , max: 1
                  }
              )
              program
          )
        expectRight (1 /\ 2) result

    describe "withTransaction" do
      it "commits when the body succeeds" do
        let
          program :: RIO (postgres :: Postgres) DbErr (Array Int)
          program = do
            resetTable
            _ <- withTransaction dbTag \client -> do
              _ <- execUsing dbTag
                ("insert into rio_test_items values (1, 'kept')" :: String)
                client
              execUsing dbTag
                ("insert into rio_test_items values (2, 'kept')" :: String)
                client
            query dbTag
              ("select id from rio_test_items order by id" :: String)
        result <- runWithLayer conn program
        expectRight [ 1, 2 ] result

      it "rolls back on a typed failure raised inside the body" do
        let
          -- Body raises `forced` after inserting; withTransaction
          -- catches every typed failure on the variant, runs
          -- ROLLBACK, then re-raises. We catch `forced` outside the
          -- transaction so the program returns successfully, then
          -- assert the table is empty.
          attempt :: RIO (postgres :: Postgres) DbErrPlus Unit
          attempt = withTransaction dbTag \client -> do
            _ <- execUsing dbTag
              ( "insert into rio_test_items values (99, 'discarded')"
                  :: String
              )
              client
            RIO.fail forcedTag unit

          program :: RIO (postgres :: Postgres) DbErr (Array Int)
          program = do
            resetTable
            RIO.catchTag forcedTag (\_ -> pure unit) attempt
            query dbTag
              ("select id from rio_test_items" :: String)
        result <- runWithLayer conn program
        expectRight ([] :: Array Int) result

    describe "PgError surfacing" do
      it "raises the chosen typed tag when the driver returns an error" do
        let
          program :: RIO (postgres :: Postgres) DbErr Unit
          program = do
            _ <- exec dbTag
              ("this is not valid sql" :: String)
            pure unit
        result <- runWithLayer conn program
        case result of
          Right _ -> fail "expected the malformed query to raise a typed failure"
          Left _ -> pure unit
