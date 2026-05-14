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
  , execParams
  , execParamsUsing
  , execPrepared
  , execPreparedUsing
  , execUsing
  , pgErrorMessage
  , query
  , queryParams
  , queryPrepared
  , queryPreparedUsing
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

    describe "parameterized queries" do
      it "execParams binds a single scalar and queryParams reads it back" do
        let
          program :: RIO (postgres :: Postgres) DbErr (Array (Int /\ String))
          program = do
            resetTable
            _ <- execParams dbTag
              "insert into rio_test_items (id, label) values ($1, $2)"
              (1 /\ "one")
            queryParams dbTag
              "select id, label from rio_test_items where id = $1"
              1
        result <- runWithLayer conn program
        expectRight [ 1 /\ "one" ] result

      it "queryParams binds multiple parameters in left-to-right order" do
        let
          program :: RIO (postgres :: Postgres) DbErr (Array Int)
          program = do
            resetTable
            _ <- exec dbTag
              ( "insert into rio_test_items values (1, 'a'), (2, 'b'), (3, 'c')"
                  :: String
              )
            queryParams dbTag
              "select id from rio_test_items where id >= $1 and id <= $2 order by id"
              (2 /\ 3)
        result <- runWithLayer conn program
        expectRight [ 2, 3 ] result

      it "execParamsUsing runs on the transaction client" do
        let
          program :: RIO (postgres :: Postgres) DbErr (Array Int)
          program = do
            resetTable
            _ <- withTransaction dbTag \client -> do
              _ <- execParamsUsing dbTag
                "insert into rio_test_items (id, label) values ($1, $2)"
                (10 /\ "ten")
                client
              execParamsUsing dbTag
                "insert into rio_test_items (id, label) values ($1, $2)"
                (20 /\ "twenty")
                client
            query dbTag
              ("select id from rio_test_items order by id" :: String)
        result <- runWithLayer conn program
        expectRight [ 10, 20 ] result

    describe "prepared statements" do
      it "queryPrepared / execPrepared round-trip with a named plan" do
        let
          program :: RIO (postgres :: Postgres) DbErr (Array (Int /\ String))
          program = do
            resetTable
            _ <- execPrepared dbTag "rio_insert"
              "insert into rio_test_items (id, label) values ($1, $2)"
              (1 /\ "alpha")
            _ <- execPrepared dbTag "rio_insert"
              "insert into rio_test_items (id, label) values ($1, $2)"
              (2 /\ "beta")
            queryPrepared dbTag "rio_select_by_id"
              "select id, label from rio_test_items where id = $1"
              1
        result <- runWithLayer conn program
        expectRight [ 1 /\ "alpha" ] result

      it "queryPreparedUsing reuses the cached plan inside withTransaction" do
        let
          -- Two calls with the same statement name on the same
          -- client: Postgres caches the parse the first time and
          -- reuses it the second. Functional behavior matches a
          -- non-prepared call; this exercises the same-connection
          -- code path.
          program :: RIO (postgres :: Postgres) DbErr (Array Int)
          program = do
            resetTable
            _ <- exec dbTag
              ( "insert into rio_test_items values (1, 'a'), (2, 'b'), (3, 'c')"
                  :: String
              )
            withTransaction dbTag \client -> do
              first <- queryPreparedUsing dbTag "rio_select_one"
                "select id from rio_test_items where id = $1"
                1
                client
              second <- queryPreparedUsing dbTag "rio_select_one"
                "select id from rio_test_items where id = $1"
                3
                client
              pure (first <> second)
        result <- runWithLayer conn program
        expectRight [ 1, 3 ] result

      it "execPreparedUsing reports affected-row counts" do
        let
          program :: RIO (postgres :: Postgres) DbErr Int
          program = do
            resetTable
            _ <- exec dbTag
              ( "insert into rio_test_items values (1, 'a'), (2, 'b'), (3, 'c')"
                  :: String
              )
            withTransaction dbTag \client ->
              execPreparedUsing dbTag "rio_delete_by_id"
                "delete from rio_test_items where id = $1"
                2
                client
        result <- runWithLayer conn program
        expectRight 1 result

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
