-- | Integration tests for `RIO.Aff.Postgres.Migrate` against a real
-- | Postgres instance.
module Test.RIO.Aff.Postgres.MigrateSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, provideLayer, runRIO)
import RIO.Aff.Postgres
  ( PgError
  , Postgres
  , exec
  , execUsing
  , pgErrorMessage
  , query
  )
import RIO.Aff.Postgres.Layer (postgresLayer)
import RIO.Aff.Postgres.Migrate (migrate, sqlMigration)

dbTag :: Proxy "db"
dbTag = Proxy

type DbErr = (db :: PgError)

runWith
  :: forall e a
   . String
  -> RIO (postgres :: Postgres) e a
  -> Aff (Either (Variant e) a)
runWith conn program =
  runRIO (provideLayer (postgresLayer { connectionString: conn }) program)

resetBookkeeping :: RIO (postgres :: Postgres) DbErr Unit
resetBookkeeping = do
  _ <- exec dbTag ("drop table if exists __rio_migrations" :: String)
  _ <- exec dbTag ("drop table if exists rio_migrate_test_a" :: String)
  _ <- exec dbTag ("drop table if exists rio_migrate_test_b" :: String)
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
  describe "RIO.Aff.Postgres.Migrate (integration)" do

    it "applies every migration on a fresh database" do
      let
        program
          :: RIO (postgres :: Postgres) DbErr
               (Array Int /\ Array (Int /\ Int))
        program = do
          resetBookkeeping
          migrate dbTag $ Map.fromFoldable
            [ 1 /\ sqlMigration dbTag
                "create table rio_migrate_test_a \
                \(id serial primary key, label text not null)"
            , 2 /\ sqlMigration dbTag
                "create table rio_migrate_test_b \
                \(id serial primary key, label text not null)"
            ]
          applied <- query dbTag
            ( "select version from __rio_migrations order by version"
                :: String
            )
          _ <- exec dbTag
            ( "insert into rio_migrate_test_a (label) values ('a')"
                :: String
            )
          _ <- exec dbTag
            ( "insert into rio_migrate_test_b (label) values ('b')"
                :: String
            )
          counts <- query dbTag
            ( "select (select count(*)::int from rio_migrate_test_a), \
              \(select count(*)::int from rio_migrate_test_b)" :: String
            )
          pure (applied /\ counts)
      result <- runWith conn program
      expectRight ([ 1, 2 ] /\ [ 1 /\ 1 ]) result

    it "skips already-applied migrations on a second run" do
      counter <- liftEffect (Ref.new 0)
      let
        bumpAndCreate
          :: Ref Int -> String -> _
        bumpAndCreate ref ddl client = do
          liftEffect (Ref.modify_ (_ + 1) ref)
          _ <- execUsing dbTag (ddl :: String) client
          pure unit

        migrations =
          Map.fromFoldable
            [ 1 /\ bumpAndCreate counter
                "create table rio_migrate_test_a \
                \(id serial primary key, label text not null)"
            , 2 /\ bumpAndCreate counter
                "create table rio_migrate_test_b \
                \(id serial primary key, label text not null)"
            ]

        program :: RIO (postgres :: Postgres) DbErr Int
        program = do
          resetBookkeeping
          migrate dbTag migrations
          migrate dbTag migrations
          liftEffect (Ref.read counter)
      result <- runWith conn program
      -- The bump-counter side-effect runs once per *applied*
      -- migration. Two migrations, two runs ⇒ counter == 2.
      expectRight 2 result

    it "applies only the missing version when a partial set was applied" do
      let
        firstBatch =
          Map.fromFoldable
            [ 1 /\ sqlMigration dbTag
                "create table rio_migrate_test_a \
                \(id serial primary key)"
            ]
        bothBatches =
          Map.fromFoldable
            [ 1 /\ sqlMigration dbTag
                "create table rio_migrate_test_a \
                \(id serial primary key)"
            , 2 /\ sqlMigration dbTag
                "create table rio_migrate_test_b \
                \(id serial primary key)"
            ]

        program :: RIO (postgres :: Postgres) DbErr (Array Int)
        program = do
          resetBookkeeping
          migrate dbTag firstBatch
          migrate dbTag bothBatches
          query dbTag
            ( "select version from __rio_migrations order by version"
                :: String
            )
      result <- runWith conn program
      expectRight [ 1, 2 ] result
