-- | Integration tests for `RIO.Aff.Postgres.Pool` against a real
-- | Postgres instance.
module Test.RIO.Aff.Postgres.PoolSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, provideLayer, runRIO)
import RIO.Aff.Postgres (PgError, Postgres, pgErrorMessage)
import RIO.Aff.Postgres.Layer (postgresLayer)
import RIO.Aff.Postgres.Pool (PoolStats, poolStats, warmup)

dbTag :: Proxy "db"
dbTag = Proxy

type DbErr = (db :: PgError)

runWith
  :: forall e a
   . Int
  -> String
  -> RIO (postgres :: Postgres) e a
  -> Aff (Either (Variant e) a)
runWith maxClients conn program =
  runRIO
    ( provideLayer
        ( postgresLayer
            { connectionString: conn
            , max: maxClients
            }
        )
        program
    )

expectRight
  :: PoolStats
  -> Either (Variant DbErr) PoolStats
  -> Aff Unit
expectRight expected = case _ of
  Right s -> s `shouldEqual` expected
  Left v -> fail
    ( "expected Right, got typed failure: "
        <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
    )

spec :: String -> Spec Unit
spec conn = do
  describe "RIO.Aff.Postgres.Pool (integration)" do

    it "poolStats reports a fresh pool as empty" do
      let
        program :: RIO (postgres :: Postgres) DbErr PoolStats
        program = poolStats
      result <- runWith 5 conn program
      expectRight { total: 0, idle: 0, waiting: 0 } result

    it "warmup pre-connects up to n clients" do
      let
        program :: RIO (postgres :: Postgres) DbErr PoolStats
        program = do
          warmup dbTag 3
          poolStats
      result <- runWith 5 conn program
      expectRight { total: 3, idle: 3, waiting: 0 } result

    it "warmup with n <= 0 is a no-op" do
      let
        program :: RIO (postgres :: Postgres) DbErr PoolStats
        program = do
          warmup dbTag 0
          warmup dbTag (-1)
          poolStats
      result <- runWith 5 conn program
      expectRight { total: 0, idle: 0, waiting: 0 } result
