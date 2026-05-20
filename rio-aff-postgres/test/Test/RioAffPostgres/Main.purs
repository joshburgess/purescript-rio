-- | Integration tests for `rio-postgres`.
-- |
-- | Reads the target connection string from the
-- | `PG_CONNECTION_STRING` env var. The repo's `docker-compose.yml`
-- | and the CI Postgres service-container job both expose
-- | `postgres://rio:rio@localhost:5432/rio_test`; export that
-- | value (or run via `docker compose up -d postgres`) before
-- | invoking `npx spago test -p rio-postgres`.
-- |
-- | If `PG_CONNECTION_STRING` is unset the suite is skipped
-- | (printed as a pending case) so contributors who don't have
-- | Postgres handy can still run the rest of the workspace.
module Test.RioPostgres.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Process (lookupEnv)
import Test.Spec (pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Aff.Postgres.NotifySpec as NotifySpec
import Test.RIO.Aff.Postgres.PoolSpec as PoolSpec
import Test.RIO.Aff.PostgresSpec as PostgresSpec

main :: Effect Unit
main = do
  mConn <- lookupEnv "PG_CONNECTION_STRING"
  runSpecAndExitProcess [ consoleReporter ] case mConn of
    Just conn -> do
      PostgresSpec.spec conn
      NotifySpec.spec conn
      PoolSpec.spec conn
    Nothing -> pending
      "rio-postgres integration tests: set PG_CONNECTION_STRING (e.g. via `docker compose up -d postgres`) to run"
