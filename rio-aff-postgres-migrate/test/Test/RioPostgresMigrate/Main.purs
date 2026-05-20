-- | Integration tests for `rio-postgres-migrate`. Driven by the
-- | same `PG_CONNECTION_STRING` env var as `rio-postgres`.
module Test.RioPostgresMigrate.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Process (lookupEnv)
import Test.Spec (pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Aff.Postgres.MigrateSpec as MigrateSpec

main :: Effect Unit
main = do
  mConn <- lookupEnv "PG_CONNECTION_STRING"
  runSpecAndExitProcess [ consoleReporter ] case mConn of
    Just conn -> MigrateSpec.spec conn
    Nothing -> pending
      "rio-postgres-migrate integration tests: set PG_CONNECTION_STRING to run"
