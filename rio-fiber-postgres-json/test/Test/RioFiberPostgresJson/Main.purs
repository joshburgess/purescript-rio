-- | Integration tests for `rio-fiber-postgres-json`. Driven by the
-- | same `PG_CONNECTION_STRING` env var as `rio-fiber-postgres`.
module Test.RioFiberPostgresJson.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Process (lookupEnv)
import Test.Spec (pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Fiber.Postgres.JsonSpec as JsonSpec

main :: Effect Unit
main = do
  mConn <- lookupEnv "PG_CONNECTION_STRING"
  runSpecAndExitProcess [ consoleReporter ] case mConn of
    Just conn -> JsonSpec.spec conn
    Nothing -> pending
      "rio-fiber-postgres-json integration tests: set PG_CONNECTION_STRING to run"
