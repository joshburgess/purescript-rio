-- | Integration tests for `rio-postgres-json`. Driven by the same
-- | `PG_CONNECTION_STRING` env var as `rio-postgres`.
module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Node.Process (lookupEnv)
import Test.Spec (pending)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Postgres.JsonSpec as JsonSpec

main :: Effect Unit
main = do
  mConn <- lookupEnv "PG_CONNECTION_STRING"
  runSpecAndExitProcess [ consoleReporter ] case mConn of
    Just conn -> JsonSpec.spec conn
    Nothing -> pending
      "rio-postgres-json integration tests: set PG_CONNECTION_STRING to run"
