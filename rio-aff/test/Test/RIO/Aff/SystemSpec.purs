module Test.RIO.Aff.SystemSpec (spec) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, provideAll, runRIO')
import RIO.Aff.System (System, getArgs, getCwd, lookupEnv)
import RIO.Aff.Test.System (newTestSystem)

spec :: Spec Unit
spec = describe "RIO.Aff.System + RIO.Aff.Test.System" do
  describe "lookupEnv" do
    it "returns the seeded value" do
      ts <- newTestSystem
        { env: Map.singleton "HOME" "/tmp"
        , args: []
        , cwd: "/"
        }
      let
        program :: RIO (system :: System) () (Maybe String)
        program = lookupEnv "HOME"
      v <- runRIO' (provideAll { system: ts.system } program)
      v `shouldEqual` Just "/tmp"

    it "returns Nothing for an unset variable" do
      ts <- newTestSystem
        { env: Map.empty, args: [], cwd: "/" }
      let
        program :: RIO (system :: System) () (Maybe String)
        program = lookupEnv "ABSENT"
      v <- runRIO' (provideAll { system: ts.system } program)
      v `shouldEqual` Nothing

    it "observes setEnv between calls" do
      ts <- newTestSystem
        { env: Map.empty, args: [], cwd: "/" }
      let
        program :: RIO (system :: System) () (Maybe String)
        program = lookupEnv "KEY"
      before <- runRIO' (provideAll { system: ts.system } program)
      liftAff (ts.setEnv "KEY" "value")
      after <- runRIO' (provideAll { system: ts.system } program)
      before `shouldEqual` Nothing
      after `shouldEqual` Just "value"

    it "observes unsetEnv between calls" do
      ts <- newTestSystem
        { env: Map.singleton "KEY" "value"
        , args: []
        , cwd: "/"
        }
      let
        program :: RIO (system :: System) () (Maybe String)
        program = lookupEnv "KEY"
      before <- runRIO' (provideAll { system: ts.system } program)
      liftAff (ts.unsetEnv "KEY")
      after <- runRIO' (provideAll { system: ts.system } program)
      before `shouldEqual` Just "value"
      after `shouldEqual` Nothing

  describe "getArgs" do
    it "returns the seeded argument vector" do
      ts <- newTestSystem
        { env: Map.empty
        , args: [ "node", "script.js", "--flag" ]
        , cwd: "/"
        }
      let
        program :: RIO (system :: System) () (Array String)
        program = getArgs
      v <- runRIO' (provideAll { system: ts.system } program)
      v `shouldEqual` [ "node", "script.js", "--flag" ]

    it "observes setArgs" do
      ts <- newTestSystem
        { env: Map.empty, args: [ "old" ], cwd: "/" }
      let
        program :: RIO (system :: System) () (Array String)
        program = getArgs
      liftAff (ts.setArgs [ "new", "args" ])
      v <- runRIO' (provideAll { system: ts.system } program)
      v `shouldEqual` [ "new", "args" ]

  describe "getCwd" do
    it "returns the seeded directory" do
      ts <- newTestSystem
        { env: Map.empty, args: [], cwd: "/home/test" }
      let
        program :: RIO (system :: System) () String
        program = getCwd
      v <- runRIO' (provideAll { system: ts.system } program)
      v `shouldEqual` "/home/test"

    it "observes setCwd" do
      ts <- newTestSystem
        { env: Map.empty, args: [], cwd: "/" }
      liftAff (ts.setCwd "/var/log")
      let
        program :: RIO (system :: System) () String
        program = getCwd
      v <- runRIO' (provideAll { system: ts.system } program)
      v `shouldEqual` "/var/log"
