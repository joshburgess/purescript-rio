module Test.RIO.Aff.Config.FileIntegrationSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Aff (Aff, attempt)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdtemp, writeTextFile)
import Node.Path (concat)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Config
  ( Config
  , ConfigError
  , Source
  , int
  , load
  , nested
  , string
  )
import RIO.Aff.Config.File (dotenvFileSource, jsonFileSource)
import RIO.Aff.Core (RIO, runRIO)

type Cfg = { port :: Int, db :: { url :: String } }

cfgDescriptor :: Config Cfg
cfgDescriptor =
  { port: _, db: _ }
    <$> int "PORT"
    <*> ({ url: _ } <$> nested "DB" (string "URL"))

cfgTag :: Proxy "config"
cfgTag = Proxy

type Err = (config :: ConfigError)

runLoad :: Source -> Aff (Either ConfigError Cfg)
runLoad src = do
  result <- runRIO (load cfgTag src cfgDescriptor :: RIO () Err Cfg)
  case result of
    Right ok -> pure (Right ok)
    Left v -> pure (Left (Variant.case_ # Variant.on cfgTag identity $ v))

spec :: Spec Unit
spec = describe "RIO.Aff.Config.File integration" do
  it "loads via dotenvFileSource" do
    dir <- mkdtemp "/tmp/rio-config-file-"
    let path = concat [ dir, "app.env" ]
    writeTextFile UTF8 path
      "PORT=8080\nDB_URL=postgres://localhost/test\n"
    src <- dotenvFileSource path
    res <- runLoad src
    res `shouldEqual`
      Right { port: 8080, db: { url: "postgres://localhost/test" } }

  it "loads via jsonFileSource with nested objects" do
    dir <- mkdtemp "/tmp/rio-config-file-"
    let path = concat [ dir, "app.json" ]
    writeTextFile UTF8 path
      """{ "PORT": 8080, "DB": { "URL": "postgres://localhost/test" } }"""
    src <- jsonFileSource path
    res <- runLoad src
    res `shouldEqual`
      Right { port: 8080, db: { url: "postgres://localhost/test" } }

  it "surfaces JSON parse errors via Aff" do
    dir <- mkdtemp "/tmp/rio-config-file-"
    let path = concat [ dir, "bad.json" ]
    writeTextFile UTF8 path "{ not json"
    outcome <- attempt (jsonFileSource path)
    case outcome of
      Left _ -> pure unit
      Right _ -> fail "expected jsonFileSource to throw on malformed JSON"
