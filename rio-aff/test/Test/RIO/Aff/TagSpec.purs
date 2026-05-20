module Test.RIO.Aff.TagSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, unsafeRunRIO)
import RIO.Aff.Tag (Tag, askT, asksT, labelOf, provideT, tag)

type Logger = { name :: String }

type Config = { port :: Int, host :: String }

loggerTag :: Tag "logger" Logger
loggerTag = tag

configTag :: Tag "config" Config
configTag = tag

spec :: Spec Unit
spec = describe "RIO.Aff.Tag" do

  describe "tag / labelOf" do
    it "reflects the symbol parameter as a string" do
      labelOf loggerTag `shouldEqual` "logger"
      labelOf configTag `shouldEqual` "config"

    it "is purely a label; two tags with the same name share that label" do
      let other = tag :: Tag "logger" Logger
      labelOf other `shouldEqual` labelOf loggerTag

  describe "askT" do
    it "reads the service named by the tag out of the environment" do
      let
        program :: RIO (logger :: Logger) () String
        program = do
          l <- askT loggerTag
          pure l.name
        env = { logger: { name: "tagged" } }
      result <- unsafeRunRIO program env
      result `shouldEqual` Right "tagged"

    it "infers an open row so additional services can sit alongside" do
      let
        program :: forall r. RIO (logger :: Logger | r) () String
        program = do
          l <- askT loggerTag
          pure l.name
        env = { logger: { name: "L" }, other: 99 }
      result <- unsafeRunRIO program env
      result `shouldEqual` Right "L"

  describe "asksT" do
    it "reads and projects in one step" do
      let
        program :: RIO (config :: Config) () Int
        program = asksT configTag _.port
        env = { config: { port: 7777, host: "localhost" } }
      result <- unsafeRunRIO program env
      result `shouldEqual` Right 7777

  describe "provideT" do
    it "supplies a service and shrinks the required row" do
      let
        inner :: forall r. RIO (logger :: Logger | r) () String
        inner = do
          l <- askT loggerTag
          pure l.name

        supplied :: RIO () () String
        supplied = provideT loggerTag { name: "provided" } inner
      result <- unsafeRunRIO supplied {}
      result `shouldEqual` Right "provided"

    it "can supply multiple tagged services by chaining" do
      let
        inner :: RIO (logger :: Logger, config :: Config) () String
        inner = do
          l <- askT loggerTag
          p <- asksT configTag _.port
          pure (l.name <> ":" <> show p)

        supplied :: RIO () () String
        supplied =
          provideT loggerTag { name: "svc" }
            (provideT configTag { port: 123, host: "h" } inner)
      result <- unsafeRunRIO supplied {}
      result `shouldEqual` Right "svc:123"
