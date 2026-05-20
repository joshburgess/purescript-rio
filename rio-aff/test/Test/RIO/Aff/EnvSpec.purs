module Test.RIO.Aff.EnvSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, asks, provide, provideAll, runRIO, runRIO', unsafeRunRIO)

-- A tiny "Logger" record; we don't care about the actual logging behaviour
-- here, only that the row carries it.
type Logger = { name :: String }

-- A tiny "Config" record exercising `asks` projection.
type Config = { port :: Int, host :: String }

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Env (Phase 2.1)" do
    describe "ask" do
      it "reads a single service out of the environment" do
        let
          program :: RIO (logger :: Logger) () String
          program = do
            logger <- ask (Proxy :: Proxy "logger")
            pure logger.name
          env = { logger: { name: "test-logger" } }
        result <- unsafeRunRIO program env
        result `shouldEqual` Right "test-logger"

      it "leaves the inferred row open for additional services" do
        -- This test passes by virtue of the type-checker: we hand a
        -- two-field record to a program that only requires one, and
        -- the row machinery accepts it.
        let
          program :: forall r. RIO (logger :: Logger | r) () String
          program = do
            logger <- ask (Proxy :: Proxy "logger")
            pure logger.name
          env = { logger: { name: "L" }, other: 7 }
        result <- unsafeRunRIO program env
        result `shouldEqual` Right "L"

    describe "asks" do
      it "projects a value out of a service in one step" do
        let
          program :: RIO (config :: Config) () Int
          program = asks (Proxy :: Proxy "config") _.port
          env = { config: { port: 8080, host: "localhost" } }
        result <- unsafeRunRIO program env
        result `shouldEqual` Right 8080

    describe "provide (Phase 2.2)" do
      it "supplies a service and shrinks the required row" do
        -- The inner program asks for `logger`; after `provide`-ing one,
        -- the resulting computation needs nothing from its environment.
        let
          inner :: forall r. RIO (logger :: Logger | r) () String
          inner = do
            l <- ask (Proxy :: Proxy "logger")
            pure l.name

          outer :: RIO () () String
          outer = provide (Proxy :: Proxy "logger") { name: "supplied" } inner
        result <- runRIO outer
        result `shouldEqual` Right "supplied"

      it "can be partially applied to peel one service off a multi-service row" do
        -- After providing `logger`, the resulting computation still
        -- requires `config`. The type-level shrinkage is what we are
        -- asserting; the runtime check is just confirmation.
        let
          inner :: RIO (logger :: Logger, config :: Config) () String
          inner = do
            l <- ask (Proxy :: Proxy "logger")
            c <- ask (Proxy :: Proxy "config")
            pure (l.name <> "@" <> show c.port)

          step1 :: RIO (config :: Config) () String
          step1 = provide (Proxy :: Proxy "logger") { name: "L" } inner
          env = { config: { port: 9000, host: "h" } }
        result <- unsafeRunRIO step1 env
        result `shouldEqual` Right "L@9000"

    describe "provideAll (Phase 2.3)" do
      it "discharges the full required row in one step" do
        let
          inner :: RIO (logger :: Logger, config :: Config) () String
          inner = do
            l <- ask (Proxy :: Proxy "logger")
            c <- ask (Proxy :: Proxy "config")
            pure (l.name <> ":" <> c.host <> ":" <> show c.port)

          runnable :: RIO () () String
          runnable = provideAll
            { logger: { name: "all" }
            , config: { port: 1234, host: "all.host" }
            }
            inner
        result <- runRIO runnable
        result `shouldEqual` Right "all:all.host:1234"

      it "produces a RIO () () a that runRIO' can run directly" do
        let
          inner :: RIO (logger :: Logger) () String
          inner = asks (Proxy :: Proxy "logger") _.name

          runnable :: RIO () () String
          runnable = provideAll { logger: { name: "direct" } } inner
        result <- runRIO' runnable
        result `shouldEqual` "direct"

    describe "row inference (Phase 2.5)" do
      it "composes two services from disjoint asks" do
        -- Inference test: two `ask`s for different keys aggregate their
        -- requirements into a single row covering both. Only the error
        -- row is annotated (so `shouldEqual`'s `Show` constraint can be
        -- discharged); the environment row is left to inference and
        -- comes from the call to `unsafeRunRIO`.
        let
          program :: RIO (logger :: Logger, config :: Config) () String
          program = do
            logger <- ask (Proxy :: Proxy "logger")
            cfg <- ask (Proxy :: Proxy "config")
            pure (logger.name <> "@" <> show cfg.port)
          env = { logger: { name: "L" }, config: { port: 80, host: "h" } }
        result <- unsafeRunRIO program env
        result `shouldEqual` Right "L@80"

      it "infers the row from body alone with no top-level annotation" do
        -- This test passes purely by virtue of the type-checker. The
        -- `programNoSig` definition below has no top-level signature;
        -- its row is inferred from the body. The call site supplies a
        -- concrete environment that must match the inferred row, so a
        -- regression in row inference would surface as a build failure.
        result <- unsafeRunRIO programNoSig
          { logger: { name: "L2" }
          , config: { port: 81, host: "h" }
          }
        result `shouldEqual` Right "L2@81"

-- The "body-only inference" fixture. The body uses two disjoint `ask`s;
-- the signature only pins `e` to `()` so that `shouldEqual`'s `Show`
-- constraint at the call site can be discharged. The environment row
-- is *not* nailed down by an annotation that lists `logger` and `config`
-- explicitly; it's stated as a row variable `r` that the call-site env
-- closes. This catches a regression where inference would otherwise
-- demand an explicit row at the binding site.
programNoSig :: forall r. RIO (logger :: Logger, config :: Config | r) () String
programNoSig = do
  logger <- ask (Proxy :: Proxy "logger")
  cfg <- ask (Proxy :: Proxy "config")
  pure (logger.name <> "@" <> show cfg.port)
