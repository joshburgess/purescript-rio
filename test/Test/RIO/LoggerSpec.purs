module Test.RIO.LoggerSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, catchTag, fail, provideAll, runRIO)
import RIO.Logger
  ( LogLevel(..)
  , Logger
  , logDebug
  , logError
  , logInfo
  , logTrace
  , logWarn
  , noopLogger
  , withField
  , withFields
  )
import RIO.Test.Logger (newRecordingLogger)

spec :: Spec Unit
spec = describe "RIO.Logger" do
  describe "level smart constructors" do
    it "logTrace emits at LogTrace" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logTrace "trace-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> do
          r.level `shouldEqual` LogTrace
          r.message `shouldEqual` "trace-msg"
          r.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

    it "logDebug emits at LogDebug" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logDebug "debug-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.level `shouldEqual` LogDebug
        _ -> 1 `shouldEqual` Array.length records

    it "logInfo emits at LogInfo" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logInfo "info-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.level `shouldEqual` LogInfo
        _ -> 1 `shouldEqual` Array.length records

    it "logWarn emits at LogWarn" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logWarn "warn-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.level `shouldEqual` LogWarn
        _ -> 1 `shouldEqual` Array.length records

    it "logError emits at LogError" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logError "error-msg"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.level `shouldEqual` LogError
        _ -> 1 `shouldEqual` Array.length records

  describe "withField / withFields" do
    it "withField attaches a single field to every emission inside the block" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = withField "request.id" "abc-123" do
          logInfo "first"
          logInfo "second"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2 ] -> do
          r1.message `shouldEqual` "first"
          r1.fields `shouldEqual` [ Tuple "request.id" "abc-123" ]
          r2.message `shouldEqual` "second"
          r2.fields `shouldEqual` [ Tuple "request.id" "abc-123" ]
        _ -> 1 `shouldEqual` Array.length records

    it "withFields attaches multiple fields and preserves their input order" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = withFields
          [ Tuple "request.id" "abc"
          , Tuple "user" "alice"
          , Tuple "tenant" "acme"
          ]
          (logInfo "hello")
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] ->
          r.fields `shouldEqual`
            [ Tuple "request.id" "abc"
            , Tuple "user" "alice"
            , Tuple "tenant" "acme"
            ]
        _ -> 1 `shouldEqual` Array.length records

    it "annotation set is restored after withFields exits (success)" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          withField "scope" "inner" (logInfo "inside")
          logInfo "outside"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2 ] -> do
          r1.fields `shouldEqual` [ Tuple "scope" "inner" ]
          r2.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

    it "annotation set is restored after withFields exits on typed failure" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          _ <- catchTag (Proxy :: Proxy "boom") (\_ -> pure unit)
            ( withField "scope" "inner" do
                logInfo "before-fail"
                fail (Proxy :: Proxy "boom") unit
            )
          logInfo "after-catch"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2 ] -> do
          r1.message `shouldEqual` "before-fail"
          r1.fields `shouldEqual` [ Tuple "scope" "inner" ]
          r2.message `shouldEqual` "after-catch"
          r2.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

    it "nested withFields: inner shadows outer; outer restored on inner exit" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = withFields
          [ Tuple "request.id" "outer-id"
          , Tuple "tenant" "acme"
          ]
          do
            logInfo "at-outer"
            withField "request.id" "inner-id" (logInfo "at-inner")
            logInfo "back-at-outer"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r1, r2, r3 ] -> do
          r1.fields `shouldEqual`
            [ Tuple "request.id" "outer-id"
            , Tuple "tenant" "acme"
            ]
          r2.fields `shouldEqual`
            [ Tuple "tenant" "acme"
            , Tuple "request.id" "inner-id"
            ]
          r3.fields `shouldEqual`
            [ Tuple "request.id" "outer-id"
            , Tuple "tenant" "acme"
            ]
        _ -> 1 `shouldEqual` Array.length records

    it "fields outside any withFields block are empty" do
      rec <- liftAff newRecordingLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = logInfo "bare"
      _ <- runRIO (provideAll { logger: rec.logger } program)
      records <- liftEffect rec.snapshot
      case records of
        [ r ] -> r.fields `shouldEqual` []
        _ -> 1 `shouldEqual` Array.length records

  describe "noopLogger" do
    it "runs every emission without crashing and produces no observable output" do
      logger <- liftEffect noopLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          logTrace "trace"
          logDebug "debug"
          logInfo "info"
          logWarn "warn"
          logError "error"
      result <- runRIO (provideAll { logger } program)
      case result of
        Right _ -> pure unit
        Left _ -> 1 `shouldEqual` 0

    it "scopes annotations through withFields even with discarded emissions" do
      -- noopLogger's docstring promises that withFields still cycles
      -- annotations via internal state. We can't observe emissions, but
      -- a typed failure inside withFields must not crash because of
      -- broken annotation save/restore.
      logger <- liftEffect noopLogger
      let
        program :: RIO (logger :: Logger) () Unit
        program = do
          _ <- catchTag (Proxy :: Proxy "boom") (\_ -> pure unit)
            ( withField "scope" "inner" do
                logInfo "inside"
                fail (Proxy :: Proxy "boom") unit
            )
          logInfo "after"
      result <- runRIO (provideAll { logger } program)
      case result of
        Right _ -> pure unit
        Left _ -> 1 `shouldEqual` 0
