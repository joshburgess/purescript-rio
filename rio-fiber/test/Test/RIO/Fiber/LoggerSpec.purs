module Test.RIO.Fiber.LoggerSpec (spec) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Logger (LogLevel(..), Logger(..))
import RIO.Fiber.Logger as Logger
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- | A capture logger: every emitted (level, message) pair is appended
-- | to a `Ref`.
mkCaptureLogger
  :: Effect (Tuple Logger (Effect (Array (Tuple LogLevel String))))
mkCaptureLogger = do
  ref <- Ref.new []
  let
    l = Logger
      { emit: \lvl msg ->
          Ref.modify_ (\xs -> xs <> [ Tuple lvl msg ]) ref
      }
  pure (Tuple l (Ref.read ref))

-- | A filtering logger: only emits at >= `min`. Forwards to `inner`.
mkLevelFilter :: LogLevel -> Logger -> Logger
mkLevelFilter min (Logger inner) = Logger
  { emit: \lvl msg ->
      if lvl >= min then inner.emit lvl msg
      else pure unit
  }

spec :: Spec Unit
spec = describe "rio-fiber: Logger" do
  describe "capture" do
    it "emits records each message at its level" do
      Tuple capture readBack <- liftEffect mkCaptureLogger
      let
        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture do
          Logger.debug "d-msg"
          Logger.info "i-msg"
          Logger.warn "w-msg"
          Logger.error "e-msg"
      out <- runAff prog {}
      case out of
        Success _ -> do
          msgs <- liftEffect readBack
          msgs `shouldEqual`
            [ Tuple Debug "d-msg"
            , Tuple Info "i-msg"
            , Tuple Warn "w-msg"
            , Tuple Error "e-msg"
            ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "log defaults to Info level" do
      Tuple capture readBack <- liftEffect mkCaptureLogger
      let
        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture (Logger.log "plain")
      out <- runAff prog {}
      case out of
        Success _ -> do
          msgs <- liftEffect readBack
          msgs `shouldEqual` [ Tuple Info "plain" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "logAt allows arbitrary level" do
      Tuple capture readBack <- liftEffect mkCaptureLogger
      let
        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture do
          Logger.logAt Warn "explicit-warn"
      out <- runAff prog {}
      case out of
        Success _ -> do
          msgs <- liftEffect readBack
          msgs `shouldEqual` [ Tuple Warn "explicit-warn" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "withLogger scoping" do
    it "withLogger restores the previous logger after exit" do
      Tuple cap1 read1 <- liftEffect mkCaptureLogger
      Tuple cap2 read2 <- liftEffect mkCaptureLogger
      let
        prog :: F.RIO () () Unit
        prog = Logger.withLogger cap1 do
          Logger.info "outer-before"
          Logger.withLogger cap2 (Logger.info "inner")
          Logger.info "outer-after"
      out <- runAff prog {}
      case out of
        Success _ -> do
          xs1 <- liftEffect read1
          xs2 <- liftEffect read2
          xs1 `shouldEqual`
            [ Tuple Info "outer-before"
            , Tuple Info "outer-after"
            ]
          xs2 `shouldEqual` [ Tuple Info "inner" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "child fibers inherit the active logger" do
      Tuple capture readBack <- liftEffect mkCaptureLogger
      let
        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture do
          f <- F.fork (Logger.info "from-child")
          _ <- F.join f
          Logger.info "from-parent"
      out <- runAff prog {}
      case out of
        Success _ -> do
          msgs <- liftEffect readBack
          msgs `shouldEqual`
            [ Tuple Info "from-child"
            , Tuple Info "from-parent"
            ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "filtering" do
    it "level filter suppresses messages below the threshold" do
      Tuple capture readBack <- liftEffect mkCaptureLogger
      let
        filtered = mkLevelFilter Warn capture

        prog :: F.RIO () () Unit
        prog = Logger.withLogger filtered do
          Logger.debug "skipped-d"
          Logger.info "skipped-i"
          Logger.warn "kept-w"
          Logger.error "kept-e"
      out <- runAff prog {}
      case out of
        Success _ -> do
          msgs <- liftEffect readBack
          msgs `shouldEqual`
            [ Tuple Warn "kept-w"
            , Tuple Error "kept-e"
            ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "LogLevel" do
    it "Ord agrees with severity order" do
      (Debug < Info) `shouldEqual` true
      (Info < Warn) `shouldEqual` true
      (Warn < Error) `shouldEqual` true

    it "Show renders the canonical names" do
      show Debug `shouldEqual` "DEBUG"
      show Info `shouldEqual` "INFO"
      show Warn `shouldEqual` "WARN"
      show Error `shouldEqual` "ERROR"

  describe "annotateLogs" do
    it "prepends key=value annotations to each emitted message" do
      Tuple capture readBack <- liftEffect mkCaptureLogger
      let
        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture do
          Logger.annotateLogs
            [ Tuple "request" "abc", Tuple "user" "42" ]
            (Logger.info "hello")
      out <- runAff prog {}
      case out of
        Success _ -> do
          msgs <- liftEffect readBack
          msgs `shouldEqual`
            [ Tuple Info "request=abc user=42 hello" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "nests by appending inner annotations to the outer set" do
      Tuple capture readBack <- liftEffect mkCaptureLogger
      let
        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture do
          Logger.annotateLogs [ Tuple "outer" "1" ] do
            Logger.info "outer-only"
            Logger.annotateLogs [ Tuple "inner" "2" ] do
              Logger.info "both"
            Logger.info "outer-only-again"
      out <- runAff prog {}
      case out of
        Success _ -> do
          msgs <- liftEffect readBack
          msgs `shouldEqual`
            [ Tuple Info "outer=1 outer-only"
            , Tuple Info "outer=1 inner=2 both"
            , Tuple Info "outer=1 outer-only-again"
            ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "empty annotations array is the identity" do
      Tuple capture readBack <- liftEffect mkCaptureLogger
      let
        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture
          (Logger.annotateLogs [] (Logger.info "plain"))
      out <- runAff prog {}
      case out of
        Success _ -> do
          msgs <- liftEffect readBack
          msgs `shouldEqual` [ Tuple Info "plain" ]
        other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
