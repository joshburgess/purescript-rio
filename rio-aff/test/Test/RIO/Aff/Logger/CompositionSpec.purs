module Test.RIO.Aff.Logger.CompositionSpec (spec) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Logger (LogLevel(..), combineLoggers, filterLevel, formatJsonLine)
import RIO.Aff.Test.Logger (newRecordingLogger)

spec :: Spec Unit
spec = describe "RIO.Aff.Logger (composition)" do

  describe "combineLoggers" do
    it "tees every emission into both wrapped loggers" do
      a <- newRecordingLogger
      b <- newRecordingLogger
      let combined = combineLoggers a.logger b.logger
      liftEffect (combined.log LogInfo "hello" [ Tuple "k" "v" ])
      liftEffect (combined.log LogWarn "uh" [])
      ar <- liftEffect a.snapshot
      br <- liftEffect b.snapshot
      ar `shouldEqual`
        [ { level: LogInfo, message: "hello", fields: [ Tuple "k" "v" ] }
        , { level: LogWarn, message: "uh", fields: [] }
        ]
      br `shouldEqual`
        [ { level: LogInfo, message: "hello", fields: [ Tuple "k" "v" ] }
        , { level: LogWarn, message: "uh", fields: [] }
        ]

    it "fans setAnnotations out to both children" do
      a <- newRecordingLogger
      b <- newRecordingLogger
      let combined = combineLoggers a.logger b.logger
      liftEffect (combined.setAnnotations [ Tuple "req" "1" ])
      ans1 <- liftEffect a.logger.getAnnotations
      ans2 <- liftEffect b.logger.getAnnotations
      ans1 `shouldEqual` [ Tuple "req" "1" ]
      ans2 `shouldEqual` [ Tuple "req" "1" ]

    it "reads annotations back through the combined view" do
      a <- newRecordingLogger
      b <- newRecordingLogger
      let combined = combineLoggers a.logger b.logger
      liftEffect (combined.setAnnotations [ Tuple "k" "v" ])
      ans <- liftEffect combined.getAnnotations
      ans `shouldEqual` [ Tuple "k" "v" ]

  describe "filterLevel" do
    it "drops emissions below the minimum level" do
      r <- newRecordingLogger
      let filtered = filterLevel LogWarn r.logger
      liftEffect (filtered.log LogTrace "t" [])
      liftEffect (filtered.log LogDebug "d" [])
      liftEffect (filtered.log LogInfo "i" [])
      liftEffect (filtered.log LogWarn "w" [])
      liftEffect (filtered.log LogError "e" [])
      snap <- liftEffect r.snapshot
      map _.message snap `shouldEqual` [ "w", "e" ]

    it "keeps every emission when the floor is the lowest level" do
      r <- newRecordingLogger
      let filtered = filterLevel LogTrace r.logger
      liftEffect (filtered.log LogTrace "t" [])
      liftEffect (filtered.log LogError "e" [])
      snap <- liftEffect r.snapshot
      map _.message snap `shouldEqual` [ "t", "e" ]

    it "passes annotations through unchanged" do
      r <- newRecordingLogger
      let filtered = filterLevel LogError r.logger
      liftEffect (filtered.setAnnotations [ Tuple "k" "v" ])
      ans <- liftEffect filtered.getAnnotations
      ans `shouldEqual` [ Tuple "k" "v" ]

  describe "formatJsonLine" do
    it "renders a JSON object with level, message, and fields" do
      formatJsonLine LogInfo "hello" [ Tuple "k" "v" ]
        `shouldEqual`
          "{\"level\":\"INFO\",\"message\":\"hello\",\"fields\":{\"k\":\"v\"}}"

    it "renders an empty fields object when no annotations are present" do
      formatJsonLine LogWarn "x" []
        `shouldEqual` "{\"level\":\"WARN\",\"message\":\"x\",\"fields\":{}}"

    it "preserves field order" do
      formatJsonLine LogDebug "m"
        [ Tuple "a" "1", Tuple "b" "2", Tuple "c" "3" ]
        `shouldEqual`
          "{\"level\":\"DEBUG\",\"message\":\"m\",\"fields\":{\"a\":\"1\",\"b\":\"2\",\"c\":\"3\"}}"

    it "renders each level tag in uppercase" do
      formatJsonLine LogTrace "t" [] `shouldEqual`
        "{\"level\":\"TRACE\",\"message\":\"t\",\"fields\":{}}"
      formatJsonLine LogError "e" [] `shouldEqual`
        "{\"level\":\"ERROR\",\"message\":\"e\",\"fields\":{}}"
