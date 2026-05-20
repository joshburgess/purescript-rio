module Test.RIO.Aff.Cause.CombinatorsSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Cause
  ( Cause(..)
  , catchAllCause
  , failCause
  , foldCauseRIO
  , tapErrorCause
  )
import RIO.Aff.Core (RIO, die, fail, runRIO, runRIO', sandbox)

type Errs = (boom :: Int)

renderBoom :: Variant Errs -> String
renderBoom = Variant.case_ # Variant.on (Proxy :: Proxy "boom") show

spec :: Spec Unit
spec = describe "RIO.Aff.Cause (foldCauseRIO / catchAllCause / failCause / tapErrorCause)" do

  describe "foldCauseRIO" do
    it "routes a success through the onOk arm" do
      let
        program :: RIO () () Int
        program = foldCauseRIO
          (\_ -> pure (-1))
          (\a -> pure (a + 1))
          (pure 41 :: RIO () Errs Int)
      result <- runRIO' program
      result `shouldEqual` 42

    it "routes a typed failure through the onCause arm with Fail" do
      probe <- liftEffect (Ref.new "")
      let
        program :: RIO () () String
        program = foldCauseRIO
          ( \cause -> case cause of
              Fail v -> do
                liftEffect (Ref.write ("fail " <> renderBoom v) probe)
                pure "saw-fail"
              _ -> pure "wrong"
          )
          (\_ -> pure "ok")
          (fail (Proxy :: Proxy "boom") 7 :: RIO () Errs Int)
      result <- runRIO' program
      result `shouldEqual` "saw-fail"
      seen <- liftEffect (Ref.read probe)
      seen `shouldEqual` "fail 7"

    it "routes a defect through the onCause arm with Die" do
      probe <- liftEffect (Ref.new "")
      let
        program :: RIO () () String
        program = foldCauseRIO
          ( \cause -> case cause of
              Die err -> do
                liftEffect (Ref.write (Exception.message err) probe)
                pure "saw-defect"
              _ -> pure "wrong"
          )
          (\_ -> pure "ok")
          (die (Exception.error "kapow") :: RIO () () Int)
      result <- runRIO' program
      result `shouldEqual` "saw-defect"
      seen <- liftEffect (Ref.read probe)
      seen `shouldEqual` "kapow"

  describe "catchAllCause" do
    it "recovers from a typed failure" do
      let
        program :: RIO () () Int
        program = catchAllCause
          (\_ -> pure 99)
          (fail (Proxy :: Proxy "boom") 1 :: RIO () Errs Int)
      result <- runRIO' program
      result `shouldEqual` 99

    it "recovers from a defect (unlike catchAll)" do
      let
        program :: RIO () () Int
        program = catchAllCause
          ( \cause -> case cause of
              Die _ -> pure 77
              _ -> pure 0
          )
          (die (Exception.error "kapow") :: RIO () () Int)
      result <- runRIO' program
      result `shouldEqual` 77

    it "passes a success through untouched" do
      let
        program :: RIO () () Int
        program = catchAllCause
          (\_ -> pure 0)
          (pure 5 :: RIO () Errs Int)
      result <- runRIO' program
      result `shouldEqual` 5

  describe "failCause" do
    it "re-raises a Fail leaf on the typed row" do
      let
        program :: RIO () Errs Int
        program = failCause (Fail (Variant.inj (Proxy :: Proxy "boom") 42))
      result <- runRIO program
      case result of
        Left v -> renderBoom v `shouldEqual` "42"
        Right _ -> 1 `shouldEqual` 0

    it "re-raises a Die leaf as a defect" do
      let
        program :: RIO () () (Either _ Int)
        program = sandbox (failCause (Die (Exception.error "kapow")))
      result <- runRIO' program
      case result of
        Left err -> Exception.message err `shouldEqual` "kapow"
        Right _ -> 1 `shouldEqual` 0

    it "picks the leftmost leaf of a Parallel composite" do
      let
        left = Fail (Variant.inj (Proxy :: Proxy "boom") 1)
        right = Fail (Variant.inj (Proxy :: Proxy "boom") 2)

        program :: RIO () Errs Int
        program = failCause (Parallel left right)
      result <- runRIO program
      case result of
        Left v -> renderBoom v `shouldEqual` "1"
        Right _ -> 1 `shouldEqual` 0

  describe "tapErrorCause" do
    it "fires on a typed failure and re-raises it" do
      probe <- liftEffect (Ref.new "")
      let
        program :: RIO () Errs Int
        program = tapErrorCause
          ( \cause -> case cause of
              Fail v -> liftEffect (Ref.write ("tap " <> renderBoom v) probe)
              _ -> pure unit
          )
          (fail (Proxy :: Proxy "boom") 9)
      result <- runRIO program
      case result of
        Left v -> renderBoom v `shouldEqual` "9"
        Right _ -> 1 `shouldEqual` 0
      seen <- liftEffect (Ref.read probe)
      seen `shouldEqual` "tap 9"

    it "fires on a defect and re-raises it (observable via sandbox)" do
      probe <- liftEffect (Ref.new "")
      let
        program :: RIO () () (Either _ Int)
        program = sandbox
          ( tapErrorCause
              ( \cause -> case cause of
                  Die err ->
                    liftEffect
                      ( Ref.write ("tap " <> Exception.message err) probe
                      )
                  _ -> pure unit
              )
              (die (Exception.error "kapow"))
          )
      result <- runRIO' program
      case result of
        Left err -> Exception.message err `shouldEqual` "kapow"
        Right _ -> 1 `shouldEqual` 0
      seen <- liftEffect (Ref.read probe)
      seen `shouldEqual` "tap kapow"

    it "passes a success through without invoking the handler" do
      probe <- liftEffect (Ref.new 0)
      let
        program :: RIO () Errs Int
        program = tapErrorCause
          (\_ -> liftEffect (Ref.modify_ (_ + 1) probe))
          (pure 3)
      result <- runRIO program
      result `shouldEqual` (Right 3 :: Either _ _)
      fired <- liftEffect (Ref.read probe)
      fired `shouldEqual` 0
