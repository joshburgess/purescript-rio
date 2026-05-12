module Test.RIO.EffectAndFailSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Aff (delay, Milliseconds(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions as Assertions
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO)

spec :: Spec Unit
spec = do
  describe "RIO.Core lifts and failures (Phase 1.3)" do
    describe "MonadEffect" do
      it "liftEffect runs the action and yields its result" do
        ref <- liftEffect (Ref.new 0)
        let
          program :: RIO () () Int
          program = do
            liftEffect (Ref.write 7 ref)
            liftEffect (Ref.read ref)
        result <- runRIO program
        result `shouldEqual` Right 7
        observed <- liftEffect (Ref.read ref)
        observed `shouldEqual` 7

    describe "MonadAff" do
      it "liftAff suspends in Aff and resumes with the value" do
        let
          program :: RIO () () Int
          program = do
            _ <- liftAff (delay (Milliseconds 1.0))
            pure 99
        result <- runRIO program
        result `shouldEqual` Right 99

    describe "fail" do
      it "produces a Left tagged with the symbol and payload" do
        let
          program :: RIO () (notFound :: { id :: Int }) Int
          program = fail (Proxy :: Proxy "notFound") { id: 42 }
        result <- runRIO program
        case result of
          Left v ->
            Variant.case_
              # Variant.on (Proxy :: Proxy "notFound") (\p -> p.id `shouldEqual` 42)
              $ v
          Right _ -> Assertions.fail "expected Left, got Right"

      it "short-circuits any subsequent binds" do
        ref <- liftEffect (Ref.new false)
        let
          program :: RIO () (boom :: Unit) Int
          program = do
            _ <- fail (Proxy :: Proxy "boom") unit
            liftEffect (Ref.write true ref)
            pure 1
        _ <- runRIO program
        ran <- liftEffect (Ref.read ref)
        ran `shouldEqual` false
