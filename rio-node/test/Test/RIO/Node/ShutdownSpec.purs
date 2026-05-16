module Test.RIO.Node.ShutdownSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing)
import Effect.AVar (empty, tryPut) as EAVar
import Effect.Aff (Milliseconds(..), bracket, delay, forkAff)
import Effect.Aff.AVar (take) as AVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Node.Shutdown (withShutdownOn)

spec :: Spec Unit
spec = describe "RIO.Node.Shutdown" do
  describe "withShutdownOn" do
    it "returns Just a when the program completes before the trigger fires" do
      trigger <- liftEffect EAVar.empty
      result <- withShutdownOn (AVar.take trigger) do
        delay (Milliseconds 5.0)
        pure 42
      result `shouldEqual` Just 42

    it "returns Nothing when the trigger fires before the program completes" do
      trigger <- liftEffect EAVar.empty
      _ <- forkAff do
        delay (Milliseconds 10.0)
        liftEffect (void (EAVar.tryPut unit trigger))
      result <- withShutdownOn (AVar.take trigger) do
        delay (Milliseconds 100.0)
        pure 99
      result `shouldSatisfy` isNothing

    it "runs cleanup on the program path when the trigger wins" do
      -- Pin the canonical use case: a bracket whose release runs
      -- because the body is killed by the race, not because the
      -- body completes successfully. The release writes a flag
      -- the test then asserts on.
      trigger <- liftEffect EAVar.empty
      released <- liftEffect (Ref.new false)
      _ <- forkAff do
        delay (Milliseconds 10.0)
        liftEffect (void (EAVar.tryPut unit trigger))
      result <- withShutdownOn (AVar.take trigger) do
        bracket
          (pure unit)
          (\_ -> liftEffect (Ref.write true released))
          (\_ -> delay (Milliseconds 200.0))
      result `shouldSatisfy` isNothing
      ran <- liftEffect (Ref.read released)
      ran `shouldEqual` true

    it "preserves the program's success result even if the trigger never fires" do
      trigger <- liftEffect EAVar.empty
      result <- withShutdownOn (AVar.take trigger) (pure "ok")
      result `shouldEqual` Just "ok"
      result `shouldSatisfy` isJust
