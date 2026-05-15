module Test.RIO.Schedule.EventuallySpec (spec) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO')
import RIO.Schedule (eventually)

type Errs = (boom :: Unit)

spec :: Spec Unit
spec = describe "RIO.Schedule.eventually" do

  it "returns immediately on first success" do
    counter <- liftEffect (Ref.new 0)
    let
      action :: RIO () Errs Int
      action = do
        _ <- liftEffect (Ref.modify (_ + 1) counter)
        pure 42

      program :: RIO () () Int
      program = eventually action
    result <- runRIO' program
    result `shouldEqual` 42
    attempts <- liftEffect (Ref.read counter)
    attempts `shouldEqual` 1

  it "retries until the action eventually succeeds" do
    counter <- liftEffect (Ref.new 0)
    let
      action :: RIO () Errs Int
      action = do
        n <- liftEffect (Ref.modify (_ + 1) counter)
        if n < 5 then fail (Proxy :: Proxy "boom") unit
        else pure n

      program :: RIO () () Int
      program = eventually action
    result <- runRIO' program
    result `shouldEqual` 5
    attempts <- liftEffect (Ref.read counter)
    attempts `shouldEqual` 5

  it "discharges the typed error row so the caller can pick any e'" do
    let
      action :: RIO () Errs Int
      action = pure 7

      -- The discharged row is `()`, not `Errs`, because eventually
      -- promises no typed failure can surface.
      program :: RIO () () Int
      program = eventually action
    result <- runRIO' program
    result `shouldEqual` 7
