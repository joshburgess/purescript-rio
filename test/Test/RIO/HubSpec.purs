module Test.RIO.HubSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Hub (make, publish, subscribe, subscriberCount)
import RIO.Queue (poll, take)

spec :: Spec Unit
spec = do
  describe "RIO.Hub" do

    it "publishes to every current subscriber" do
      hub <- liftEffect (make :: _ (_ Int))
      let
        program :: RIO () () { a :: Maybe Int, b :: Maybe Int }
        program = do
          subA <- subscribe hub
          subB <- subscribe hub
          publish hub 7
          a <- take subA.queue
          b <- take subB.queue
          pure { a, b }
      r <- runRIO' program
      r.a `shouldEqual` Just 7
      r.b `shouldEqual` Just 7

    it "values published before a subscribe are not retroactively delivered" do
      hub <- liftEffect (make :: _ (_ Int))
      let
        program :: RIO () () (Maybe Int)
        program = do
          publish hub 1
          sub <- subscribe hub
          publish hub 2
          take sub.queue
      r <- runRIO' program
      r `shouldEqual` Just 2

    it "unsubscribe removes the subscriber" do
      hub <- liftEffect (make :: _ (_ Int))
      let
        program :: RIO () () { remaining :: Int, drained :: Maybe Int }
        program = do
          sub <- subscribe hub
          sub.unsubscribe
          publish hub 99
          remaining <- liftEffect (subscriberCount hub)
          drained <- poll sub.queue
          pure { remaining, drained }
      r <- runRIO' program
      r.remaining `shouldEqual` 0
      r.drained `shouldEqual` Nothing

    it "subscriberCount tracks add and remove" do
      hub <- liftEffect (make :: _ (_ Int))
      let
        program :: RIO () () { afterAdd :: Int, afterRemove :: Int }
        program = do
          s1 <- subscribe hub
          _ <- subscribe hub
          afterAdd <- liftEffect (subscriberCount hub)
          s1.unsubscribe
          afterRemove <- liftEffect (subscriberCount hub)
          pure { afterAdd, afterRemove }
      r <- runRIO' program
      r.afterAdd `shouldEqual` 2
      r.afterRemove `shouldEqual` 1
