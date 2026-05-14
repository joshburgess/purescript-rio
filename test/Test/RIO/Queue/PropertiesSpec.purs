module Test.RIO.Queue.PropertiesSpec (spec) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Queue (offer, poll, take, unbounded)

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Queue (property tests)" do
  -- The unit pin for FIFO order fixes the input to `[1, 2, 3]`.
  -- Generalize so a regression that only manifests on empty,
  -- singleton, or longer arrays is still caught.
  it "unbounded: offer-then-take across all elements is FIFO" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      q <- liftEffect unbounded
      let
        program :: RIO () () (Array (Maybe Int))
        program = do
          for_ xs \x -> offer q x
          sequence (map (\_ -> take q) xs)
      received <- runRIO' program
      received `shouldEqual` map Just xs

  it "unbounded: poll on an empty queue returns Nothing regardless of prior state" do
    -- Pin that `poll` reports `Nothing` once the queue has been
    -- fully drained, no matter how many `offer`/`take` rounds
    -- preceded the drain. A regression that left a sentinel
    -- behind after a drain would surface here.
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      q <- liftEffect unbounded
      let
        program :: RIO () () (Maybe Int)
        program = do
          for_ xs \x -> offer q x
          for_ xs \_ -> take q
          poll q
      r <- runRIO' program
      r `shouldEqual` Nothing
