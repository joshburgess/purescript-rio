module Test.RIO.Aff.Queue.PropertiesSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Foldable (and, for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Queue (offer, poll, size, take, unbounded)

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Aff.Queue (property tests)" do
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

  it "unbounded: size after N offers equals N (and 0 after full drain)" do
    -- The unit pin checks `size` after one offer / one take. Pin
    -- the linear relationship across arbitrary batches so a
    -- regression that, e.g., decremented size on offer or failed
    -- to decrement on take would be caught.
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      q <- liftEffect (unbounded :: _ (_ Int))
      for_ xs \x -> do
        _ <- runRIO' (offer q x)
        pure unit
      afterOffer <- liftEffect (size q)
      for_ xs \_ -> do
        _ <- runRIO' (take q)
        pure unit
      afterDrain <- liftEffect (size q)
      afterOffer `shouldEqual` Array.length xs
      afterDrain `shouldEqual` 0

  it "unbounded: offer always returns true" do
    -- Docstring promise: "`offer` ... always non-blocking on
    -- unbounded queues". The non-blocking half is observable
    -- through the `Boolean` return: `false` means "rejected"
    -- (shutdown), `true` means "accepted". On a live unbounded
    -- queue, every offer must report `true`. A regression that
    -- spuriously rejected an offer on an unbounded queue would
    -- surface here, regardless of the size of the input batch.
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      q <- liftEffect (unbounded :: _ (_ Int))
      let
        program :: RIO () () (Array Boolean)
        program = traverse (\x -> offer q x) xs
      results <- runRIO' program
      and results `shouldEqual` true
