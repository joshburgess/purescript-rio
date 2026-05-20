module Test.RIO.Fiber.Queue.PropertiesSpec (spec) where

import Prelude

import Data.Array (length, range, sort) as Array
import Data.Foldable (and, for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Aff (runAffThrow)
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

forAll :: forall a. Int -> Gen a -> (a -> Aff Unit) -> Aff Unit
forAll n gen prop = do
  samples <- liftEffect (randomSample' n gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "rio-fiber: Queue (property tests)" do
  -- The unit pin "preserves FIFO order" fixes a 3-element input.
  -- Generalise so a regression that only manifests on empty,
  -- singleton, or longer arrays is still caught.
  it "offer-then-take across all elements is FIFO" do
    forAll 20 (arbitrary :: Gen (Array Int)) \xs -> do
      let
        prog :: RIO () () (Array Int)
        prog = do
          q <- F.liftEffect (Q.make 64 :: _ (Queue Int))
          for_ xs (Q.offer q)
          sequence (map (\_ -> Q.take q) xs)
      received <- runAffThrow prog
      received `shouldEqual` xs

  it "tryTake on an empty queue returns Nothing regardless of prior state" do
    -- Pin that `tryTake` reports `Nothing` once the queue has been
    -- fully drained, no matter how many offer / take rounds
    -- preceded the drain. A regression that left a sentinel
    -- behind after a drain would surface here.
    forAll 20 (arbitrary :: Gen (Array Int)) \xs -> do
      let
        prog :: RIO () () (Maybe Int)
        prog = do
          q <- F.liftEffect (Q.make 64 :: _ (Queue Int))
          for_ xs (Q.offer q)
          _ <- traverse (\_ -> Q.take q) xs
          Q.tryTake q
      r <- runAffThrow prog
      r `shouldEqual` Nothing

  -- The unit pin "tryOffer returns false on a full queue with no
  -- takers" only fires a single overflow. Pin the linear contract:
  -- after `n` offers into a queue with capacity `>= n` every
  -- `tryOffer` accepts; one more is rejected.
  it "tryOffer accepts up to capacity and then rejects" do
    forAll 20 (chooseInt 1 16) \cap -> do
      let
        prog :: RIO () () { accepted :: Array Boolean, overflow :: Boolean }
        prog = do
          q <- F.liftEffect (Q.make cap :: _ (Queue Int))
          accepted <- traverse (Q.tryOffer q) (Array.range 1 cap)
          overflow <- Q.tryOffer q 999
          pure { accepted, overflow }
      r <- runAffThrow prog
      and r.accepted `shouldEqual` true
      Array.length r.accepted `shouldEqual` cap
      r.overflow `shouldEqual` false

  -- `size` returns the number of items currently buffered, not
  -- including ones a waiting offerer is still holding. Pin the
  -- linear relationship across arbitrary batches: after N offers
  -- (with capacity >= N) size = N; after N takes size = 0.
  it "size after N offers equals N (and 0 after full drain)" do
    forAll 20 (arbitrary :: Gen (Array Int)) \xs -> do
      let
        cap = max 1 (Array.length xs)
        prog :: RIO () () { afterOffer :: Int, afterDrain :: Int }
        prog = do
          q <- F.liftEffect (Q.make cap :: _ (Queue Int))
          for_ xs (Q.offer q)
          afterOffer <- Q.size q
          _ <- traverse (\_ -> Q.take q) xs
          afterDrain <- Q.size q
          pure { afterOffer, afterDrain }
      r <- runAffThrow prog
      r.afterOffer `shouldEqual` Array.length xs
      r.afterDrain `shouldEqual` 0

  -- Conservation under contention: many concurrent offerers
  -- push into a small capacity queue; a single consumer drains
  -- the expected count. The multiset of seen items equals the
  -- multiset of produced items. (FIFO is not asserted across
  -- different producers since their interleaving is arbitrary.)
  it "multi-producer single-consumer conserves the multiset" do
    forAll 6 (chooseInt 1 4) \cap -> do
      let
        producers = 4
        perProducer = 6
        total = producers * perProducer

        expected :: Array Int
        expected = Array.sort do
          p <- Array.range 1 producers
          i <- Array.range 1 perProducer
          pure (p * 100 + i)

        prog :: RIO () () (Array Int)
        prog = do
          q <- F.liftEffect (Q.make cap :: _ (Queue Int))
          ref <- F.liftEffect (Ref.new ([] :: Array Int))
          let
            produce :: Int -> RIO () () Unit
            produce p = for_ (Array.range 1 perProducer) \i ->
              Q.offer q (p * 100 + i)

            consume :: RIO () () Unit
            consume = for_ (Array.range 1 total) \_ -> do
              x <- Q.take q
              F.liftEffect (Ref.modify_ (\xs -> xs <> [ x ]) ref)

            actions :: Array (RIO () () Unit)
            actions = [ consume ] <> map produce (Array.range 1 producers)
          _ <- F.parTraverse identity actions
          F.liftEffect (Ref.read ref)
      seen <- runAffThrow prog
      Array.sort seen `shouldEqual` expected

  -- Capacity invariant under contention: the buffered count
  -- observed by `size` after every take must never exceed the
  -- configured capacity. A regression that admitted an offer past
  -- capacity (e.g. losing the offerer queue under contention)
  -- would manifest here.
  it "size never exceeds capacity under contention" do
    forAll 6 (chooseInt 1 4) \cap -> do
      let
        producers = 4
        perProducer = 6
        total = producers * perProducer

        prog :: RIO () () Boolean
        prog = do
          q <- F.liftEffect (Q.make cap :: _ (Queue Int))
          ok <- F.liftEffect (Ref.new true)
          let
            produce :: Int -> RIO () () Unit
            produce p = for_ (Array.range 1 perProducer) \i ->
              Q.offer q (p * 100 + i)

            consume :: RIO () () Unit
            consume = for_ (Array.range 1 total) \_ -> do
              _ <- Q.take q
              n <- Q.size q
              F.liftEffect do
                cur <- Ref.read ok
                Ref.write (cur && n <= cap) ok

            actions :: Array (RIO () () Unit)
            actions = [ consume ] <> map produce (Array.range 1 producers)
          _ <- F.parTraverse identity actions
          F.liftEffect (Ref.read ok)
      r <- runAffThrow prog
      r `shouldEqual` true
