module Test.RIO.Fiber.Hub.PropertiesSpec (spec) where

import Prelude

import Data.Array (length, range) as Array
import Data.Foldable (for_)
import Data.Traversable (sequence, traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import RIO.Fiber.Aff (runAffThrow)
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Hub (Hub)
import RIO.Fiber.Hub as Hub
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

forAll :: forall a. Int -> Gen a -> (a -> Aff Unit) -> Aff Unit
forAll n gen prop = do
  samples <- liftEffect (randomSample' n gen)
  for_ samples prop

-- Keep subscriber counts small so the test budget stays
-- predictable; each sample allocates `n` per-subscriber queues.
smallNat :: Gen Int
smallNat = chooseInt 0 10

spec :: Spec Unit
spec = describe "rio-fiber: Hub (property tests)" do
  -- The unit pin "fans publishes out to every subscriber" fixes
  -- two subscribers and one publish. Generalise across arbitrary
  -- batches of `Int` so a regression that only breaks on empty,
  -- singleton, or longer batches is still caught.
  it "publish delivers every value to every subscriber in publish order" do
    forAll 20 (arbitrary :: Gen (Array Int)) \xs -> do
      let
        prog :: RIO () () { a :: Array Int, b :: Array Int }
        prog = do
          -- Capacity large enough that no publish backpressures
          -- inside the test (we want to observe order, not the
          -- blocking behaviour, here).
          let cap = max 1 (Array.length xs)
          hub <- F.liftEffect (Hub.make cap :: _ (Hub Int))
          s1 <- Hub.subscribe hub
          s2 <- Hub.subscribe hub
          for_ xs (Hub.publish hub)
          a <- sequence (map (\_ -> Hub.take s1) xs)
          b <- sequence (map (\_ -> Hub.take s2) xs)
          Hub.unsubscribe s1
          Hub.unsubscribe s2
          pure { a, b }
      r <- runAffThrow prog
      r.a `shouldEqual` xs
      r.b `shouldEqual` xs

  -- The unit pin "subscribers see only messages published after
  -- subscribe" fixes a single drop. Generalise: every value
  -- published before the subscribe is invisible to the subscriber,
  -- regardless of batch size.
  it "messages published before subscribe are not delivered" do
    forAll 20 (arbitrary :: Gen (Array Int)) \prelude -> do
      let
        prog :: RIO () () Int
        prog = do
          hub <- F.liftEffect (Hub.make 4 :: _ (Hub Int))
          for_ prelude (Hub.publish hub)
          sub <- Hub.subscribe hub
          Hub.publish hub 99
          x <- Hub.take sub
          Hub.unsubscribe sub
          pure x
      r <- runAffThrow prog
      r `shouldEqual` 99

  -- `subscribers` should report exactly the count of live
  -- subscriptions: after `n` subscribes it's `n`, after each
  -- subsequent `unsubscribe` it decrements, hitting 0 at the end.
  it "subscribers reflects the live subscription count" do
    forAll 10 smallNat \n -> do
      let
        prog
          :: RIO () ()
              { afterSubscribe :: Int, afterUnsubscribe :: Int }
        prog = do
          hub <- F.liftEffect (Hub.make 4 :: _ (Hub Int))
          subs <-
            if n == 0 then pure []
            else traverse (\_ -> Hub.subscribe hub) (Array.range 1 n)
          afterSubscribe <- Hub.subscribers hub
          for_ subs Hub.unsubscribe
          afterUnsubscribe <- Hub.subscribers hub
          pure { afterSubscribe, afterUnsubscribe }
      r <- runAffThrow prog
      r.afterSubscribe `shouldEqual` n
      r.afterUnsubscribe `shouldEqual` 0

  -- The unit pin "publishDropNew per-subscriber drops only on
  -- full subs" demonstrates the drop on a single slot. Generalise:
  -- a hub with capacity `cap` and a single subscriber that never
  -- drains accepts exactly the first `cap` values via
  -- `publishDropNew`; every later publish is silently dropped for
  -- that subscriber. The first `cap` items are visible on take
  -- in publish order.
  it "publishDropNew buffers the first `cap` and silently drops the rest" do
    forAll 10 (chooseInt 1 5) \cap -> do
      let
        n = cap + 4
        prog :: RIO () () (Array Int)
        prog = do
          hub <- F.liftEffect (Hub.make cap :: _ (Hub Int))
          sub <- Hub.subscribe hub
          for_ (Array.range 1 n) (Hub.publishDropNew hub)
          xs <- traverse (\_ -> Hub.take sub) (Array.range 1 cap)
          Hub.unsubscribe sub
          pure xs
      r <- runAffThrow prog
      r `shouldEqual` Array.range 1 cap

  -- publishDropOld evicts the oldest buffered message under
  -- overflow. Generalise across capacity and total-publish counts:
  -- a stalled subscriber with capacity `cap` who only drains at
  -- the very end sees the last `cap` published values (in publish
  -- order), since each new publish past capacity pushes out the
  -- head.
  it "publishDropOld keeps the most recent `cap` messages" do
    forAll 10 (chooseInt 1 5) \cap -> do
      let
        n = cap + 4
        prog :: RIO () () (Array Int)
        prog = do
          hub <- F.liftEffect (Hub.make cap :: _ (Hub Int))
          sub <- Hub.subscribe hub
          for_ (Array.range 1 n) (Hub.publishDropOld hub)
          xs <- traverse (\_ -> Hub.take sub) (Array.range 1 cap)
          Hub.unsubscribe sub
          pure xs
      r <- runAffThrow prog
      r `shouldEqual` Array.range (n - cap + 1) n
