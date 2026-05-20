module Test.RIO.Aff.HubSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Hub (make, publish, publishAll, subscribe, subscriberCount, unsubscribe)
import RIO.Aff.Queue (poll, take)

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Hub" do

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

    it "make starts with zero subscribers" do
      hub <- liftEffect (make :: _ (_ Int))
      n <- liftEffect (subscriberCount hub)
      n `shouldEqual` 0

    it "publish with no subscribers is a no-op" do
      hub <- liftEffect (make :: _ (_ Int))
      let
        program :: RIO () () { drained :: Maybe Int, count :: Int }
        program = do
          publish hub 1
          publish hub 2
          count <- liftEffect (subscriberCount hub)
          -- subscribe after the publishes so we can confirm nothing
          -- was retroactively queued for a brand-new subscriber.
          sub <- subscribe hub
          drained <- poll sub.queue
          pure { drained, count }
      r <- runRIO' program
      r.count `shouldEqual` 0
      r.drained `shouldEqual` Nothing

    it "publishAll delivers a batch to every subscriber in order" do
      hub <- liftEffect (make :: _ (_ Int))
      let
        program
          :: RIO ()
               ()
               { a :: Array (Maybe Int), b :: Array (Maybe Int) }
        program = do
          subA <- subscribe hub
          subB <- subscribe hub
          publishAll hub [ 1, 2, 3 ]
          a1 <- take subA.queue
          a2 <- take subA.queue
          a3 <- take subA.queue
          b1 <- take subB.queue
          b2 <- take subB.queue
          b3 <- take subB.queue
          pure { a: [ a1, a2, a3 ], b: [ b1, b2, b3 ] }
      r <- runRIO' program
      r.a `shouldEqual` [ Just 1, Just 2, Just 3 ]
      r.b `shouldEqual` [ Just 1, Just 2, Just 3 ]

    it "the `unsubscribe` smart constructor behaves identically to running the action directly" do
      -- Docstring promise: `unsubscribe action = action` is
      -- "Equivalent to running that action directly; provided
      -- for readability." Pin that running the returned action
      -- through `unsubscribe` removes the subscriber just like
      -- calling `sub.unsubscribe` would.
      hub <- liftEffect (make :: _ (_ Int))
      let
        program
          :: RIO () ()
               { afterAdd :: Int, afterRemove :: Int, drained :: Maybe Int }
        program = do
          sub <- subscribe hub
          afterAdd <- liftEffect (subscriberCount hub)
          unsubscribe sub.unsubscribe
          publish hub 99
          afterRemove <- liftEffect (subscriberCount hub)
          drained <- poll sub.queue
          pure { afterAdd, afterRemove, drained }
      r <- runRIO' program
      r.afterAdd `shouldEqual` 1
      r.afterRemove `shouldEqual` 0
      r.drained `shouldEqual` Nothing

    it "a slow consumer does not block publishes or other subscribers" do
      -- The module's docstring promises that "a slow consumer does
      -- not slow the producer down" and "values published while a
      -- subscriber is alive land in its queue". Each subscriber's
      -- queue is unbounded; the natural tradeoff is "a slow
      -- consumer can fall arbitrarily far behind". Pin both halves
      -- by holding one subscriber's queue undrained, publishing a
      -- batch synchronously, and asserting (a) every publish
      -- returns immediately, (b) the fast subscriber sees every
      -- value in order, and (c) the slow subscriber's queue still
      -- holds every value in order when drained later.
      hub <- liftEffect (make :: _ (_ Int))
      let
        program
          :: RIO () ()
               { fast :: Array (Maybe Int), slow :: Array (Maybe Int) }
        program = do
          slow <- subscribe hub
          fast <- subscribe hub
          publishAll hub [ 1, 2, 3, 4, 5 ]
          f1 <- take fast.queue
          f2 <- take fast.queue
          f3 <- take fast.queue
          f4 <- take fast.queue
          f5 <- take fast.queue
          s1 <- take slow.queue
          s2 <- take slow.queue
          s3 <- take slow.queue
          s4 <- take slow.queue
          s5 <- take slow.queue
          pure
            { fast: [ f1, f2, f3, f4, f5 ]
            , slow: [ s1, s2, s3, s4, s5 ]
            }
      r <- runRIO' program
      r.fast `shouldEqual` [ Just 1, Just 2, Just 3, Just 4, Just 5 ]
      r.slow `shouldEqual` [ Just 1, Just 2, Just 3, Just 4, Just 5 ]

    it "an unsubscribed consumer does not receive subsequent publishes" do
      hub <- liftEffect (make :: _ (_ Int))
      let
        program
          :: RIO ()
               ()
               { firstA :: Maybe Int, drainedA :: Maybe Int, firstB :: Maybe Int }
        program = do
          subA <- subscribe hub
          subB <- subscribe hub
          publish hub 1
          firstA <- take subA.queue
          firstB <- take subB.queue
          subA.unsubscribe
          publish hub 2
          drainedA <- poll subA.queue
          _ <- take subB.queue
          pure { firstA, drainedA, firstB }
      r <- runRIO' program
      r.firstA `shouldEqual` Just 1
      r.firstB `shouldEqual` Just 1
      r.drainedA `shouldEqual` Nothing
