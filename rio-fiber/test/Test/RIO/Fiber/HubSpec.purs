module Test.RIO.Fiber.HubSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Hub as Hub
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Hub" do
  it "fans publishes out to every subscriber" do
    hub <- liftEffect (Hub.make 4 :: _ (Hub.Hub Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        s1 <- Hub.subscribe hub
        s2 <- Hub.subscribe hub
        Hub.publish hub 7
        a <- Hub.take s1
        b <- Hub.take s2
        Hub.unsubscribe s1
        Hub.unsubscribe s2
        pure [ a, b ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 7, 7 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "subscribers see only messages published after subscribe" do
    hub <- liftEffect (Hub.make 4 :: _ (Hub.Hub Int))
    let
      prog :: F.RIO () () Int
      prog = do
        Hub.publish hub 1 -- no subscribers yet, dropped
        sub <- Hub.subscribe hub
        Hub.publish hub 2
        a <- Hub.take sub
        Hub.unsubscribe sub
        pure a
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 2
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "publish backpressures when a subscriber's queue is full" do
    hub <- liftEffect (Hub.make 1 :: _ (Hub.Hub Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        sub <- Hub.subscribe hub
        Hub.publish hub 1
        fib <- F.fork (Hub.publish hub 2)
        F.sleep (Milliseconds 5.0)
        a <- Hub.take sub
        _ <- F.join fib
        b <- Hub.take sub
        Hub.unsubscribe sub
        pure [ a, b ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "subscribeScoped releases the slot when the scope closes" do
    hub <- liftEffect (Hub.make 2 :: _ (Hub.Hub Int))
    let
      prog :: F.RIO () () { inside :: Int, outside :: Int }
      prog = do
        inside <- Scope.scoped \scope -> do
          _ <- Hub.subscribeScoped scope hub
          _ <- Hub.subscribeScoped scope hub
          Hub.subscribers hub
        outside <- Hub.subscribers hub
        pure { inside, outside }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.inside `shouldEqual` 2
        r.outside `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryPublish returns false when any subscriber is full" do
    hub <- liftEffect (Hub.make 1 :: _ (Hub.Hub Int))
    let
      prog :: F.RIO () () { first :: Boolean, second :: Boolean }
      prog = do
        s1 <- Hub.subscribe hub
        _ <- Hub.subscribe hub
        first <- Hub.tryPublish hub 1
        -- s1 is now full
        second <- Hub.tryPublish hub 2
        _ <- Hub.take s1
        pure { first, second }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.first `shouldEqual` true
        r.second `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "publishDropNew per-subscriber drops only on full subs" do
    hub <- liftEffect (Hub.make 1 :: _ (Hub.Hub Int))
    let
      prog :: F.RIO () () { fast :: Int, slowFirst :: Int }
      prog = do
        slow <- Hub.subscribe hub
        fast <- Hub.subscribe hub
        Hub.publishDropNew hub 1
        -- both received the first; both queues full now
        -- drain fast so it can accept more; slow stays full
        a <- Hub.take fast
        Hub.publishDropNew hub 2 -- slow drops, fast accepts
        b <- Hub.take fast
        s1 <- Hub.take slow
        Hub.unsubscribe slow
        Hub.unsubscribe fast
        -- a + b = 1 + 2 = 3; s1 = 1 (its only slot, never freed in time)
        pure { fast: a + b, slowFirst: s1 }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.fast `shouldEqual` 3
        r.slowFirst `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "publishDropOld evicts oldest in full subs" do
    hub <- liftEffect (Hub.make 1 :: _ (Hub.Hub Int))
    let
      prog :: F.RIO () () Int
      prog = do
        sub <- Hub.subscribe hub
        Hub.publishDropOld hub 1
        Hub.publishDropOld hub 2 -- evicts 1, accepts 2
        a <- Hub.take sub
        Hub.unsubscribe sub
        pure a
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 2
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
