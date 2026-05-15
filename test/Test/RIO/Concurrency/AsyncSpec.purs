module Test.RIO.Concurrency.AsyncSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Fiber, delay, forkAff, launchAff_) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Concurrency (async, asyncInterrupt, fork, interrupt, race, timeout)
import RIO.Core (RIO, runRIO, runRIO')

type Errs = (oops :: String)

mkOops :: String -> Variant Errs
mkOops = Variant.inj (Proxy :: Proxy "oops")

-- | Fire a callback after a small Aff-driven delay. Standing in for
-- | a real setTimeout-style asynchronous source.
afterDelay :: Milliseconds -> Effect Unit -> Effect Unit
afterDelay ms action = Aff.launchAff_ do
  Aff.delay ms
  liftEffect action

spec :: Spec Unit
spec = describe "RIO.Concurrency (async / asyncInterrupt)" do

  describe "async" do
    it "resolves with the value the callback delivers" do
      let
        program :: RIO () Errs Int
        program = async \resume -> resume (Right 42)
      result <- runRIO program
      result `shouldEqual` (Right 42 :: Either _ _)

    it "fails with the typed Variant the callback delivers" do
      let
        program :: RIO () Errs Int
        program = async \resume -> resume (Left (mkOops "nope"))
      result <- runRIO program
      result `shouldEqual` (Left (mkOops "nope") :: Either _ _)

    it "ignores callback invocations after the first" do
      seen <- liftEffect (Ref.new 0)
      let
        program :: RIO () () Int
        program = async \resume -> do
          resume (Right 1)
          resume (Right 2)
          Ref.modify_ (_ + 1) seen
      result <- runRIO' program
      result `shouldEqual` 1
      -- the register function ran exactly once
      callCount <- liftEffect (Ref.read seen)
      callCount `shouldEqual` 1

    it "bridges a deferred callback that resolves after an Aff delay" do
      let
        program :: RIO () () Int
        program = async \resume ->
          afterDelay (Milliseconds 10.0) (resume (Right 7))
      result <- runRIO' program
      result `shouldEqual` 7

  describe "asyncInterrupt" do
    it "resolves like async when the callback fires before interruption" do
      let
        program :: RIO () Errs Int
        program = asyncInterrupt \resume -> do
          resume (Right 99)
          pure (pure unit)
      result <- runRIO program
      result `shouldEqual` (Right 99 :: Either _ _)

    it "fires the cancel effect when timeout interrupts the action" do
      cancelled <- liftEffect (Ref.new false)
      let
        slow :: RIO () () Int
        slow = asyncInterrupt \resume -> do
          -- never fires within the test window
          afterDelay (Milliseconds 5000.0) (resume (Right 0))
          pure (Ref.write true cancelled)

        program :: RIO () () (Maybe Int)
        program = timeout (Milliseconds 25.0) slow
      result <- runRIO' program
      result `shouldEqual` Nothing
      didCancel <- liftEffect (Ref.read cancelled)
      didCancel `shouldEqual` true

    it "does not fire the cancel effect when the callback wins the race" do
      cancelled <- liftEffect (Ref.new false)
      let
        fast :: RIO () () Int
        fast = asyncInterrupt \resume -> do
          afterDelay (Milliseconds 5.0) (resume (Right 1))
          pure (Ref.write true cancelled)

        program :: RIO () () (Maybe Int)
        program = timeout (Milliseconds 200.0) fast
      result <- runRIO' program
      result `shouldEqual` Just 1
      didCancel <- liftEffect (Ref.read cancelled)
      didCancel `shouldEqual` false

    it "cancel runs under explicit fork + interrupt" do
      cancelled <- liftEffect (Ref.new false)
      let
        slow :: RIO () () Int
        slow = asyncInterrupt \resume -> do
          afterDelay (Milliseconds 5000.0) (resume (Right 0))
          pure (Ref.write true cancelled)

        program :: RIO () () Unit
        program = do
          fib <- fork slow
          -- let the fiber suspend inside makeAff before we kill it
          liftAff (Aff.delay (Milliseconds 10.0))
          interrupt fib
      _ <- runRIO' program
      didCancel <- liftEffect (Ref.read cancelled)
      didCancel `shouldEqual` true

  describe "async + race" do
    it "lets a deferred async action lose a race to an immediate value" do
      let
        slow :: RIO () () Int
        slow = async \resume ->
          afterDelay (Milliseconds 500.0) (resume (Right 1))

        fast :: RIO () () Int
        fast = pure 2

        program :: RIO () () Int
        program = race slow fast
      result <- runRIO' program
      result `shouldEqual` 2
