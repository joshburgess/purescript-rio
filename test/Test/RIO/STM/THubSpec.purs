module Test.RIO.STM.THubSpec (spec) where

import Prelude hiding (join)

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, fork, join, runRIO')
import RIO.STM (atomically)
import RIO.STM.THub
  ( THub
  , lengthSubscription
  , newBoundedTHub
  , newDroppingTHub
  , newSlidingTHub
  , newUnboundedTHub
  , publishTHub
  , subscribeTHub
  , subscriberCount
  , takeSubscription
  , tryTakeSubscription
  , unsubscribeTHub
  , withSubscription
  )

spec :: Spec Unit
spec = describe "RIO.STM.THub" do
  describe "subscribers and basic delivery" do
    it "a single subscriber receives every value in publish order" do
      let
        program :: RIO () () (Array Int)
        program = do
          hub <- atomically newUnboundedTHub
          sub <- atomically (subscribeTHub hub)
          traverse_ (\n -> atomically (publishTHub hub n) >>= \_ -> pure unit)
            [ 1, 2, 3 ]
          traverse (\_ -> atomically (takeSubscription sub)) [ 1, 2, 3 ]
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3 ]

    it "two subscribers each receive every value independently" do
      let
        program :: RIO () () { a :: Array Int, b :: Array Int }
        program = do
          hub <- atomically newUnboundedTHub
          sa <- atomically (subscribeTHub hub)
          sb <- atomically (subscribeTHub hub)
          traverse_ (\n -> atomically (publishTHub hub n) >>= \_ -> pure unit)
            [ 10, 20, 30 ]
          a <- traverse (\_ -> atomically (takeSubscription sa)) [ 10, 20, 30 ]
          b <- traverse (\_ -> atomically (takeSubscription sb)) [ 10, 20, 30 ]
          pure { a, b }
      result <- runRIO' program
      result `shouldEqual` { a: [ 10, 20, 30 ], b: [ 10, 20, 30 ] }

    it "a subscriber sees only values published after it registers" do
      let
        program :: RIO () () (Maybe Int)
        program = do
          hub <- atomically newUnboundedTHub
          _ <- atomically (publishTHub hub 1)
          sub <- atomically (subscribeTHub hub)
          atomically (tryTakeSubscription sub)
      result <- runRIO' program
      result `shouldEqual` Nothing

    it "subscriberCount tracks subscribe and unsubscribe" do
      let
        program :: RIO () () { c0 :: Int, c1 :: Int, c2 :: Int, c1again :: Int }
        program = do
          (hub :: THub Int) <- atomically newUnboundedTHub
          c0 <- atomically (subscriberCount hub)
          a <- atomically (subscribeTHub hub)
          c1 <- atomically (subscriberCount hub)
          b <- atomically (subscribeTHub hub)
          c2 <- atomically (subscriberCount hub)
          atomically (unsubscribeTHub b)
          c1again <- atomically (subscriberCount hub)
          atomically (unsubscribeTHub a)
          pure { c0, c1, c2, c1again }
      result <- runRIO' program
      result `shouldEqual` { c0: 0, c1: 1, c2: 2, c1again: 1 }

  describe "Sliding strategy" do
    it "drops the oldest value when the buffer is full" do
      let
        program :: RIO () () (Array Int)
        program = do
          hub <- atomically (newSlidingTHub 2)
          sub <- atomically (subscribeTHub hub)
          _ <- atomically (publishTHub hub 1)
          _ <- atomically (publishTHub hub 2)
          _ <- atomically (publishTHub hub 3)
          traverse (\_ -> atomically (takeSubscription sub)) [ 0, 0 ]
      result <- runRIO' program
      result `shouldEqual` [ 2, 3 ]

    it "publishTHub always returns true for sliding" do
      let
        program :: RIO () () { r1 :: Boolean, r2 :: Boolean, r3 :: Boolean }
        program = do
          hub <- atomically (newSlidingTHub 1)
          _ <- atomically (subscribeTHub hub)
          r1 <- atomically (publishTHub hub 10)
          r2 <- atomically (publishTHub hub 20)
          r3 <- atomically (publishTHub hub 30)
          pure { r1, r2, r3 }
      result <- runRIO' program
      result `shouldEqual` { r1: true, r2: true, r3: true }

  describe "Dropping strategy" do
    it "drops the new value and returns false when the buffer is full" do
      let
        program
          :: RIO () ()
               { r1 :: Boolean
               , r2 :: Boolean
               , r3 :: Boolean
               , buffered :: Array Int
               }
        program = do
          hub <- atomically (newDroppingTHub 2)
          sub <- atomically (subscribeTHub hub)
          r1 <- atomically (publishTHub hub 1)
          r2 <- atomically (publishTHub hub 2)
          r3 <- atomically (publishTHub hub 3)
          buffered <- traverse (\_ -> atomically (takeSubscription sub)) [ 0, 0 ]
          pure { r1, r2, r3, buffered }
      result <- runRIO' program
      result `shouldEqual`
        { r1: true, r2: true, r3: false, buffered: [ 1, 2 ] }

  describe "Bounded strategy" do
    it "publishTHub blocks (retries) until a consumer takes a value" do
      events <- liftEffect (Ref.new [])
      let
        push :: forall r e. String -> RIO r e Unit
        push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

        program :: RIO () () Int
        program = do
          hub <- atomically (newBoundedTHub 1)
          sub <- atomically (subscribeTHub hub)
          _ <- atomically (publishTHub hub 1)
          push "buffer-full"
          producer <- fork do
            _ <- atomically (publishTHub hub 2)
            push "publish-2-committed"
            pure unit
          liftAff (delay (Milliseconds 20.0))
          push "before-take"
          v1 <- atomically (takeSubscription sub)
          _ <- join producer
          v2 <- atomically (takeSubscription sub)
          pure (v1 + v2)
      result <- runRIO' program
      result `shouldEqual` 3
      order <- liftEffect (Ref.read events)
      order `shouldEqual`
        [ "buffer-full", "before-take", "publish-2-committed" ]

    it "with no subscribers, publish never blocks (no buffer to fill)" do
      let
        program :: RIO () () Boolean
        program = do
          (hub :: THub Int) <- atomically (newBoundedTHub 0)
          atomically (publishTHub hub 1)
      result <- runRIO' program
      result `shouldEqual` true

  describe "Unbounded strategy" do
    it "never drops; many publishes buffer up and drain in order" do
      let
        n = 50

        program :: RIO () () { len :: Int, drained :: Array Int }
        program = do
          hub <- atomically newUnboundedTHub
          sub <- atomically (subscribeTHub hub)
          traverse_ (\k -> atomically (publishTHub hub k) >>= \_ -> pure unit)
            (range 1 n)
          len <- atomically (lengthSubscription sub)
          drained <- traverse (\_ -> atomically (takeSubscription sub))
            (range 1 n)
          pure { len, drained }
      result <- runRIO' program
      result.len `shouldEqual` n
      result.drained `shouldEqual` range 1 n

  describe "unsubscribe" do
    it "after unsubscribe, further publishes are not delivered" do
      let
        program :: RIO () () (Maybe Int)
        program = do
          hub <- atomically newUnboundedTHub
          sub <- atomically (subscribeTHub hub)
          _ <- atomically (publishTHub hub 1)
          _ <- atomically (takeSubscription sub)
          atomically (unsubscribeTHub sub)
          _ <- atomically (publishTHub hub 2)
          atomically (tryTakeSubscription sub)
      result <- runRIO' program
      result `shouldEqual` Nothing

  describe "withSubscription" do
    it "releases the subscription on success" do
      let
        program :: RIO () () { during :: Int, after :: Int }
        program = do
          hub <- atomically newUnboundedTHub
          during <- withSubscription hub \sub -> do
            _ <- atomically (publishTHub hub 7)
            v <- atomically (takeSubscription sub)
            n <- atomically (subscriberCount hub)
            pure (v + n)
          after <- atomically (subscriberCount hub)
          pure { during, after }
      result <- runRIO' program
      result `shouldEqual` { during: 8, after: 0 }
