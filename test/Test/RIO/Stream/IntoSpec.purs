module Test.RIO.Stream.IntoSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Hub as Hub
import RIO.Queue as Queue
import RIO.Stream (Stream)
import RIO.Stream as Stream

spec :: Spec Unit
spec = describe "RIO.Stream (intoQueue / intoHub)" do

  describe "intoQueue" do
    it "offers every element in order, draining the stream" do
      q <- liftEffect Queue.unbounded
      let
        s :: Stream () () Int
        s = Stream.fromArray [ 1, 2, 3 ]

        program :: RIO () () (Array Int)
        program = do
          Stream.intoQueue q s
          a <- Queue.poll q
          b <- Queue.poll q
          c <- Queue.poll q
          d <- Queue.poll q
          pure
            [ maybeOr a (-1)
            , maybeOr b (-1)
            , maybeOr c (-1)
            , maybeOr d (-1)
            ]
      result <- runRIO' program
      result `shouldEqual` [ 1, 2, 3, -1 ]

    it "is a no-op for an empty stream" do
      q <- liftEffect Queue.unbounded
      let
        program :: RIO () () (Maybe Int)
        program = do
          Stream.intoQueue q (Stream.empty :: Stream () () Int)
          Queue.poll q
      result <- runRIO' program
      result `shouldEqual` Nothing

    it "stops once the queue is shut down" do
      q <- liftEffect Queue.unbounded
      let
        program :: RIO () () Unit
        program = do
          Queue.shutdown q
          Stream.intoQueue q (Stream.fromArray [ 10, 20, 30 ])
      _ <- runRIO' program
      remaining <- runRIO' (Queue.poll q)
      remaining `shouldEqual` Nothing

  describe "intoHub" do
    it "publishes every element to every active subscriber" do
      hub <- liftEffect (Hub.make :: _ (Hub.Hub Int))
      let
        program :: RIO () () { a :: Array Int, b :: Array Int }
        program = do
          subA <- Hub.subscribe hub
          subB <- Hub.subscribe hub
          Stream.intoHub hub (Stream.fromArray [ 1, 2, 3 ])
          a1 <- Queue.poll subA.queue
          a2 <- Queue.poll subA.queue
          a3 <- Queue.poll subA.queue
          b1 <- Queue.poll subB.queue
          b2 <- Queue.poll subB.queue
          b3 <- Queue.poll subB.queue
          pure
            { a:
                [ maybeOr a1 (-1)
                , maybeOr a2 (-1)
                , maybeOr a3 (-1)
                ]
            , b:
                [ maybeOr b1 (-1)
                , maybeOr b2 (-1)
                , maybeOr b3 (-1)
                ]
            }
      result <- runRIO' program
      result.a `shouldEqual` [ 1, 2, 3 ]
      result.b `shouldEqual` [ 1, 2, 3 ]

    it "is a no-op for an empty stream" do
      hub <- liftEffect (Hub.make :: _ (Hub.Hub Int))
      let
        program :: RIO () () (Maybe Int)
        program = do
          sub <- Hub.subscribe hub
          Stream.intoHub hub (Stream.empty :: Stream () () Int)
          Queue.poll sub.queue
      result <- runRIO' program
      result `shouldEqual` Nothing

    it "subscribers added after intoHub begins miss earlier items" do
      hub <- liftEffect (Hub.make :: _ (Hub.Hub Int))
      let
        program :: RIO () () (Array Int)
        program = do
          Stream.intoHub hub (Stream.fromArray [ 1, 2 ])
          sub <- Hub.subscribe hub
          Stream.intoHub hub (Stream.fromArray [ 3, 4 ])
          a <- Queue.poll sub.queue
          b <- Queue.poll sub.queue
          c <- Queue.poll sub.queue
          pure
            [ maybeOr a (-1)
            , maybeOr b (-1)
            , maybeOr c (-1)
            ]
      result <- runRIO' program
      result `shouldEqual` [ 3, 4, -1 ]

maybeOr :: forall a. Maybe a -> a -> a
maybeOr (Just a) _ = a
maybeOr Nothing fallback = fallback
