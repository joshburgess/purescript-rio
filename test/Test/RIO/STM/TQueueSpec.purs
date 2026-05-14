module Test.RIO.STM.TQueueSpec (spec) where

import Prelude

import Data.Array (range)
import Data.Foldable (sum)
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
import RIO.STM.TQueue
  ( isEmptyTQueue
  , lengthTQueue
  , newTQueue
  , peekTQueue
  , readTQueue
  , tryReadTQueue
  , writeTQueue
  )

spec :: Spec Unit
spec = describe "RIO.STM.TQueue" do
  it "preserves FIFO order across sequential writes and reads" do
    let
      program :: RIO () () (Array Int)
      program = do
        q <- atomically newTQueue
        traverse_ (\n -> atomically (writeTQueue q n)) [ 1, 2, 3, 4 ]
        traverse (\_ -> atomically (readTQueue q)) [ 1, 2, 3, 4 ]
    result <- runRIO' program
    result `shouldEqual` [ 1, 2, 3, 4 ]

  it "tryReadTQueue returns Nothing on empty, Just on non-empty" do
    let
      program :: RIO () () { empty :: Maybe Int, full :: Maybe Int }
      program = do
        q <- atomically newTQueue
        empty <- atomically (tryReadTQueue q)
        atomically (writeTQueue q 42)
        full <- atomically (tryReadTQueue q)
        pure { empty, full }
    result <- runRIO' program
    result `shouldEqual` { empty: Nothing, full: Just 42 }

  it "isEmptyTQueue and lengthTQueue track state" do
    let
      program :: RIO () () { isEmpty0 :: Boolean, len2 :: Int, isEmptyAfter :: Boolean }
      program = do
        q <- atomically newTQueue
        isEmpty0 <- atomically (isEmptyTQueue q)
        atomically (writeTQueue q 1)
        atomically (writeTQueue q 2)
        len2 <- atomically (lengthTQueue q)
        _ <- atomically (readTQueue q)
        _ <- atomically (readTQueue q)
        isEmptyAfter <- atomically (isEmptyTQueue q)
        pure { isEmpty0, len2, isEmptyAfter }
    result <- runRIO' program
    result `shouldEqual` { isEmpty0: true, len2: 2, isEmptyAfter: true }

  it "readTQueue blocks until a producer writes" do
    events <- liftEffect (Ref.new [])
    let
      push :: forall r e. String -> RIO r e Unit
      push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

      program :: RIO () () Int
      program = do
        q <- atomically newTQueue
        push "before-fork"
        waiter <- fork do
          v <- atomically (readTQueue q)
          push "after-read"
          pure v
        liftAff (delay (Milliseconds 20.0))
        push "before-write"
        atomically (writeTQueue q 99)
        join waiter
    result <- runRIO' program
    result `shouldEqual` 99
    order <- liftEffect (Ref.read events)
    order `shouldEqual` [ "before-fork", "before-write", "after-read" ]

  it "peekTQueue returns the head without removing it" do
    let
      program
        :: RIO ()
             ()
             { peeked :: Int, lenAfterPeek :: Int, readBack :: Int }
      program = do
        q <- atomically newTQueue
        atomically (writeTQueue q 7)
        atomically (writeTQueue q 8)
        peeked <- atomically (peekTQueue q)
        lenAfterPeek <- atomically (lengthTQueue q)
        readBack <- atomically (readTQueue q)
        pure { peeked, lenAfterPeek, readBack }
    result <- runRIO' program
    result `shouldEqual` { peeked: 7, lenAfterPeek: 2, readBack: 7 }

  it "peekTQueue blocks on an empty queue until a write commits" do
    events <- liftEffect (Ref.new [])
    let
      push :: forall r e. String -> RIO r e Unit
      push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

      program :: RIO () () Int
      program = do
        q <- atomically newTQueue
        push "before-fork"
        waiter <- fork do
          v <- atomically (peekTQueue q)
          push "after-peek"
          pure v
        liftAff (delay (Milliseconds 20.0))
        push "before-write"
        atomically (writeTQueue q 11)
        join waiter
    result <- runRIO' program
    result `shouldEqual` 11
    order <- liftEffect (Ref.read events)
    order `shouldEqual` [ "before-fork", "before-write", "after-peek" ]

  it "many parallel producers and consumers preserve total" do
    let
      n = 30

      program :: RIO () () Int
      program = do
        q <- atomically newTQueue
        producers <- traverse
          (\k -> fork (atomically (writeTQueue q k)))
          (range 1 n)
        consumers <- traverse
          (\_ -> fork (atomically (readTQueue q)))
          (range 1 n)
        _ <- traverse join producers
        ys <- traverse join consumers
        pure (sum ys)
    result <- runRIO' program
    result `shouldEqual` (sum (range 1 30))
