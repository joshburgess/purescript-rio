module Test.RIO.Aff.STM.TChanSpec (spec) where

import Prelude hiding (join)

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, fork, join, runRIO')
import RIO.Aff.STM (atomically)
import RIO.Aff.STM.TChan
  ( isEmptyTChan
  , newTChan
  , peekTChan
  , readTChan
  , tryReadTChan
  , writeTChan
  )

spec :: Spec Unit
spec = describe "RIO.Aff.STM.TChan" do
  it "new channel is empty" do
    let
      program :: RIO () () Boolean
      program = do
        ch <- atomically newTChan
        atomically (isEmptyTChan ch)
    result <- runRIO' program
    result `shouldEqual` true

  it "writeTChan then readTChan preserves FIFO order" do
    let
      program :: RIO () () (Array Int)
      program = do
        ch <- atomically newTChan
        atomically do
          writeTChan ch 1
          writeTChan ch 2
          writeTChan ch 3
        atomically do
          a <- readTChan ch
          b <- readTChan ch
          c <- readTChan ch
          pure [ a, b, c ]
    result <- runRIO' program
    result `shouldEqual` [ 1, 2, 3 ]

  it "tryReadTChan returns Nothing on empty" do
    let
      program :: RIO () () (Maybe Int)
      program = do
        ch <- atomically newTChan
        atomically (tryReadTChan ch)
    result <- runRIO' program
    result `shouldEqual` (Nothing :: Maybe Int)

  it "peekTChan does not consume the value" do
    let
      program
        :: RIO ()
             ()
             { peeked :: Int, taken :: Int, empty :: Boolean }
      program = do
        ch <- atomically newTChan
        atomically (writeTChan ch 42)
        peeked <- atomically (peekTChan ch)
        taken <- atomically (readTChan ch)
        empty <- atomically (isEmptyTChan ch)
        pure { peeked, taken, empty }
    result <- runRIO' program
    result `shouldEqual` { peeked: 42, taken: 42, empty: true }

  it "readTChan blocks until another fiber writes" do
    events <- liftEffect (Ref.new [])
    let
      push :: forall r e. String -> RIO r e Unit
      push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

      program :: RIO () () Int
      program = do
        ch <- atomically newTChan
        push "before-fork"
        consumer <- fork do
          v <- atomically (readTChan ch)
          push "after-read"
          pure v
        liftAff (delay (Milliseconds 20.0))
        push "before-write"
        atomically (writeTChan ch 7)
        join consumer
    result <- runRIO' program
    result `shouldEqual` 7
    order <- liftEffect (Ref.read events)
    order `shouldEqual` [ "before-fork", "before-write", "after-read" ]
