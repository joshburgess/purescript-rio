module Test.RIO.Aff.STM.TMVarSpec (spec) where

import Prelude hiding (join)

import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, fork, join, runRIO')
import RIO.Aff.STM (atomically)
import RIO.Aff.STM.TMVar
  ( isEmptyTMVar
  , newEmptyTMVar
  , newTMVar
  , putTMVar
  , readTMVar
  , takeTMVar
  , tryPutTMVar
  , tryTakeTMVar
  )

spec :: Spec Unit
spec = describe "RIO.Aff.STM.TMVar" do
  it "newEmptyTMVar starts empty" do
    let
      program :: RIO () () Boolean
      program = do
        m <- atomically (newEmptyTMVar :: _ _ _)
        atomically (isEmptyTMVar (m :: _ Int))
    result <- runRIO' program
    result `shouldEqual` true

  it "newTMVar starts full and readTMVar returns the value" do
    let
      program :: RIO () () Int
      program = do
        m <- atomically (newTMVar 42)
        atomically (readTMVar m)
    result <- runRIO' program
    result `shouldEqual` 42

  it "takeTMVar empties the cell" do
    let
      program :: RIO () () { taken :: Int, empty :: Boolean }
      program = do
        m <- atomically (newTMVar 7)
        taken <- atomically (takeTMVar m)
        empty <- atomically (isEmptyTMVar m)
        pure { taken, empty }
    result <- runRIO' program
    result `shouldEqual` { taken: 7, empty: true }

  it "putTMVar then takeTMVar round-trips" do
    let
      program :: RIO () () Int
      program = do
        m <- atomically (newEmptyTMVar :: _ _ _)
        atomically (putTMVar (m :: _ Int) 99)
        atomically (takeTMVar m)
    result <- runRIO' program
    result `shouldEqual` 99

  it "tryTakeTMVar returns Nothing when empty" do
    let
      program :: RIO () () (Maybe Int)
      program = do
        m <- atomically (newEmptyTMVar :: _ _ _)
        atomically (tryTakeTMVar (m :: _ Int))
    result <- runRIO' program
    result `shouldEqual` (Nothing :: Maybe Int)

  it "tryPutTMVar returns false when full" do
    let
      program :: RIO () () Boolean
      program = do
        m <- atomically (newTMVar 1)
        atomically (tryPutTMVar m 2)
    result <- runRIO' program
    result `shouldEqual` false

  it "takeTMVar blocks until put from another fiber" do
    let
      program :: RIO () () Int
      program = do
        m <- atomically (newEmptyTMVar :: _ _ _)
        consumer <- fork (atomically (takeTMVar (m :: _ Int)))
        liftAff (delay (Milliseconds 10.0))
        atomically (putTMVar m 5)
        join consumer
    result <- runRIO' program
    result `shouldEqual` 5
