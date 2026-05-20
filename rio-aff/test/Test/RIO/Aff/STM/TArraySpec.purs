module Test.RIO.Aff.STM.TArraySpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.STM (atomically)
import RIO.Aff.STM.TArray
  ( fromArrayTArray
  , lengthTArray
  , modifyTArray
  , newTArray
  , readTArray
  , swapTArray
  , toArrayTArray
  , writeTArray
  )

spec :: Spec Unit
spec = describe "RIO.Aff.STM.TArray" do
  it "newTArray + lengthTArray reports the configured size" do
    let
      program :: RIO () () Int
      program = do
        a <- atomically (newTArray 5 0)
        atomically (lengthTArray a)
    result <- runRIO' program
    result `shouldEqual` 5

  it "newTArray fills every cell with the initial value" do
    let
      program :: RIO () () (Array Int)
      program = do
        a <- atomically (newTArray 3 42)
        atomically (toArrayTArray a)
    result <- runRIO' program
    result `shouldEqual` [ 42, 42, 42 ]

  it "negative size produces an empty array" do
    -- A regression that did not clamp negative sizes via Array.replicate
    -- would either crash or produce something other than [].
    let
      program :: RIO () () (Array Int)
      program = do
        a <- atomically (newTArray (-3) 99)
        atomically (toArrayTArray a)
    result <- runRIO' program
    result `shouldEqual` []

  it "fromArrayTArray wraps an existing array verbatim" do
    let
      program :: RIO () () (Array Int)
      program = do
        a <- atomically (fromArrayTArray [ 10, 20, 30 ])
        atomically (toArrayTArray a)
    result <- runRIO' program
    result `shouldEqual` [ 10, 20, 30 ]

  it "writeTArray + readTArray round-trip at a valid index" do
    let
      program :: RIO () () (Maybe Int)
      program = do
        a <- atomically (newTArray 3 0)
        _ <- atomically (writeTArray 1 77 a)
        atomically (readTArray 1 a)
    result <- runRIO' program
    result `shouldEqual` Just 77

  it "writeTArray returns False and is a no-op for an out-of-bounds index" do
    let
      program :: RIO () () { ok :: Boolean, snapshot :: Array Int }
      program = do
        a <- atomically (fromArrayTArray [ 1, 2, 3 ])
        ok <- atomically (writeTArray 99 0 a)
        snapshot <- atomically (toArrayTArray a)
        pure { ok, snapshot }
    result <- runRIO' program
    result `shouldEqual` { ok: false, snapshot: [ 1, 2, 3 ] }

  it "writeTArray returns False for a negative index and is a no-op" do
    let
      program :: RIO () () { ok :: Boolean, snapshot :: Array Int }
      program = do
        a <- atomically (fromArrayTArray [ 1, 2, 3 ])
        ok <- atomically (writeTArray (-1) 0 a)
        snapshot <- atomically (toArrayTArray a)
        pure { ok, snapshot }
    result <- runRIO' program
    result `shouldEqual` { ok: false, snapshot: [ 1, 2, 3 ] }

  it "readTArray returns Nothing for an out-of-bounds index" do
    let
      program :: RIO () () (Maybe Int)
      program = do
        a <- atomically (fromArrayTArray [ 1, 2, 3 ])
        atomically (readTArray 99 a)
    result <- runRIO' program
    result `shouldEqual` Nothing

  it "modifyTArray applies a function to a single cell" do
    let
      program :: RIO () () (Array Int)
      program = do
        a <- atomically (fromArrayTArray [ 1, 2, 3, 4 ])
        _ <- atomically (modifyTArray 2 (_ * 10) a)
        atomically (toArrayTArray a)
    result <- runRIO' program
    result `shouldEqual` [ 1, 2, 30, 4 ]

  it "modifyTArray returns False and is a no-op for an out-of-bounds index" do
    let
      program :: RIO () () { ok :: Boolean, snapshot :: Array Int }
      program = do
        a <- atomically (fromArrayTArray [ 1, 2, 3 ])
        ok <- atomically (modifyTArray 99 (_ + 1) a)
        snapshot <- atomically (toArrayTArray a)
        pure { ok, snapshot }
    result <- runRIO' program
    result `shouldEqual` { ok: false, snapshot: [ 1, 2, 3 ] }

  it "swapTArray exchanges two valid cells" do
    let
      program :: RIO () () { ok :: Boolean, snapshot :: Array Int }
      program = do
        a <- atomically (fromArrayTArray [ 1, 2, 3, 4 ])
        ok <- atomically (swapTArray 0 3 a)
        snapshot <- atomically (toArrayTArray a)
        pure { ok, snapshot }
    result <- runRIO' program
    result `shouldEqual` { ok: true, snapshot: [ 4, 2, 3, 1 ] }

  it "swapTArray with equal indices is a no-op that returns True" do
    let
      program :: RIO () () { ok :: Boolean, snapshot :: Array Int }
      program = do
        a <- atomically (fromArrayTArray [ 1, 2, 3 ])
        ok <- atomically (swapTArray 1 1 a)
        snapshot <- atomically (toArrayTArray a)
        pure { ok, snapshot }
    result <- runRIO' program
    result `shouldEqual` { ok: true, snapshot: [ 1, 2, 3 ] }

  it "swapTArray returns False when either index is out of bounds" do
    let
      program :: RIO () () { ok :: Boolean, snapshot :: Array Int }
      program = do
        a <- atomically (fromArrayTArray [ 1, 2, 3 ])
        ok <- atomically (swapTArray 0 99 a)
        snapshot <- atomically (toArrayTArray a)
        pure { ok, snapshot }
    result <- runRIO' program
    result `shouldEqual` { ok: false, snapshot: [ 1, 2, 3 ] }

  it "writes inside one transaction are committed together" do
    -- All three writes happen in one atomically block, so a
    -- snapshot in a second transaction sees every change at once.
    -- A regression that committed cell-by-cell would still pass
    -- this single-threaded test, but the test pins the API
    -- contract that toArrayTArray reflects every committed write.
    let
      program :: RIO () () (Array Int)
      program = do
        a <- atomically (newTArray 3 0)
        atomically do
          _ <- writeTArray 0 10 a
          _ <- writeTArray 1 20 a
          _ <- writeTArray 2 30 a
          pure unit
        atomically (toArrayTArray a)
    result <- runRIO' program
    result `shouldEqual` [ 10, 20, 30 ]
