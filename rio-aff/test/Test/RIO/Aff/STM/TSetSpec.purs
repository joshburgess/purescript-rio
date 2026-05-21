module Test.RIO.Aff.STM.TSetSpec (spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.STM (atomically)
import RIO.Aff.STM.TSet
  ( deleteTSet
  , insertTSet
  , memberTSet
  , newTSet
  , nullTSet
  , sizeTSet
  , toArrayTSet
  )

spec :: Spec Unit
spec = describe "RIO.Aff.STM.TSet" do
  it "newTSet starts with size 0 and reports null" do
    let
      program :: RIO () () { size :: Int, n :: Boolean }
      program = do
        s <- atomically (newTSet :: _ _ _)
        size <- atomically (sizeTSet (s :: _ Int))
        n <- atomically (nullTSet s)
        pure { size, n }
    result <- runRIO' program
    result `shouldEqual` { size: 0, n: true }

  it "insertTSet is idempotent and memberTSet reflects it" do
    let
      program :: RIO () () { has :: Boolean, size :: Int }
      program = do
        s <- atomically (newTSet :: _ _ _)
        atomically (insertTSet 7 (s :: _ Int))
        atomically (insertTSet 7 s)
        has <- atomically (memberTSet 7 s)
        size <- atomically (sizeTSet s)
        pure { has, size }
    result <- runRIO' program
    result `shouldEqual` { has: true, size: 1 }

  it "deleteTSet removes the element" do
    let
      program :: RIO () () Boolean
      program = do
        s <- atomically (newTSet :: _ _ _)
        atomically (insertTSet 1 (s :: _ Int))
        atomically (deleteTSet 1 s)
        atomically (memberTSet 1 s)
    result <- runRIO' program
    result `shouldEqual` false

  it "toArrayTSet snapshots in ascending order" do
    let
      program :: RIO () () (Array Int)
      program = do
        s <- atomically (newTSet :: _ _ _)
        atomically (insertTSet 3 (s :: _ Int))
        atomically (insertTSet 1 s)
        atomically (insertTSet 2 s)
        atomically (toArrayTSet s)
    result <- runRIO' program
    result `shouldEqual` [ 1, 2, 3 ]

  it "atomic multi-element write is observed all-or-nothing" do
    let
      program :: RIO () () (Array Int)
      program = do
        s <- atomically (newTSet :: _ _ _)
        atomically do
          insertTSet 1 (s :: _ Int)
          insertTSet 2 s
          insertTSet 3 s
        atomically (toArrayTSet s)
    result <- runRIO' program
    result `shouldEqual` [ 1, 2, 3 ]
