module Test.RIO.RefSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO)
import RIO.Ref (modify, modify_, new, newEffect, read, update, write)

spec :: Spec Unit
spec = describe "RIO.Ref" do
  it "new + read returns the initial value" do
    let
      program :: RIO () () Int
      program = do
        ref <- new 7
        read ref
    result <- runRIO program
    result `shouldEqual` (Right 7 :: Either _ Int)

  it "write overwrites and subsequent read sees the new value" do
    let
      program :: RIO () () String
      program = do
        ref <- new "init"
        write ref "next"
        read ref
    result <- runRIO program
    result `shouldEqual` (Right "next" :: Either _ String)

  it "modify applies a function and returns the new value" do
    let
      program :: RIO () () { returned :: Int, observed :: Int }
      program = do
        ref <- new 10
        returned <- modify ref (_ + 5)
        observed <- read ref
        pure { returned, observed }
    result <- runRIO program
    result `shouldEqual`
      (Right { returned: 15, observed: 15 } :: Either _ _)

  it "modify_ applies a function and discards the result" do
    let
      program :: RIO () () Int
      program = do
        ref <- new 1
        modify_ ref (_ * 10)
        read ref
    result <- runRIO program
    result `shouldEqual` (Right 10 :: Either _ Int)

  it "update is an alias for modify_" do
    let
      program :: RIO () () Int
      program = do
        ref <- new 100
        update ref (_ - 7)
        read ref
    result <- runRIO program
    result `shouldEqual` (Right 93 :: Either _ Int)

  it "newEffect produces a Ref usable from RIO" do
    ref <- liftEffect (newEffect 42)
    let
      program :: RIO () () { initial :: Int, afterModify :: Int }
      program = do
        initial <- read ref
        modify_ ref (_ + 1)
        afterModify <- read ref
        pure { initial, afterModify }
    result <- runRIO program
    result `shouldEqual`
      (Right { initial: 42, afterModify: 43 } :: Either _ _)

  it "two refs hold independent values" do
    let
      program :: RIO () () { a :: Int, b :: Int }
      program = do
        ra <- new 1
        rb <- new 2
        write ra 10
        write rb 20
        a <- read ra
        b <- read rb
        pure { a, b }
    result <- runRIO program
    result `shouldEqual`
      (Right { a: 10, b: 20 } :: Either _ _)

  it "modify on a Ref reads the latest written value (read-modify-write)" do
    -- A regression that broke the read-then-apply ordering (for
    -- example, applying the function to a stale cached value
    -- instead of the cell's current contents) would surface here:
    -- after two writes and a modify, the final value must reflect
    -- the most recent write.
    let
      program :: RIO () () Int
      program = do
        ref <- new 0
        write ref 1
        write ref 5
        _ <- modify ref (_ * 3)
        read ref
    result <- runRIO program
    result `shouldEqual` (Right 15 :: Either _ Int)
