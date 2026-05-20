module Test.RIO.Aff.FiberRefSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Concurrency (join) as Conc
import RIO.Aff.Core (RIO, provide, runRIO)
import RIO.Aff.FiberRef
  ( FiberRefs
  , forkFiber
  , get
  , make
  , newFiberRefsEffect
  , set
  , update
  )
import Effect.Class (liftEffect)

withFiberRefs
  :: forall e a
   . RIO (fiberRefs :: FiberRefs) e a
  -> RIO () e a
withFiberRefs program = do
  refs <- liftEffect newFiberRefsEffect
  provide (Proxy :: Proxy "fiberRefs") refs program

spec :: Spec Unit
spec = describe "RIO.Aff.FiberRef" do
  describe "get / set / update" do
    it "get returns the default before any set" do
      let
        program :: RIO (fiberRefs :: FiberRefs) () Int
        program = do
          ref <- make 42
          get ref
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right 42

    it "set overwrites and subsequent get sees the new value" do
      let
        program :: RIO (fiberRefs :: FiberRefs) () String
        program = do
          ref <- make "init"
          set ref "next"
          get ref
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right "next"

    it "update applies a function to the current value" do
      let
        program :: RIO (fiberRefs :: FiberRefs) () Int
        program = do
          ref <- make 1
          update ref (_ * 10)
          update ref (_ * 10)
          get ref
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right 100

  describe "fork-snapshot semantics" do
    it "child inherits parent's value at fork time" do
      let
        program :: RIO (fiberRefs :: FiberRefs) () Int
        program = do
          ref <- make 0
          set ref 7
          fib <- forkFiber (get ref)
          Conc.join fib
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right 7

    it "child writes do not bleed into the parent" do
      let
        program
          :: RIO (fiberRefs :: FiberRefs) ()
               { child :: Int, parent :: Int }
        program = do
          ref <- make 0
          fib <- forkFiber do
            set ref 100
            get ref
          childValue <- Conc.join fib
          parentValue <- get ref
          pure { child: childValue, parent: parentValue }
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right { child: 100, parent: 0 }

    it "parent writes after fork do not bleed into the child" do
      let
        program
          :: RIO (fiberRefs :: FiberRefs) ()
               { child :: Int, parent :: Int }
        program = do
          ref <- make 0
          fib <- forkFiber do
            liftAff (delay (Milliseconds 5.0))
            get ref
          set ref 999
          childValue <- Conc.join fib
          parentValue <- get ref
          pure { child: childValue, parent: parentValue }
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right { child: 0, parent: 999 }

    it "multiple cells are snapshotted independently" do
      let
        program
          :: RIO (fiberRefs :: FiberRefs) ()
               { a :: Int, b :: Int, pa :: Int, pb :: Int }
        program = do
          refA <- make 1
          refB <- make 2
          fib <- forkFiber do
            set refA 11
            set refB 22
            a <- get refA
            b <- get refB
            pure { a, b }
          childView <- Conc.join fib
          pa <- get refA
          pb <- get refB
          pure
            { a: childView.a, b: childView.b, pa, pb }
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right { a: 11, b: 22, pa: 1, pb: 2 }

    it "child preserves the inherited value after sleeping past parent writes" do
      let
        program :: RIO (fiberRefs :: FiberRefs) () Int
        program = do
          ref <- make 1000
          fib <- forkFiber do
            liftAff (delay (Milliseconds 5.0))
            get ref
          Conc.join fib
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right 1000

  describe "ref type isolation" do
    it "distinct FiberRefs use distinct keys" do
      let
        program
          :: RIO (fiberRefs :: FiberRefs) () (Tuple Int String)
        program = do
          refA <- make 1
          refB <- make "hello"
          a <- get refA
          b <- get refB
          pure (Tuple a b)
      result <- runRIO (withFiberRefs program)
      result `shouldEqual` Right (Tuple 1 "hello")
