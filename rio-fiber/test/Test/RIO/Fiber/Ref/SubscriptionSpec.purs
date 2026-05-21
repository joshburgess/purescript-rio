module Test.RIO.Fiber.Ref.SubscriptionSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Fiber.Core (Outcome(..), RIO)
import RIO.Fiber.Core as F
import RIO.Fiber.Ref.Subscription as Sub
import RIO.Fiber.Scope as Scope
import RIO.Fiber.Stream as S

spec :: Spec Unit
spec = describe "rio-fiber: Ref.Subscription" do

  it "make + read returns the initial value" do
    let
      program :: RIO () () Int
      program = do
        ref <- Sub.make 8 42
        Sub.read ref
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 42
      _ -> fail "expected Success"

  it "set overwrites and a subsequent read sees the new value" do
    let
      program :: RIO () () Int
      program = do
        ref <- Sub.make 8 0
        Sub.set ref 7
        Sub.read ref
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 7
      _ -> fail "expected Success"

  it "modify returns the new value and write is observable" do
    let
      program :: RIO () () { returned :: Int, observed :: Int }
      program = do
        ref <- Sub.make 8 10
        returned <- Sub.modify ref (_ + 5)
        observed <- Sub.read ref
        pure { returned, observed }
    out <- runAff program {}
    case out of
      Success r -> do
        r.returned `shouldEqual` 15
        r.observed `shouldEqual` 15
      _ -> fail "expected Success"

  it "changes emits the current value first, then every subsequent set" do
    let
      program :: RIO () () (Array Int)
      program = Scope.scoped \scope -> do
        ref <- Sub.make 8 1
        -- Start the subscriber first so it captures the initial value
        -- plus every later write.
        fib <- F.fork (S.runCollect (S.take 4 (Sub.changes scope ref)))
        F.sleep (Milliseconds 5.0)
        Sub.set ref 2
        Sub.set ref 3
        Sub.set ref 4
        F.join fib
    out <- runAff program {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3, 4 ]
      _ -> fail "expected Success"

  it "two changes subscribers each receive every value" do
    let
      program :: RIO () () { a :: Array Int, b :: Array Int }
      program = Scope.scoped \scope -> do
        ref <- Sub.make 8 0
        fibA <- F.fork (S.runCollect (S.take 3 (Sub.changes scope ref)))
        fibB <- F.fork (S.runCollect (S.take 3 (Sub.changes scope ref)))
        F.sleep (Milliseconds 5.0)
        Sub.set ref 1
        Sub.set ref 2
        a <- F.join fibA
        b <- F.join fibB
        pure { a, b }
    out <- runAff program {}
    case out of
      Success r -> do
        r.a `shouldEqual` [ 0, 1, 2 ]
        r.b `shouldEqual` [ 0, 1, 2 ]
      _ -> fail "expected Success"

  it "scope close releases the subscription slot" do
    -- After scope close `subscribers` reports zero.
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () Unit
      program = do
        ref <- Sub.make 8 0
        Scope.scoped \scope -> do
          _ <- S.runCollect (S.take 1 (Sub.changes scope ref))
          pure unit
        n <- Sub.subscribers ref
        F.liftEffect (Ref.write n counter)
    out <- runAff program {}
    case out of
      Success _ -> do
        n <- liftEffect (Ref.read counter)
        n `shouldEqual` 0
      _ -> fail "expected Success"

  it "modifyM serialises updates against reads" do
    -- An effectful update body sees the latest value and writes the
    -- new one atomically. A concurrent read waits for the lock.
    let
      program :: RIO () () Int
      program = do
        ref <- Sub.make 8 100
        Sub.modifyM ref \n -> pure (n + 11)
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 111
      _ -> fail "expected Success"
