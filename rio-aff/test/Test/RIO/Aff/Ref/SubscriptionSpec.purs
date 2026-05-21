module Test.RIO.Aff.Ref.SubscriptionSpec (spec) where

import Prelude hiding (join)

import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, fork, join, runRIO')
import RIO.Aff.Ref.Subscription as Sub
import RIO.Aff.Resource (scoped)
import RIO.Aff.Stream as S

spec :: Spec Unit
spec = describe "RIO.Aff.Ref.Subscription" do
  it "make + read returns the initial value" do
    let
      program :: RIO () () Int
      program = do
        ref <- Sub.make 42
        Sub.read ref
    n <- runRIO' program
    n `shouldEqual` 42

  it "set overwrites and a subsequent read sees the new value" do
    let
      program :: RIO () () Int
      program = do
        ref <- Sub.make 0
        Sub.set ref 7
        Sub.read ref
    n <- runRIO' program
    n `shouldEqual` 7

  it "modify returns the new value and write is observable" do
    let
      program :: RIO () () { returned :: Int, observed :: Int }
      program = do
        ref <- Sub.make 10
        returned <- Sub.modify ref (_ + 5)
        observed <- Sub.read ref
        pure { returned, observed }
    r <- runRIO' program
    r.returned `shouldEqual` 15
    r.observed `shouldEqual` 15

  it "changes emits the current value first, then every subsequent set" do
    let
      program :: RIO () () (Array Int)
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        ref <- Sub.make 1
        fib <- fork (S.runCollect (S.take 4 (Sub.changes scope ref)))
        liftAff (Aff.delay (Milliseconds 5.0))
        Sub.set ref 2
        Sub.set ref 3
        Sub.set ref 4
        join fib
    xs <- runRIO' program
    xs `shouldEqual` [ 1, 2, 3, 4 ]

  it "two changes subscribers each receive every value" do
    let
      program :: RIO () () { a :: Array Int, b :: Array Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        ref <- Sub.make 0
        fibA <- fork (S.runCollect (S.take 3 (Sub.changes scope ref)))
        fibB <- fork (S.runCollect (S.take 3 (Sub.changes scope ref)))
        liftAff (Aff.delay (Milliseconds 5.0))
        Sub.set ref 1
        Sub.set ref 2
        a <- join fibA
        b <- join fibB
        pure { a, b }
    r <- runRIO' program
    r.a `shouldEqual` [ 0, 1, 2 ]
    r.b `shouldEqual` [ 0, 1, 2 ]

  it "scope close releases the subscription slot" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () Unit
      program = do
        ref <- Sub.make 0
        scoped do
          scope <- ask (Proxy :: Proxy "scope")
          _ <- S.runCollect (S.take 1 (Sub.changes scope ref))
          pure unit
        -- One tick for the finalizer to detach the subscription.
        liftAff (Aff.delay (Milliseconds 5.0))
        n <- Sub.subscribers ref
        liftEffect (Ref.write n counter)
    _ <- runRIO' program
    n <- liftEffect (Ref.read counter)
    n `shouldEqual` 0

  it "modifyM serialises updates against reads" do
    let
      program :: RIO () () Int
      program = do
        ref <- Sub.make 100
        Sub.modifyM ref \n -> pure (n + 11)
    n <- runRIO' program
    n `shouldEqual` 111
