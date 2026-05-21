module Test.RIO.Aff.RcRefSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Effect.Ref as ERef
import RIO.Aff.Core (RIO, ask, runRIO)
import RIO.Aff.Error (either, fail)
import RIO.Aff.RcRef as RcRef
import RIO.Aff.Resource (scoped)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

type Boom = (boom :: String)

spec :: Spec Unit
spec = describe "RIO.Aff.RcRef" do
  it "acquires lazily on the first get and releases when the scope closes" do
    acquires <- liftEffect (ERef.new 0)
    releases <- liftEffect (ERef.new 0)
    let
      acquire :: RIO () () Int
      acquire = liftEffect
        (ERef.modify' (\n -> { state: n + 1, value: 42 }) acquires)

      release :: Int -> RIO () () Unit
      release _ = liftEffect (ERef.modify_ (_ + 1) releases)

      program :: RIO () () { acquires :: Int, releases :: Int, count :: Int, v :: Int }
      program = do
        rc <- RcRef.make { acquire, release }
        a0 <- liftEffect (ERef.read acquires)
        v <- scoped do
          scope <- ask (Proxy :: Proxy "scope")
          RcRef.get scope rc
        a1 <- liftEffect (ERef.read acquires)
        r1 <- liftEffect (ERef.read releases)
        count <- RcRef.refCount rc
        pure { acquires: a1 - a0, releases: r1, count, v }
    result <- runRIO program
    result `shouldEqual`
      (Right { acquires: 1, releases: 1, count: 0, v: 42 } :: Either _ _)

  it "shares one acquire across overlapping scopes" do
    acquires <- liftEffect (ERef.new 0)
    releases <- liftEffect (ERef.new 0)
    insideReleases <- liftEffect (ERef.new (-1))
    let
      acquire :: RIO () () Int
      acquire = liftEffect
        (ERef.modify' (\n -> { state: n + 1, value: 99 }) acquires)

      release :: Int -> RIO () () Unit
      release _ = liftEffect (ERef.modify_ (_ + 1) releases)

      program :: RIO () () { totalAcquires :: Int, totalReleases :: Int, midReleases :: Int }
      program = do
        rc <- RcRef.make { acquire, release }
        scoped do
          outer <- ask (Proxy :: Proxy "scope")
          _ <- RcRef.get outer rc
          scoped do
            inner <- ask (Proxy :: Proxy "scope")
            _ <- RcRef.get inner rc
            pure unit
          -- inner closed, outer still alive: release not yet run
          r <- liftEffect (ERef.read releases)
          liftEffect (ERef.write r insideReleases)
        a' <- liftEffect (ERef.read acquires)
        r' <- liftEffect (ERef.read releases)
        mid <- liftEffect (ERef.read insideReleases)
        pure { totalAcquires: a', totalReleases: r', midReleases: mid }
    result <- runRIO program
    result `shouldEqual`
      (Right { totalAcquires: 1, totalReleases: 1, midReleases: 0 } :: Either _ _)

  it "re-acquires after dropping to zero" do
    acquires <- liftEffect (ERef.new 0)
    releases <- liftEffect (ERef.new 0)
    let
      acquire :: RIO () () Int
      acquire = liftEffect
        (ERef.modify' (\n -> { state: n + 1, value: n + 1 }) acquires)

      release :: Int -> RIO () () Unit
      release _ = liftEffect (ERef.modify_ (_ + 1) releases)

      program :: RIO () () { acquires :: Int, releases :: Int, v1 :: Int, v2 :: Int }
      program = do
        rc <- RcRef.make { acquire, release }
        v1 <- scoped do
          s <- ask (Proxy :: Proxy "scope")
          RcRef.get s rc
        v2 <- scoped do
          s <- ask (Proxy :: Proxy "scope")
          RcRef.get s rc
        a <- liftEffect (ERef.read acquires)
        r <- liftEffect (ERef.read releases)
        pure { acquires: a, releases: r, v1, v2 }
    result <- runRIO program
    result `shouldEqual`
      (Right { acquires: 2, releases: 2, v1: 1, v2: 2 } :: Either _ _)

  it "leaves the cell empty after a failed acquire so the next get retries" do
    acquires <- liftEffect (ERef.new 0)
    let
      acquire :: RIO () Boom Int
      acquire = do
        n <- liftEffect
          (ERef.modify' (\k -> { state: k + 1, value: k + 1 }) acquires)
        if n == 1 then fail (Proxy :: Proxy "boom") "first"
        else pure n

      release :: Int -> RIO () () Unit
      release _ = pure unit

      program :: RIO () Boom Int
      program = do
        rc <- RcRef.make { acquire, release }
        -- First attempt fails; swallow via `either` so the second
        -- attempt can run and observe the cell having been left empty.
        _ <- either (scoped do
          s <- ask (Proxy :: Proxy "scope")
          RcRef.get s rc)
        scoped do
          s <- ask (Proxy :: Proxy "scope")
          RcRef.get s rc

    result <- runRIO program
    case result of
      Right n -> n `shouldEqual` 2
      Left _ -> 1 `shouldEqual` 0

  it "refCount tracks live references" do
    let
      program :: RIO () () (Array Int)
      program = do
        rc <- RcRef.make { acquire: pure 0, release: \_ -> pure unit }
        c0 <- RcRef.refCount rc
        c1 <- scoped do
          s <- ask (Proxy :: Proxy "scope")
          _ <- RcRef.get s rc
          RcRef.refCount rc
        c2 <- RcRef.refCount rc
        pure [ c0, c1, c2 ]
    result <- runRIO program
    result `shouldEqual` (Right [ 0, 1, 0 ] :: Either _ _)
