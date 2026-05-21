module Test.RIO.Fiber.RcRefSpec (spec) where

import Prelude

import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.RcRef as RcRef
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

type Boom = (boom :: String)

spec :: Spec Unit
spec = describe "rio-fiber: RcRef" do
  it "acquires lazily on the first get and releases when the scope closes" do
    acquires <- liftEffect (Ref.new 0)
    releases <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () () Int
      acquire = F.liftEffect
        (Ref.modify' (\n -> { state: n + 1, value: 42 }) acquires)

      release :: Int -> F.RIO () () Unit
      release _ = F.liftEffect (Ref.modify_ (_ + 1) releases)

      prog :: F.RIO () () { acquires :: Int, releases :: Int, count :: Int, v :: Int }
      prog = do
        rc <- RcRef.make { acquire, release }
        a0 <- F.liftEffect (Ref.read acquires)
        v <- Scope.scoped \s -> RcRef.get s rc
        a1 <- F.liftEffect (Ref.read acquires)
        r1 <- F.liftEffect (Ref.read releases)
        count <- RcRef.refCount rc
        pure { acquires: a1 - a0, releases: r1, count, v }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.acquires `shouldEqual` 1
        rec.releases `shouldEqual` 1
        rec.count `shouldEqual` 0
        rec.v `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "shares one acquire across overlapping scopes" do
    acquires <- liftEffect (Ref.new 0)
    releases <- liftEffect (Ref.new 0)
    insideReleases <- liftEffect (Ref.new (-1))
    let
      acquire :: F.RIO () () Int
      acquire = F.liftEffect
        (Ref.modify' (\n -> { state: n + 1, value: 99 }) acquires)

      release :: Int -> F.RIO () () Unit
      release _ = F.liftEffect (Ref.modify_ (_ + 1) releases)

      prog :: F.RIO () () { totalAcquires :: Int, totalReleases :: Int, midReleases :: Int }
      prog = do
        rc <- RcRef.make { acquire, release }
        Scope.scoped \outer -> do
          _ <- RcRef.get outer rc
          Scope.scoped \inner -> do
            _ <- RcRef.get inner rc
            pure unit
          -- inner scope closed, outer still alive; release not yet run
          r <- F.liftEffect (Ref.read releases)
          F.liftEffect (Ref.write r insideReleases)
        a' <- F.liftEffect (Ref.read acquires)
        r' <- F.liftEffect (Ref.read releases)
        mid <- F.liftEffect (Ref.read insideReleases)
        pure { totalAcquires: a', totalReleases: r', midReleases: mid }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.totalAcquires `shouldEqual` 1
        rec.totalReleases `shouldEqual` 1
        rec.midReleases `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "re-acquires after dropping to zero" do
    acquires <- liftEffect (Ref.new 0)
    releases <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () () Int
      acquire = F.liftEffect
        (Ref.modify' (\n -> { state: n + 1, value: n + 1 }) acquires)

      release :: Int -> F.RIO () () Unit
      release _ = F.liftEffect (Ref.modify_ (_ + 1) releases)

      prog :: F.RIO () () { acquires :: Int, releases :: Int, v1 :: Int, v2 :: Int }
      prog = do
        rc <- RcRef.make { acquire, release }
        v1 <- Scope.scoped \s -> RcRef.get s rc
        v2 <- Scope.scoped \s -> RcRef.get s rc
        a <- F.liftEffect (Ref.read acquires)
        r <- F.liftEffect (Ref.read releases)
        pure { acquires: a, releases: r, v1, v2 }

    out <- runAff prog {}
    case out of
      Success rec -> do
        rec.acquires `shouldEqual` 2
        rec.releases `shouldEqual` 2
        rec.v1 `shouldEqual` 1
        rec.v2 `shouldEqual` 2
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "leaves the cell empty after a failed acquire so the next get retries" do
    acquires <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () Boom Int
      acquire = do
        n <- F.liftEffect
          (Ref.modify' (\k -> { state: k + 1, value: k + 1 }) acquires)
        if n == 1 then F.fail (Variant.inj (Proxy :: Proxy "boom") "first")
        else pure n

      release :: Int -> F.RIO () Boom Unit
      release _ = pure unit

      prog :: F.RIO () Boom Int
      prog = do
        rc <- RcRef.make { acquire, release }
        -- first attempt fails; swallow via causeOf (catchAll cannot
        -- catch failures re-raised by Scope.scoped because the
        -- runtime promotes them to M_CAUSE mode)
        _ <- F.causeOf (Scope.scoped \s -> RcRef.get s rc)
        -- second attempt should re-run acquire and succeed
        Scope.scoped \s -> RcRef.get s rc

    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 2
      Fail v -> Variant.match { boom: \s -> fail ("got Fail boom: " <> s) } v
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "refCount tracks live references" do
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        rc <- RcRef.make { acquire: pure 0, release: \_ -> pure unit }
        c0 <- RcRef.refCount rc
        c1 <- Scope.scoped \s -> do
          _ <- RcRef.get s rc
          RcRef.refCount rc
        c2 <- RcRef.refCount rc
        pure [ c0, c1, c2 ]

    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 0, 1, 0 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
