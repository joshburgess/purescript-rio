module Test.RIO.Fiber.PoolSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Pool as Pool
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Pool" do
  it "creates a resource on first borrow and reuses on the next" do
    created <- liftEffect (Ref.new 0)
    destroyed <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () () Int
      acquire = F.liftEffect (Ref.modify (_ + 1) created)

      release :: Int -> F.RIO () () Unit
      release _ = F.liftEffect (Ref.modify_ (_ + 1) destroyed)

      prog :: F.RIO () () { a :: Int, b :: Int }
      prog = do
        pool <- Pool.make 2 acquire release
        a <- Pool.withResource pool pure
        b <- Pool.withResource pool pure
        pure { a, b }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.a `shouldEqual` 1
        r.b `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)
    n <- liftEffect (Ref.read created)
    n `shouldEqual` 1

  it "caps simultaneous borrowers at the configured capacity" do
    active <- liftEffect (Ref.new 0)
    peak <- liftEffect (Ref.new 0)
    let
      bump :: F.RIO () () Unit
      bump = F.liftEffect do
        n <- Ref.modify (_ + 1) active
        p <- Ref.read peak
        when (n > p) (Ref.write n peak)

      unbump :: F.RIO () () Unit
      unbump = F.liftEffect (Ref.modify_ (_ - 1) active)

      borrow :: Pool.Pool () () Int -> Int -> F.RIO () () Unit
      borrow pool _ = Pool.withResource pool \_ -> do
        bump
        F.sleep (Milliseconds 5.0)
        unbump

      prog :: F.RIO () () Unit
      prog = do
        pool <- Pool.make 2 (pure 0) (\_ -> pure unit)
        _ <- F.parTraverse (borrow pool) [ 1, 2, 3, 4, 5, 6 ]
        pure unit
    _ <- runAff prog {}
    pk <- liftEffect (Ref.read peak)
    pk `shouldEqual` 2

  it "shutdown destroys every idle resource" do
    destroyed <- liftEffect (Ref.new ([] :: Array Int))
    let
      release :: Int -> F.RIO () () Unit
      release r = F.liftEffect (Ref.modify_ (\xs -> xs <> [ r ]) destroyed)

      prog :: F.RIO () () Unit
      prog = do
        pool <- Pool.make 3 (pure 42) release
        for_ [ 1, 2, 3 ] (\_ -> Pool.withResource pool \_ -> pure unit)
        Pool.shutdown pool
    _ <- runAff prog {}
    rs <- liftEffect (Ref.read destroyed)
    -- only one resource is ever created (reused across three borrows),
    -- and shutdown destroys it exactly once.
    rs `shouldEqual` [ 42 ]

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
