module Test.RIO.Fiber.KeyedPoolSpec (spec) where

import Prelude

import Data.Array (sort)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.KeyedPool as KP
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: KeyedPool" do
  it "creates one pool per key on first use and reuses it after" do
    counts <- liftEffect (Ref.new (0 :: Int))
    let
      acquire :: String -> F.RIO () () { key :: String, id :: Int }
      acquire k = F.liftEffect do
        n <- Ref.modify (_ + 1) counts
        pure { key: k, id: n }

      release :: { key :: String, id :: Int } -> F.RIO () () Unit
      release _ = pure unit

      prog :: F.RIO () () { a1 :: Int, a2 :: Int, b1 :: Int, total :: Int }
      prog = do
        kp <- KP.make 2 acquire release
        a1 <- KP.withResource kp "a" \r -> pure r.id
        a2 <- KP.withResource kp "a" \r -> pure r.id
        b1 <- KP.withResource kp "b" \r -> pure r.id
        total <- F.liftEffect (Ref.read counts)
        pure { a1, a2, b1, total }
    out <- runAff prog {}
    case out of
      Success r -> do
        -- "a" reuses its single resource; "b" allocates its own.
        r.a1 `shouldEqual` 1
        r.a2 `shouldEqual` 1
        r.b1 `shouldEqual` 2
        r.total `shouldEqual` 2
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "caps concurrent borrowers per key independently" do
    active <- liftEffect (Ref.new (0 :: Int))
    peak <- liftEffect (Ref.new (0 :: Int))
    let
      acquire :: String -> F.RIO () () Unit
      acquire _ = pure unit

      borrow :: KP.KeyedPool () () String Unit -> String -> F.RIO () () Unit
      borrow kp k = KP.withResource kp k \_ -> do
        F.liftEffect do
          n <- Ref.modify (_ + 1) active
          p <- Ref.read peak
          when (n > p) (Ref.write n peak)
        F.sleep (Milliseconds 5.0)
        F.liftEffect (Ref.modify_ (_ - 1) active)

      prog :: F.RIO () () Unit
      prog = do
        kp <- KP.make 1 acquire (\_ -> pure unit)
        -- 3 borrows against "a" and 3 against "b" in parallel; each
        -- pool has capacity 1, so at most 2 borrows ever overlap.
        _ <- F.parTraverse (borrow kp) [ "a", "a", "a", "b", "b", "b" ]
        pure unit
    _ <- runAff prog {}
    pk <- liftEffect (Ref.read peak)
    pk `shouldEqual` 2

  it "shutdown destroys every idle resource across keys" do
    destroyed <- liftEffect (Ref.new ([] :: Array String))
    let
      acquire :: String -> F.RIO () () String
      acquire k = pure k

      release :: String -> F.RIO () () Unit
      release r = F.liftEffect (Ref.modify_ (\xs -> xs <> [ r ]) destroyed)

      prog :: F.RIO () () Unit
      prog = do
        kp <- KP.make 2 acquire release
        _ <- traverse (\k -> KP.withResource kp k \_ -> pure unit)
          [ "x", "y", "z" ]
        KP.shutdown kp
    _ <- runAff prog {}
    rs <- liftEffect (Ref.read destroyed)
    sort rs `shouldEqual` [ "x", "y", "z" ]

  it "keys reports every key that has been borrowed" do
    let
      prog :: F.RIO () () (Array String)
      prog = do
        kp <- KP.make 1 (\k -> pure k) (\_ -> pure unit)
        for_ [ "alpha", "beta", "gamma" ] \k ->
          KP.withResource kp k \_ -> pure unit
        ks <- KP.keys kp
        pure (Set.toUnfoldable ks)
    out <- runAff prog {}
    case out of
      Success xs -> sort xs `shouldEqual` [ "alpha", "beta", "gamma" ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "makeWithTTL applies per-key capacity from the supplied function" do
    active <- liftEffect (Ref.new (0 :: Int))
    peak <- liftEffect (Ref.new (0 :: Int))
    let
      borrow
        :: KP.KeyedPool () () String Unit
        -> String
        -> F.RIO () () Unit
      borrow kp k = KP.withResource kp k \_ -> do
        F.liftEffect do
          n <- Ref.modify (_ + 1) active
          p <- Ref.read peak
          when (n > p) (Ref.write n peak)
        F.sleep (Milliseconds 5.0)
        F.liftEffect (Ref.modify_ (_ - 1) active)

      prog :: F.RIO () () Unit
      prog = do
        kp <- KP.makeWithTTL
          { capacity: \k -> if k == "hot" then 3 else 1
          , create: \_ -> pure unit
          , destroy: \_ -> pure unit
          , timeToLive: Nothing
          }
        -- 5 concurrent borrows against "hot" should permit up to 3
        -- simultaneously.
        _ <- F.parTraverse (borrow kp) [ "hot", "hot", "hot", "hot", "hot" ]
        pure unit
    _ <- runAff prog {}
    pk <- liftEffect (Ref.read peak)
    pk `shouldEqual` 3

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
