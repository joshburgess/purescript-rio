module Test.RIO.Fiber.PoolSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_)
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Pool as Pool
import RIO.Fiber.TestClock as TestClock
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

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

  it "size reports the configured capacity" do
    let
      prog :: F.RIO () () Int
      prog = do
        pool <- Pool.make 5 (pure 0) (\_ -> pure unit)
        Pool.size pool
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 5
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "size clamps non-positive capacity to 1" do
    let
      prog :: F.RIO () () Int
      prog = do
        pool <- Pool.make 0 (pure 0) (\_ -> pure unit)
        Pool.size pool
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "available reports how many resources are currently idle" do
    let
      prog :: F.RIO () () { afterMake :: Int, afterUse :: Int }
      prog = do
        pool <- Pool.make 3 (pure 0) (\_ -> pure unit)
        afterMake <- Pool.available pool
        _ <- Pool.withResource pool pure
        afterUse <- Pool.available pool
        pure { afterMake, afterUse }
    out <- runAff prog {}
    case out of
      Success r -> do
        -- Empty pool starts with no idle resources (none created yet).
        r.afterMake `shouldEqual` 0
        -- After one borrow / release the resource is back in the queue.
        r.afterUse `shouldEqual` 1
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "resource returns to the pool after a failing use" do
    created <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () (boom :: String) Int
      acquire = F.liftEffect (Ref.modify (_ + 1) created)

      prog :: F.RIO () (boom :: String) Unit
      prog = do
        pool <- Pool.make 1 acquire (\_ -> pure unit)
        -- First use fails; the resource must be returned to the pool.
        _ <- F.catchAll (\_ -> pure 0)
          (Pool.withResource pool \_ ->
              F.fail (Variant.inj (Proxy :: _ "boom") "x")
          )
        -- Second use should reuse the same resource, not create a new one.
        _ <- Pool.withResource pool pure
        pure unit
    _ <- runAff prog {}
    n <- liftEffect (Ref.read created)
    n `shouldEqual` 1

  it "resource returns to the pool after an interrupted use" do
    created <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () () Int
      acquire = F.liftEffect (Ref.modify (_ + 1) created)

      prog :: F.RIO () () Unit
      prog = do
        pool <- Pool.make 1 acquire (\_ -> pure unit)
        f <- F.fork
          ( Pool.withResource pool \_ ->
              F.sleep (Milliseconds 1000.0)
          )
        F.sleep (Milliseconds 10.0)
        F.interrupt f
        _ <- F.join f
        -- Borrow again; should reuse the resource.
        _ <- Pool.withResource pool pure
        pure unit
    _ <- runAff prog {}
    n <- liftEffect (Ref.read created)
    n `shouldEqual` 1

  it "if create fails, the permit is released so the next borrow can run" do
    attempts <- liftEffect (Ref.new 0)
    let
      acquire :: F.RIO () (boom :: String) Int
      acquire = do
        n <- F.liftEffect (Ref.modify (_ + 1) attempts)
        if n == 1 then F.fail (Variant.inj (Proxy :: _ "boom") "first attempt")
        else pure 42

      prog
        :: F.RIO () (boom :: String) (Either (Cause (boom :: String)) Int)
      prog = do
        pool <- Pool.make 1 acquire (\_ -> pure unit)
        -- First borrow fails inside create; permit must be released.
        -- The Pool re-raises the failure via `failCause`, which
        -- becomes an M_CAUSE that `catchAll` does not catch, so we
        -- go through `causeOf` to recover from any shape of failure.
        _ <- F.causeOf (Pool.withResource pool pure)
        -- Second borrow should succeed.
        F.causeOf (Pool.withResource pool pure)
    out <- runAff prog {}
    case out of
      Success (Right n) -> n `shouldEqual` 42
      Success (Left _) ->
        fail "second borrow failed: permit was not released"
      other -> fail ("expected Success, got " <> describeOutcome other)
    a <- liftEffect (Ref.read attempts)
    a `shouldEqual` 2

  describe "makeWithTTL" do
    it "evicts and recreates a resource whose idle age exceeds the TTL" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      created <- liftEffect (Ref.new 0)
      destroyed <- liftEffect (Ref.new 0)
      let
        acquire :: F.RIO () () Int
        acquire = F.liftEffect (Ref.modify (_ + 1) created)

        release :: Int -> F.RIO () () Unit
        release _ = F.liftEffect (Ref.modify_ (_ + 1) destroyed)

        prog :: F.RIO () () Unit
        prog = Clock.withClock (TestClock.clock tc) do
          pool <- Pool.makeWithTTL
            { capacity: 1
            , create: acquire
            , destroy: release
            , timeToLive: Milliseconds 100.0
            }
          _ <- Pool.withResource pool pure
          TestClock.advance tc (Milliseconds 250.0)
          _ <- Pool.withResource pool pure
          pure unit
      _ <- runAff prog {}
      c <- liftEffect (Ref.read created)
      d <- liftEffect (Ref.read destroyed)
      c `shouldEqual` 2
      d `shouldEqual` 1

    it "reuses a resource that is still within the TTL" do
      tc <- liftEffect (TestClock.make (Milliseconds 0.0))
      created <- liftEffect (Ref.new 0)
      let
        acquire :: F.RIO () () Int
        acquire = F.liftEffect (Ref.modify (_ + 1) created)

        prog :: F.RIO () () Unit
        prog = Clock.withClock (TestClock.clock tc) do
          pool <- Pool.makeWithTTL
            { capacity: 1
            , create: acquire
            , destroy: \_ -> pure unit
            , timeToLive: Milliseconds 100.0
            }
          _ <- Pool.withResource pool pure
          TestClock.advance tc (Milliseconds 50.0)
          _ <- Pool.withResource pool pure
          pure unit
      _ <- runAff prog {}
      c <- liftEffect (Ref.read created)
      c `shouldEqual` 1

  describe "withResource'" do
    it "invalidate causes the resource to be destroyed and not recycled" do
      created <- liftEffect (Ref.new 0)
      destroyed <- liftEffect (Ref.new ([] :: Array Int))
      let
        acquire :: F.RIO () () Int
        acquire = F.liftEffect (Ref.modify (_ + 1) created)

        release :: Int -> F.RIO () () Unit
        release r = F.liftEffect (Ref.modify_ (\xs -> xs <> [ r ]) destroyed)

        prog :: F.RIO () () Unit
        prog = do
          pool <- Pool.make 1 acquire release
          Pool.withResource' pool \_ invalidate -> invalidate
          -- The first resource was invalidated. The next borrow must
          -- create a brand new one.
          _ <- Pool.withResource pool pure
          pure unit
      _ <- runAff prog {}
      c <- liftEffect (Ref.read created)
      d <- liftEffect (Ref.read destroyed)
      c `shouldEqual` 2
      d `shouldEqual` [ 1 ]

    it "without invalidate the resource returns to the pool" do
      created <- liftEffect (Ref.new 0)
      let
        acquire :: F.RIO () () Int
        acquire = F.liftEffect (Ref.modify (_ + 1) created)

        prog :: F.RIO () () Unit
        prog = do
          pool <- Pool.make 1 acquire (\_ -> pure unit)
          Pool.withResource' pool \_ _ -> pure unit
          _ <- Pool.withResource pool pure
          pure unit
      _ <- runAff prog {}
      c <- liftEffect (Ref.read created)
      c `shouldEqual` 1

  it "shutdown is idempotent" do
    destroyed <- liftEffect (Ref.new 0)
    let
      prog :: F.RIO () () Unit
      prog = do
        pool <- Pool.make 2 (pure 1) (\_ -> F.liftEffect (Ref.modify_ (_ + 1) destroyed))
        _ <- Pool.withResource pool pure
        Pool.shutdown pool
        Pool.shutdown pool
        Pool.shutdown pool
    _ <- runAff prog {}
    n <- liftEffect (Ref.read destroyed)
    -- Only one resource was created, destroyed exactly once.
    n `shouldEqual` 1

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
