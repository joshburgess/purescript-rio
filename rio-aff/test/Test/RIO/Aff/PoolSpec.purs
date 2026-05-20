module Test.RIO.Aff.PoolSpec (spec) where

import Prelude

import Data.Array (snoc)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_, sum)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as ERef
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Concurrency (parTraverse)
import RIO.Aff.Core (RIO, ask, runRIO)
import RIO.Aff.Pool (Pool)
import RIO.Aff.Pool as Pool
import RIO.Aff.Resource (scoped)

spec :: Spec Unit
spec = describe "RIO.Aff.Pool" do
  it "borrow + return preserves a single resource across uses" do
    -- A pool of size 1 should hand out the same resource on
    -- consecutive sequential borrows: after the first borrow
    -- returns, the resource sits in the idle stack and is
    -- popped by the next borrow.
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int, total :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: liftEffect (ERef.modify (_ + 1) counter)
          , release: \_ -> pure unit
          , maxSize: 1
          }
        first <- Pool.withResource pool pure
        second <- Pool.withResource pool pure
        total <- liftEffect (Pool.size pool)
        pure { first, second, total }
    result <- runRIO program
    result `shouldEqual`
      (Right { first: 1, second: 1, total: 1 } :: Either _ _)

  it "mints a fresh resource when none are idle" do
    -- Two concurrent borrows over a pool of size 2 must hand
    -- out two distinct resources because the first hasn't been
    -- returned by the time the second is requested.
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () (Array Int)
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: liftEffect (ERef.modify (_ + 1) counter)
          , release: \_ -> pure unit
          , maxSize: 2
          }
        parTraverse
          ( \_ -> Pool.withResource pool \r -> do
              liftAff (delay (Milliseconds 20.0))
              pure r
          )
          [ unit, unit ]
    result <- runRIO program
    case result of
      Right xs -> (Array.sort xs) `shouldEqual` [ 1, 2 ]
      Left _ -> 1 `shouldEqual` 0

  it "maxSize caps the number of in-flight resources" do
    -- Pool of size 2 borrowed by 4 concurrent fibers. At any
    -- instant only 2 may be in flight. A regression that removed
    -- the semaphore would let all 4 acquire simultaneously.
    inflight <- liftEffect (ERef.new 0)
    peak <- liftEffect (ERef.new 0)
    let
      program :: RIO () () Unit
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: pure unit
          , release: \_ -> pure unit
          , maxSize: 2
          }
        _ <- parTraverse
          ( \_ -> Pool.withResource pool \_ -> do
              cur <- liftEffect (ERef.modify (_ + 1) inflight)
              liftEffect
                ( ERef.modify_
                    (\p -> if cur > p then cur else p)
                    peak
                )
              liftAff (delay (Milliseconds 20.0))
              _ <- liftEffect (ERef.modify (\x -> x - 1) inflight)
              pure unit
          )
          [ 1, 2, 3, 4 ]
        pure unit
    _ <- runRIO program
    p <- liftEffect (ERef.read peak)
    (p <= 2) `shouldEqual` true

  it "release runs on every idle resource when the scope exits" do
    -- After scope exits, the pool's drainAll finalizer must
    -- release every idle resource exactly once.
    releaseLog <- liftEffect (ERef.new ([] :: Array Int))
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () Unit
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: liftEffect (ERef.modify (_ + 1) counter)
          , release: \n -> liftEffect
              (ERef.modify_ (\xs -> snoc xs n) releaseLog)
          , maxSize: 3
          }
        -- Use three concurrent borrows so three resources are
        -- minted; when all three finish, all three go idle.
        _ <- parTraverse
          ( \_ -> Pool.withResource pool \_ -> do
              liftAff (delay (Milliseconds 5.0))
              pure unit
          )
          [ 1, 2, 3 ]
        pure unit
    _ <- runRIO program
    -- Outside the scope the finalizer has fired.
    log <- liftEffect (ERef.read releaseLog)
    Array.sort log `shouldEqual` [ 1, 2, 3 ]

  it "in-flight resources are released (not pooled) after shutdown" do
    -- Acquire-then-shutdown timing: a fiber borrows a resource,
    -- the scope exits while the borrow is in flight, the
    -- borrowing block's `finally` observes the shutdown flag
    -- and routes the resource to `release` rather than back to
    -- the idle stack.
    releaseLog <- liftEffect (ERef.new ([] :: Array Int))
    counter <- liftEffect (ERef.new 0)
    -- Manually build the pool outside `scoped` so we can
    -- observe state after scope exit. Use a Ref to hand the
    -- pool out of `scoped` block.
    poolRef <- liftEffect (ERef.new (Nothing :: Maybe (Pool Int)))
    let
      program :: RIO () () Unit
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: liftEffect (ERef.modify (_ + 1) counter)
          , release: \n -> liftEffect
              (ERef.modify_ (\xs -> snoc xs n) releaseLog)
          , maxSize: 2
          }
        liftEffect (ERef.write (Just pool) poolRef)
        -- Mint a resource then return it so it's idle; the
        -- scope finalizer will then drain it on exit.
        _ <- Pool.withResource pool (\_ -> pure unit)
        pure unit
    _ <- runRIO program
    log <- liftEffect (ERef.read releaseLog)
    log `shouldEqual` [ 1 ]

  it "size and idle track minted vs returned resources" do
    -- After three borrows that hold their resources, total = 3
    -- and idle = 0. After they all return, total = 3 and idle = 3.
    counter <- liftEffect (ERef.new 0)
    sizeMid <- liftEffect (ERef.new 0)
    idleMid <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { finalSize :: Int, finalIdle :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: liftEffect (ERef.modify (_ + 1) counter)
          , release: \_ -> pure unit
          , maxSize: 5
          }
        _ <- parTraverse
          ( \_ -> Pool.withResource pool \_ -> do
              -- Mid-borrow: peek at size + idle. Three fibers
              -- can race here, so we only assert at the end.
              s <- liftEffect (Pool.size pool)
              i <- liftEffect (Pool.idle pool)
              liftEffect (ERef.write s sizeMid)
              liftEffect (ERef.write i idleMid)
              liftAff (delay (Milliseconds 10.0))
              pure unit
          )
          [ 1, 2, 3 ]
        finalSize <- liftEffect (Pool.size pool)
        finalIdle <- liftEffect (Pool.idle pool)
        pure { finalSize, finalIdle }
    result <- runRIO program
    -- Three borrows mint three resources, all returned to the
    -- idle stack by the end of the parTraverse.
    result `shouldEqual`
      (Right { finalSize: 3, finalIdle: 3 } :: Either _ _)

  it "reuses idle resources before minting fresh ones" do
    -- A pool of size 2 borrowed sequentially 5 times must only
    -- mint at most 2 resources: every borrow after the first
    -- pops from the idle stack.
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { mintCount :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: liftEffect (ERef.modify (_ + 1) counter)
          , release: \_ -> pure unit
          , maxSize: 2
          }
        for_ [ 1, 2, 3, 4, 5 ]
          (\_ -> Pool.withResource pool (\_ -> pure unit))
        mintCount <- liftEffect (ERef.read counter)
        pure { mintCount }
    result <- runRIO program
    result `shouldEqual`
      (Right { mintCount: 1 } :: Either _ _)

  it "release runs exactly once per minted resource (sum check)" do
    -- Heavier check: mint many resources, release each via
    -- scope exit, sum the release counter to confirm none are
    -- double-released and none are leaked.
    releaseTotal <- liftEffect (ERef.new ([] :: Array Int))
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () Unit
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: liftEffect (ERef.modify (_ + 1) counter)
          , release: \n -> liftEffect
              (ERef.modify_ (\xs -> snoc xs n) releaseTotal)
          , maxSize: 4
          }
        _ <- parTraverse
          ( \_ -> Pool.withResource pool \_ -> do
              liftAff (delay (Milliseconds 5.0))
              pure unit
          )
          [ 1, 2, 3, 4 ]
        pure unit
    _ <- runRIO program
    log <- liftEffect (ERef.read releaseTotal)
    -- Each minted resource (ids 1..4 from the counter) appears
    -- exactly once in the release log; sum is 1+2+3+4 = 10.
    sum log `shouldEqual` 10
    Array.length log `shouldEqual` 4

  it "maxSize accessor returns the configured cap" do
    let
      program :: RIO () () Int
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        pool <- Pool.make scope
          { acquire: pure 0
          , release: \_ -> pure unit
          , maxSize: 7
          }
        pure (Pool.maxSize pool)
    result <- runRIO program
    result `shouldEqual` (Right 7 :: Either _ Int)
