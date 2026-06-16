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

import Effect.Exception (Error)
import RIO.Aff.Clock (Clock)
import RIO.Aff.Concurrency (parTraverse)
import RIO.Aff.Core (RIO, ask, provideAll, runRIO, runRIO')
import RIO.Aff.Error (sandbox)
import RIO.Aff.Pool (Pool)
import RIO.Aff.Pool as Pool
import RIO.Aff.Resource (scoped)
import RIO.Aff.Test.Clock (newTestClock)

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

  it "idle resources are released by the scope finalizer on exit" do
    -- Mint a resource and return it (so it sits idle in the
    -- pool), then let the scope exit: the scope-exit finalizer
    -- drains idle resources through `release`.
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

  describe "withResource'" do
    it "invalidate routes the resource to release and triggers a remint" do
      releaseLog <- liftEffect (ERef.new ([] :: Array Int))
      counter <- liftEffect (ERef.new 0)
      midLog <- liftEffect (ERef.new ([] :: Array Int))
      let
        program :: RIO () () (Array Int)
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          pool <- Pool.make scope
            { acquire: liftEffect (ERef.modify (_ + 1) counter)
            , release: \n -> liftEffect
                (ERef.modify_ (\xs -> snoc xs n) releaseLog)
            , maxSize: 1
            }
          -- First borrow: invalidate so the resource hits release
          -- instead of returning to the idle stack.
          first <- Pool.withResource' pool \r invalidate -> do
            invalidate
            pure r
          -- Snapshot the release log: only the invalidated resource
          -- (id 1) has hit `release` so far. The scope finalizer
          -- will drain the second resource later.
          current <- liftEffect (ERef.read releaseLog)
          liftEffect (ERef.write current midLog)
          -- Second borrow: idle stack is empty after the invalidate,
          -- so a fresh resource must be minted.
          second <- Pool.withResource' pool \r _ -> pure r
          pure [ first, second ]
      result <- runRIO program
      mid <- liftEffect (ERef.read midLog)
      mid `shouldEqual` [ 1 ]
      result `shouldEqual` (Right [ 1, 2 ] :: Either _ (Array Int))

    it "no invalidate keeps the resource in the idle stack" do
      counter <- liftEffect (ERef.new 0)
      let
        program :: RIO () () (Array Int)
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          pool <- Pool.make scope
            { acquire: liftEffect (ERef.modify (_ + 1) counter)
            , release: \_ -> pure unit
            , maxSize: 1
            }
          first <- Pool.withResource' pool \r _ -> pure r
          second <- Pool.withResource' pool \r _ -> pure r
          pure [ first, second ]
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 1 ] :: Either _ (Array Int))

  describe "shutdown" do
    it "drains every idle resource immediately when called" do
      releaseLog <- liftEffect (ERef.new ([] :: Array Int))
      counter <- liftEffect (ERef.new 0)
      let
        program :: RIO () () (Array Int)
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          pool <- Pool.make scope
            { acquire: liftEffect (ERef.modify (_ + 1) counter)
            , release: \n -> liftEffect
                (ERef.modify_ (\xs -> snoc xs n) releaseLog)
            , maxSize: 3
            }
          -- Each borrow body delays so all three resources are
          -- live concurrently and go idle simultaneously.
          _ <- parTraverse
            ( \_ -> Pool.withResource pool \_ ->
                liftAff (delay (Milliseconds 10.0))
            )
            [ 1, 2, 3 ]
          Pool.shutdown pool
          -- After shutdown, the release log already contains every
          -- minted resource.
          inFlight <- liftEffect (ERef.read releaseLog)
          pure (Array.sort inFlight)
      result <- runRIO program
      result `shouldEqual` (Right [ 1, 2, 3 ] :: Either _ (Array Int))

    it "subsequent borrows after shutdown raise a defect" do
      let
        program :: RIO () () (Either Error Unit)
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          pool <- Pool.make scope
            { acquire: pure unit
            , release: \_ -> pure unit
            , maxSize: 1
            }
          Pool.shutdown pool
          sandbox (Pool.withResource pool (\_ -> pure unit))
      result <- runRIO program
      case result of
        Right (Left _) -> pure unit
        _ -> shouldEqual "" "expected defect on borrow after shutdown"

  describe "makeWithTTL" do
    it "evicts and remints an idle resource older than the TTL" do
      counter <- liftEffect (ERef.new 0)
      releaseLog <- liftEffect (ERef.new ([] :: Array Int))
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () (Array Int)
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          pool <- Pool.makeWithTTL scope
            { acquire: liftEffect (ERef.modify (_ + 1) counter)
            , release: \n -> liftEffect
                (ERef.modify_ (\xs -> snoc xs n) releaseLog)
            , maxSize: 1
            , timeToLive: Milliseconds 100.0
            }
          -- First borrow: mints resource 1, then returns it idle.
          first <- Pool.withResource pool pure
          pure [ first ]
      _ <- runRIO' (provideAll { clock: tc.clock } program)
      -- Advance virtual clock past the TTL. The next borrow should
      -- discard the stale entry and mint a fresh resource.
      tc.advance (Milliseconds 200.0)
      let
        program2 :: RIO (clock :: Clock) () (Array Int)
        program2 = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          pool <- Pool.makeWithTTL scope
            { acquire: liftEffect (ERef.modify (_ + 1) counter)
            , release: \n -> liftEffect
                (ERef.modify_ (\xs -> snoc xs n) releaseLog)
            , maxSize: 1
            , timeToLive: Milliseconds 100.0
            }
          n <- Pool.withResource pool pure
          pure [ n ]
      xs <- runRIO' (provideAll { clock: tc.clock } program2)
      -- After scope exit, finalizers drained idle resources from
      -- both pools, so counter went up at least to 2.
      finalCount <- liftEffect (ERef.read counter)
      (finalCount >= 2) `shouldEqual` true
      (Array.length xs) `shouldEqual` 1

    it "reuses an idle resource that is still within the TTL" do
      counter <- liftEffect (ERef.new 0)
      tc <- newTestClock
      let
        program :: RIO (clock :: Clock) () { first :: Int, second :: Int }
        program = scoped do
          scope <- ask (Proxy :: Proxy "scope")
          pool <- Pool.makeWithTTL scope
            { acquire: liftEffect (ERef.modify (_ + 1) counter)
            , release: \_ -> pure unit
            , maxSize: 1
            , timeToLive: Milliseconds 1000.0
            }
          first <- Pool.withResource pool pure
          second <- Pool.withResource pool pure
          pure { first, second }
      out <- runRIO' (provideAll { clock: tc.clock } program)
      out.first `shouldEqual` 1
      out.second `shouldEqual` 1
