module Test.RIO.Fiber.CacheSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Ref as Ref
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Fiber.Cache as Cache
import RIO.Fiber.Core (Outcome(..), RIO)
import RIO.Fiber.Core as F

spec :: Spec Unit
spec = describe "rio-fiber: Cache" do

  it "get on first call runs the lookup and stores the result" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () Int
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> F.liftEffect
              (Ref.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        Cache.get cache "k"
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 1
      _ -> fail "expected Success"

  it "get on subsequent calls returns the cached value (no extra lookup)" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int, runs :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> F.liftEffect
              (Ref.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        first <- Cache.get cache "k"
        second <- Cache.get cache "k"
        runs <- F.liftEffect (Ref.read counter)
        pure { first, second, runs }
    out <- runAff program {}
    case out of
      Success r -> do
        r.first `shouldEqual` 1
        r.second `shouldEqual` 1
        r.runs `shouldEqual` 1
      _ -> fail "expected Success"

  it "distinct keys are looked up independently" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () { a :: Int, b :: Int, runs :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> F.liftEffect
              (Ref.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        a <- Cache.get cache "a"
        b <- Cache.get cache "b"
        runs <- F.liftEffect (Ref.read counter)
        pure { a, b, runs }
    out <- runAff program {}
    case out of
      Success r -> do
        r.a `shouldEqual` 1
        r.b `shouldEqual` 2
        r.runs `shouldEqual` 2
      _ -> fail "expected Success"

  it "concurrent misses on the same key share one lookup (single-flight)" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () { results :: Array Int, runs :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> do
              F.sleep (Milliseconds 20.0)
              F.liftEffect (Ref.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        results <- F.parTraverse (\_ -> Cache.get cache "k") [ unit, unit ]
        runs <- F.liftEffect (Ref.read counter)
        pure { results, runs }
    out <- runAff program {}
    case out of
      Success r -> do
        r.results `shouldEqual` [ 1, 1 ]
        r.runs `shouldEqual` 1
      _ -> fail "expected Success"

  it "invalidate evicts a key so the next get re-runs the lookup" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> F.liftEffect
              (Ref.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        first <- Cache.get cache "k"
        Cache.invalidate cache "k"
        second <- Cache.get cache "k"
        pure { first, second }
    out <- runAff program {}
    case out of
      Success r -> do
        r.first `shouldEqual` 1
        r.second `shouldEqual` 2
      _ -> fail "expected Success"

  it "invalidateAll clears every entry" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () Int
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> F.liftEffect
              (Ref.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        _ <- Cache.get cache "a"
        _ <- Cache.get cache "b"
        Cache.invalidateAll cache
        F.liftEffect (Cache.size cache)
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 0
      _ -> fail "expected Success"

  it "size tracks the number of stored entries" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () { afterTwo :: Int, afterOne :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> F.liftEffect
              (Ref.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        _ <- Cache.get cache "a"
        _ <- Cache.get cache "b"
        afterTwo <- F.liftEffect (Cache.size cache)
        Cache.invalidate cache "a"
        afterOne <- F.liftEffect (Cache.size cache)
        pure { afterTwo, afterOne }
    out <- runAff program {}
    case out of
      Success r -> do
        r.afterTwo `shouldEqual` 2
        r.afterOne `shouldEqual` 1
      _ -> fail "expected Success"

  it "TTL: an entry is re-looked up after its age exceeds the TTL" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> F.liftEffect
              (Ref.modify (_ + 1) counter)
          , timeToLive: Just (Milliseconds 10.0)
          }
        first <- Cache.get cache "k"
        F.sleep (Milliseconds 30.0)
        second <- Cache.get cache "k"
        pure { first, second }
    out <- runAff program {}
    case out of
      Success r -> do
        r.first `shouldEqual` 1
        r.second `shouldEqual` 2
      _ -> fail "expected Success"

  it "TTL: a fresh entry within the TTL is served from cache" do
    counter <- liftEffect (Ref.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int, runs :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> F.liftEffect
              (Ref.modify (_ + 1) counter)
          , timeToLive: Just (Milliseconds 1000.0)
          }
        first <- Cache.get cache "k"
        second <- Cache.get cache "k"
        runs <- F.liftEffect (Ref.read counter)
        pure { first, second, runs }
    out <- runAff program {}
    case out of
      Success r -> do
        r.first `shouldEqual` 1
        r.second `shouldEqual` 1
        r.runs `shouldEqual` 1
      _ -> fail "expected Success"

  it "evicts on defect: next get retries the lookup" do
    state <- liftEffect (Ref.new 0)
    let
      program :: RIO () () Int
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> do
              n <- F.liftEffect (Ref.modify (_ + 1) state)
              -- first call fails (defect), second succeeds
              if n == 1 then F.die (error "first call fails")
              else pure n
          , timeToLive: Nothing
          }
        -- swallow the first failure via causeOf, then retry
        _ <- F.causeOf (Cache.get cache "k")
        Cache.get cache "k"
    out <- runAff program {}
    case out of
      Success n -> n `shouldEqual` 2
      _ -> fail "expected Success on retry"
