module Test.RIO.CacheSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as ERef
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Cache as Cache
import RIO.Concurrency (parTraverse)
import RIO.Core (RIO, runRIO)

spec :: Spec Unit
spec = describe "RIO.Cache" do
  it "get on first call runs the lookup and stores the result" do
    -- A miss must run the lookup. Counter increments once.
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () Int
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> liftEffect
              (ERef.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        Cache.get cache "k"
    result <- runRIO program
    result `shouldEqual` (Right 1 :: Either _ Int)

  it "get on subsequent calls returns the cached value (no extra lookup)" do
    -- Two sequential `get`s for the same key must only run lookup
    -- once. A regression that bypassed the cache would increment
    -- the counter twice.
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int, runs :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> liftEffect
              (ERef.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        first <- Cache.get cache "k"
        second <- Cache.get cache "k"
        runs <- liftEffect (ERef.read counter)
        pure { first, second, runs }
    result <- runRIO program
    result `shouldEqual`
      (Right { first: 1, second: 1, runs: 1 } :: Either _ _)

  it "distinct keys are looked up independently" do
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { a :: Int, b :: Int, runs :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> liftEffect
              (ERef.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        a <- Cache.get cache "a"
        b <- Cache.get cache "b"
        runs <- liftEffect (ERef.read counter)
        pure { a, b, runs }
    result <- runRIO program
    -- Two distinct keys yield two lookups (counter goes 1 then 2).
    result `shouldEqual`
      (Right { a: 1, b: 2, runs: 2 } :: Either _ _)

  it "concurrent misses on the same key share one lookup (single-flight)" do
    -- Two fibers race on the same key. The lookup delays so both
    -- arrive before completion; only one increment must happen.
    -- A regression that removed the AVar-share would let both
    -- fibers run independent lookups.
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { results :: Array Int, runs :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> do
              delay (Milliseconds 20.0)
              liftEffect (ERef.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        results <- parTraverse (\_ -> Cache.get cache "k") [ unit, unit ]
        runs <- liftEffect (ERef.read counter)
        pure { results, runs }
    result <- runRIO program
    result `shouldEqual`
      (Right { results: [ 1, 1 ], runs: 1 } :: Either _ _)

  it "invalidate evicts a key so the next get re-runs the lookup" do
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> liftEffect
              (ERef.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        first <- Cache.get cache "k"
        Cache.invalidate cache "k"
        second <- Cache.get cache "k"
        pure { first, second }
    result <- runRIO program
    result `shouldEqual`
      (Right { first: 1, second: 2 } :: Either _ _)

  it "invalidateAll clears every entry" do
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () Int
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> liftEffect
              (ERef.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        _ <- Cache.get cache "a"
        _ <- Cache.get cache "b"
        Cache.invalidateAll cache
        liftEffect (Cache.size cache)
    result <- runRIO program
    result `shouldEqual` (Right 0 :: Either _ Int)

  it "size tracks the number of stored entries" do
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { afterTwo :: Int, afterOne :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> liftEffect
              (ERef.modify (_ + 1) counter)
          , timeToLive: Nothing
          }
        _ <- Cache.get cache "a"
        _ <- Cache.get cache "b"
        afterTwo <- liftEffect (Cache.size cache)
        Cache.invalidate cache "a"
        afterOne <- liftEffect (Cache.size cache)
        pure { afterTwo, afterOne }
    result <- runRIO program
    result `shouldEqual`
      (Right { afterTwo: 2, afterOne: 1 } :: Either _ _)

  it "TTL: an entry is re-looked up after its age exceeds the TTL" do
    -- A very short TTL means the second get (after a delay) must
    -- re-run the lookup; a regression that ignored TTL would only
    -- run it once.
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> liftEffect
              (ERef.modify (_ + 1) counter)
          , timeToLive: Just (Milliseconds 10.0)
          }
        first <- Cache.get cache "k"
        liftAff (delay (Milliseconds 30.0))
        second <- Cache.get cache "k"
        pure { first, second }
    result <- runRIO program
    result `shouldEqual`
      (Right { first: 1, second: 2 } :: Either _ _)

  it "TTL: a fresh entry within the TTL is served from cache" do
    -- Within the TTL window, the second get must hit the cache.
    counter <- liftEffect (ERef.new 0)
    let
      program :: RIO () () { first :: Int, second :: Int, runs :: Int }
      program = do
        cache <- Cache.make
          { lookup: \(_ :: String) -> liftEffect
              (ERef.modify (_ + 1) counter)
          , timeToLive: Just (Milliseconds 1000.0)
          }
        first <- Cache.get cache "k"
        second <- Cache.get cache "k"
        runs <- liftEffect (ERef.read counter)
        pure { first, second, runs }
    result <- runRIO program
    result `shouldEqual`
      (Right { first: 1, second: 1, runs: 1 } :: Either _ _)
