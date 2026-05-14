module Test.RIO.Stream.PropertiesSpec (spec) where

import Prelude

import Data.Array (concatMap, drop, filter, take) as Array
import Data.Foldable (for_, sum)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (runRIO')
import RIO.Stream
  ( concat
  , drop
  , filter
  , flatMap
  , fromArray
  , map
  , runCollect
  , runFold
  , runFoldM
  , single
  , take
  )

-- Run `prop` against 50 randomly generated samples from `gen`. A
-- failing assertion inside `prop` throws via `shouldEqual`, which
-- already includes the expected/got diff in its message; no extra
-- per-sample annotation is needed.
forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 50 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Stream (property tests)" do
  -- These properties pin the Stream pipeline against its pure
  -- Array-shaped specification. Every unit pin in `StreamSpec`
  -- fixes specific inputs; here we sample 50 random inputs per
  -- law to catch regressions that only break on inputs we
  -- forgot to enumerate (e.g. empty inputs, length-1 inputs,
  -- inputs where the predicate is constantly false / true,
  -- large arrays). The intent is structural redundancy with
  -- the unit pins, not replacement.

  it "runCollect ∘ fromArray ≡ identity" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO' (runCollect (fromArray xs))
      r `shouldEqual` xs

  it "map corresponds to Functor (<$>)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let f n = n * 2 + 1
      r <- runRIO' (runCollect (map f (fromArray xs)))
      r `shouldEqual` (f <$> xs)

  it "filter corresponds to Array.filter" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let p n = n `mod` 2 == 0
      r <- runRIO' (runCollect (filter p (fromArray xs)))
      r `shouldEqual` Array.filter p xs

  it "take corresponds to Array.take" do
    forAll ((/\) <$> arbitrary <*> arbitrary :: Gen (Int /\ Array Int))
      \(n /\ xs) -> do
        r <- runRIO' (runCollect (take n (fromArray xs)))
        r `shouldEqual` Array.take n xs

  it "drop corresponds to Array.drop" do
    forAll ((/\) <$> arbitrary <*> arbitrary :: Gen (Int /\ Array Int))
      \(n /\ xs) -> do
        r <- runRIO' (runCollect (drop n (fromArray xs)))
        r `shouldEqual` Array.drop n xs

  it "concat corresponds to <>" do
    forAll ((/\) <$> arbitrary <*> arbitrary :: Gen (Array Int /\ Array Int))
      \(xs /\ ys) -> do
        r <- runRIO' (runCollect (concat (fromArray xs) (fromArray ys)))
        r `shouldEqual` (xs <> ys)

  it "concat is associative under runCollect" do
    let
      gen :: Gen (Array Int /\ Array Int /\ Array Int)
      gen = (\a b c -> a /\ b /\ c) <$> arbitrary <*> arbitrary <*> arbitrary
    forAll gen \(xs /\ ys /\ zs) -> do
      let
        left =
          concat (concat (fromArray xs) (fromArray ys)) (fromArray zs)
        right =
          concat (fromArray xs) (concat (fromArray ys) (fromArray zs))
      rl <- runRIO' (runCollect left)
      rr <- runRIO' (runCollect right)
      rl `shouldEqual` rr

  it "flatMap corresponds to Array.concatMap" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let f n = [ n, n * 10 ]
      r <- runRIO' (runCollect (flatMap (fromArray xs) (\n -> fromArray (f n))))
      r `shouldEqual` Array.concatMap f xs

  it "runFold (+) 0 ≡ sum" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO' (runFold 0 (+) (fromArray xs))
      r `shouldEqual` sum xs

  it "runFoldM with a pure step matches runFold" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r1 <- runRIO'
        (runFoldM 0 (\acc n -> pure (acc + n)) (fromArray xs))
      r2 <- runRIO' (runFold 0 (+) (fromArray xs))
      r1 `shouldEqual` r2

  -- Monad laws for the Stream functor. `flatMap` is bind and
  -- `single` is return. The properties hold under `runCollect`'s
  -- pure Array projection.

  it "flatMap left identity: flatMap (single x) f ≡ f x" do
    forAll (arbitrary :: Gen Int) \x -> do
      let f n = fromArray [ n, n * 2, n + 1 ]
      left <- runRIO' (runCollect (flatMap (single x) f))
      right <- runRIO' (runCollect (f x))
      left `shouldEqual` right

  it "flatMap right identity: flatMap s single ≡ s" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let s = fromArray xs
      left <- runRIO' (runCollect (flatMap s single))
      right <- runRIO' (runCollect s)
      left `shouldEqual` right

  it "flatMap associativity: flatMap (flatMap s f) g ≡ flatMap s (\\x -> flatMap (f x) g)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        f n = fromArray [ n, n + 1 ]
        g n = fromArray [ n * 10, n * 10 + 1 ]
        s = fromArray xs
      left <- runRIO' (runCollect (flatMap (flatMap s f) g))
      right <- runRIO' (runCollect (flatMap s (\x -> flatMap (f x) g)))
      left `shouldEqual` right
