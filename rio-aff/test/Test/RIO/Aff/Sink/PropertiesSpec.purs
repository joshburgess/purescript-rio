module Test.RIO.Aff.Sink.PropertiesSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (runRIO')
import RIO.Aff.Sink as Sink
import RIO.Aff.Stream as Stream

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 50 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Aff.Sink (property tests)" do
  -- Mirror of `RIO.Aff.Stream (property tests)`: pin each Sink
  -- primitive against its pure `Array`-shaped specification across
  -- 50 random samples. Catches regressions on inputs the unit pins
  -- don't enumerate (empty, length-1, predicate-always-true /
  -- always-false, large arrays).

  it "collect matches the source array" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO' (Sink.runSink Sink.collect (Stream.fromArray xs))
      r `shouldEqual` xs

  it "count matches Array.length" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO' (Sink.runSink Sink.count (Stream.fromArray xs))
      r `shouldEqual` Array.length xs

  it "head matches Array.head" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO' (Sink.runSink Sink.head (Stream.fromArray xs))
      r `shouldEqual` Array.head xs

  it "last matches Array.last" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO' (Sink.runSink Sink.last (Stream.fromArray xs))
      r `shouldEqual` Array.last xs

  it "foldL matches Foldable.foldl" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO'
        (Sink.runSink (Sink.foldL 0 (+)) (Stream.fromArray xs))
      r `shouldEqual` foldl (+) 0 xs

  it "take matches Array.take" do
    forAll ((/\) <$> arbitrary <*> arbitrary :: Gen (Int /\ Array Int))
      \(n /\ xs) -> do
        r <- runRIO'
          (Sink.runSink (Sink.take n) (Stream.fromArray xs))
        r `shouldEqual` Array.take n xs

  it "find matches Array.find" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let p n = n > 0
      r <- runRIO'
        (Sink.runSink (Sink.find p) (Stream.fromArray xs))
      r `shouldEqual` Array.find p xs

  it "any matches Array.any" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let p n = n > 0
      r <- runRIO'
        (Sink.runSink (Sink.any p) (Stream.fromArray xs))
      r `shouldEqual` Array.any p xs

  it "all matches Array.all" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let p n = n > 0
      r <- runRIO'
        (Sink.runSink (Sink.all p) (Stream.fromArray xs))
      r `shouldEqual` Array.all p xs

  it "mapResult f over count corresponds to f ∘ length" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO'
        ( Sink.runSink
            (Sink.mapResult (_ + 1) Sink.count)
            (Stream.fromArray xs)
        )
      r `shouldEqual` (Array.length xs + 1)

  it "filterIn p collect corresponds to Array.filter p" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let p n = n `mod` 2 == 0
      r <- runRIO'
        ( Sink.runSink
            (Sink.filterIn p Sink.collect)
            (Stream.fromArray xs)
        )
      r `shouldEqual` Array.filter p xs

  it "zipPar count collect tuples (length, full array)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO'
        ( Sink.runSink
            (Sink.zipPar Sink.count Sink.collect)
            (Stream.fromArray xs)
        )
      r `shouldEqual` Tuple (Array.length xs) xs

  -- Algebraic laws for Sink combinators.

  it "mapResult identity ≡ id" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      left <- runRIO'
        ( Sink.runSink
            (Sink.mapResult identity Sink.count)
            (Stream.fromArray xs)
        )
      right <- runRIO'
        (Sink.runSink Sink.count (Stream.fromArray xs))
      left `shouldEqual` right

  it "mapResult composition: mapResult f ∘ mapResult g ≡ mapResult (f ∘ g)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        f n = n + 1
        g n = n * 2
      left <- runRIO'
        ( Sink.runSink
            (Sink.mapResult f (Sink.mapResult g Sink.count))
            (Stream.fromArray xs)
        )
      right <- runRIO'
        ( Sink.runSink
            (Sink.mapResult (f <<< g) Sink.count)
            (Stream.fromArray xs)
        )
      left `shouldEqual` right

  it "mapInput identity ≡ id" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      left <- runRIO'
        ( Sink.runSink
            (Sink.mapInput identity Sink.collect)
            (Stream.fromArray xs)
        )
      right <- runRIO'
        (Sink.runSink Sink.collect (Stream.fromArray xs))
      left `shouldEqual` right

  it "mapInput is contravariant: mapInput f ∘ mapInput g ≡ mapInput (g ∘ f)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      -- All on `Int -> Int` so the type stays Sink () () Int (Array Int)
      -- through every step.
      let
        f :: Int -> Int
        f n = n + 1

        g :: Int -> Int
        g n = n * 2
      left <- runRIO'
        ( Sink.runSink
            (Sink.mapInput f (Sink.mapInput g Sink.collect))
            (Stream.fromArray xs)
        )
      right <- runRIO'
        ( Sink.runSink
            (Sink.mapInput (g <<< f) Sink.collect)
            (Stream.fromArray xs)
        )
      left `shouldEqual` right

  it "zipParWith f sa sb ≡ mapResult (\\(Tuple a b) -> f a b) (zipPar sa sb)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let f a b = a + b
      left <- runRIO'
        ( Sink.runSink
            (Sink.zipParWith f Sink.count (Sink.foldL 0 (+)))
            (Stream.fromArray xs)
        )
      right <- runRIO'
        ( Sink.runSink
            ( Sink.mapResult (\(Tuple a b) -> f a b)
                (Sink.zipPar Sink.count (Sink.foldL 0 (+)))
            )
            (Stream.fromArray xs)
        )
      left `shouldEqual` right

  -- Sequencing law: `andThen head (\_ -> count)` consumes the first
  -- element with `head` then counts the rest. The result equals the
  -- count of remaining elements, i.e. `max 0 (length xs - 1)` (zero
  -- for empty and singleton inputs because `head`'s `finish` returns
  -- `Nothing` on empty input and `count` then runs on the empty
  -- tail). Pin the docstring promise that `andThen` resumes from
  -- the same stream position the first sink halted at.
  it "andThen head (\\_ -> count) ≡ max 0 (length xs - 1)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      r <- runRIO'
        ( Sink.runSink
            (Sink.andThen Sink.head (\_ -> Sink.count))
            (Stream.fromArray xs)
        )
      r `shouldEqual` max 0 (Array.length xs - 1)

  it "andThen (take n) (\\_ -> collect) ≡ Array.drop n xs" do
    -- Generate non-negative `n` so the property reads cleanly. The
    -- unit pins cover `take 0` and `take` overshooting; here we
    -- pin the algebraic equivalence across the regular regime.
    let
      smallNat :: Gen Int
      smallNat = (\k -> (if k < 0 then -k else k) `mod` 12) <$> arbitrary
    forAll ((/\) <$> smallNat <*> arbitrary :: Gen (Int /\ Array Int))
      \(n /\ xs) -> do
        r <- runRIO'
          ( Sink.runSink
              (Sink.andThen (Sink.take n) (\_ -> Sink.collect))
              (Stream.fromArray xs)
          )
        r `shouldEqual` Array.drop n xs

  -- Sink primitives factored through `collect`. These pin that
  -- the early-halting / aggregate sinks observe the same result
  -- as deriving the answer from a fully-materialized collect,
  -- modulo their finish behaviour. They are NOT statements about
  -- consumption (early-halt sinks pull fewer elements), only
  -- about the value the runner returns.

  it "count ≡ mapResult Array.length collect" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      left <- runRIO' (Sink.runSink Sink.count (Stream.fromArray xs))
      right <- runRIO'
        ( Sink.runSink
            (Sink.mapResult Array.length Sink.collect)
            (Stream.fromArray xs)
        )
      left `shouldEqual` right

  it "head ≡ mapResult Array.head (take 1)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      left <- runRIO' (Sink.runSink Sink.head (Stream.fromArray xs))
      right <- runRIO'
        ( Sink.runSink
            (Sink.mapResult Array.head (Sink.take 1))
            (Stream.fromArray xs)
        )
      left `shouldEqual` right

  it "last ≡ mapResult Array.last collect" do
    -- Dual of `head ≡ mapResult Array.head (take 1)`. The two
    -- paths differ in consumption (`last` walks the whole stream,
    -- `collect` materializes the whole array), but their values
    -- agree on every input. Pin the value-level duality so a
    -- regression in either primitive surfaces against the other.
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      left <- runRIO' (Sink.runSink Sink.last (Stream.fromArray xs))
      right <- runRIO'
        ( Sink.runSink
            (Sink.mapResult Array.last Sink.collect)
            (Stream.fromArray xs)
        )
      left `shouldEqual` right

  it "any p ≡ mapResult (case _ of Just _ -> true; Nothing -> false) (find p)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        p n = n > 0
        toBool = case _ of
          Just _ -> true
          Nothing -> false
      left <- runRIO'
        (Sink.runSink (Sink.any p) (Stream.fromArray xs))
      right <- runRIO'
        ( Sink.runSink
            (Sink.mapResult toBool (Sink.find p))
            (Stream.fromArray xs)
        )
      left `shouldEqual` right

  it "count ≡ foldL 0 (\\acc _ -> acc + 1)" do
    -- Pin the counting equivalence between the primitive `count`
    -- sink and a `foldL` that increments per element regardless
    -- of value. Catches a regression that, e.g., implemented
    -- `count` against a length-of-collected-array path that
    -- accidentally collapsed duplicates.
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      left <- runRIO' (Sink.runSink Sink.count (Stream.fromArray xs))
      right <- runRIO'
        ( Sink.runSink
            (Sink.foldL 0 (\acc _ -> acc + 1))
            (Stream.fromArray xs)
        )
      left `shouldEqual` right

  -- Cross-module duality: `Stream.runFold` and the
  -- `Sink.foldL`-driven runner must agree on the same fold. They
  -- are two paths to the same observable result; pinning the
  -- equivalence locks the duality so a refactor of either side
  -- cannot drift the other.
  it "Stream.runFold ≡ Sink.runSink (foldL ...)" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      left <- runRIO' (Stream.runFold 0 (+) (Stream.fromArray xs))
      right <- runRIO'
        ( Sink.runSink
            (Sink.foldL 0 (+))
            (Stream.fromArray xs)
        )
      left `shouldEqual` right
