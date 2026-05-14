module Test.RIO.Sink.PropertiesSpec (spec) where

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

import RIO.Core (runRIO')
import RIO.Sink as Sink
import RIO.Stream as Stream

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 50 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Sink (property tests)" do
  -- Mirror of `RIO.Stream (property tests)`: pin each Sink
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
