module Test.RIO.Aff.Concurrency.PropertiesSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Concurrency (parSequence, parTraverse, zipPar)
import RIO.Aff.Core (RIO, runRIO)

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 20 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Aff.Concurrency (property tests)" do
  -- The unit pin `parTraverse preserves array order in the result`
  -- fixes the input to `[1, 2, 3, 4, 5]` with `f n = n * n`.
  -- Generalize the ordering contract across arbitrary `Array Int`
  -- inputs and a pure mapping. A regression that, e.g., bound the
  -- result to fiber-completion order instead of input order would
  -- pass the fixed-length pin (in practice every branch completes
  -- in the same tick for `pure`) but could break on certain
  -- input shapes here.

  it "parTraverse on a pure mapping matches Functor map" do
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        f n = n * 2 + 1

        prog :: RIO () () (Array Int)
        prog = parTraverse (\n -> pure (f n)) xs
      result <- runRIO prog
      result `shouldEqual` (Right (map f xs) :: Either _ (Array Int))

  it "zipPar associates up to tupling across arbitrary triples" do
    -- The unit pin uses the fixed triple `(1, 2, 3)`. Generalize
    -- so the (Tuple (Tuple a b) c) ↔ (Tuple a (Tuple b c)) shape
    -- equivalence is exercised across arbitrary `Int` triples. A
    -- regression that, e.g., applied the wrong projection inside
    -- one of the branches would still satisfy `(1, 2, 3)` if the
    -- regression happened to commute with those exact values.
    let
      gen :: Gen { a :: Int, b :: Int, c :: Int }
      gen = { a: _, b: _, c: _ } <$> arbitrary <*> arbitrary <*> arbitrary
    forAll gen \{ a, b, c } -> do
      let
        lhs :: RIO () () (Tuple (Tuple Int Int) Int)
        lhs = zipPar (zipPar (pure a) (pure b)) (pure c)

        rhs :: RIO () () (Tuple Int (Tuple Int Int))
        rhs = zipPar (pure a) (zipPar (pure b) (pure c))
      rL <- runRIO lhs
      rR <- runRIO rhs
      case rL, rR of
        Right (Tuple (Tuple x y) z), Right (Tuple x' (Tuple y' z')) ->
          (Tuple (Tuple x y) z) `shouldEqual` (Tuple (Tuple x' y') z')
        _, _ -> 1 `shouldEqual` 0

  it "parSequence ≡ parTraverse identity" do
    -- Docstring promise (and the unit pin): `parSequence` is
    -- `parTraverse identity`. Pin the equivalence across
    -- arbitrary `Array Int` so both implementations agree on
    -- every shape, not just the `[10, 20, 30]` pin.
    forAll (arbitrary :: Gen (Array Int)) \xs -> do
      let
        traversed :: RIO () () (Array Int)
        traversed = parTraverse pure xs

        sequenced :: RIO () () (Array Int)
        sequenced = parSequence (map pure xs)
      rt <- runRIO traversed
      rs <- runRIO sequenced
      rt `shouldEqual` rs
