module Test.RIO.Concurrency.PropertiesSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Concurrency (parSequence, parTraverse)
import RIO.Core (RIO, runRIO)

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 20 gen)
  for_ samples prop

spec :: Spec Unit
spec = describe "RIO.Concurrency (property tests)" do
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
