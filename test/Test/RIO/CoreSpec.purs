module Test.RIO.CoreSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO, runRIO', unsafeRunRIO)

-- | Sample inputs used for the law checks. Three values is enough to catch
-- | a flat-out broken instance; richer property-based coverage is queued
-- | for Phase 7, once we have a way to drive `Aff`-valued properties.
sampleInts :: Array Int
sampleInts = [ -7, 0, 42 ]

forSamples :: (Int -> Aff Unit) -> Aff Unit
forSamples f = traverse_ f sampleInts

-- | Pretty-print both sides of a law check by running each runner and
-- | comparing with `shouldEqual` (which reports the failing values).
checkEq
  :: forall a
   . Eq a
  => Show a
  => Aff a
  -> Aff a
  -> Aff Unit
checkEq lhs rhs = do
  l <- lhs
  r <- rhs
  l `shouldEqual` r

spec :: Spec Unit
spec = do
  describe "RIO.Core" do
    describe "runners (Phase 1.1)" do
      it "runRIO (pure 42) returns Right 42" do
        result <- runRIO (pureInt 42)
        result `shouldEqual` Right 42

      it "runRIO' (pure 42) returns 42" do
        result <- runRIO' (pureInt 42)
        result `shouldEqual` 42

      it "unsafeRunRIO threads the supplied environment" do
        let env = { x: 7 }
        result <- unsafeRunRIO (pureIntEnv env.x) env
        result `shouldEqual` Right 7

    describe "Functor laws (Phase 1.2)" do
      it "identity: map id = id" $ forSamples \n ->
        checkEq
          (runRIO (map identity (pureInt n)))
          (runRIO (pureInt n))

      it "composition: map (f <<< g) = map f <<< map g" $ forSamples \n ->
        let
          f x = x + 1
          g x = x * 2
        in
          checkEq
            (runRIO (map (f <<< g) (pureInt n)))
            (runRIO (map f (map g (pureInt n))))

    describe "Applicative laws (Phase 1.2)" do
      it "identity: pure id <*> v = v" $ forSamples \n ->
        checkEq
          (runRIO (pure identity <*> pureInt n))
          (runRIO (pureInt n))

      it "homomorphism: pure f <*> pure x = pure (f x)" $ forSamples \n ->
        let
          f x = x + 100
        in
          checkEq
            (runRIO (pureFn f <*> pureInt n))
            (runRIO (pureInt (f n)))

      it "interchange: u <*> pure y = pure (\\f -> f y) <*> u" $ forSamples \n ->
        let
          u = pureFn (\x -> x * 3)
        in
          checkEq
            (runRIO (u <*> pureInt n))
            (runRIO (pureFn (\f -> f n) <*> u))

      it "composition: pure (<<<) <*> u <*> v <*> w = u <*> (v <*> w)" $ forSamples \n ->
        let
          u = pureFn (\x -> x + 1)
          v = pureFn (\x -> x * 2)
        in
          checkEq
            (runRIO (pureFn (<<<) <*> u <*> v <*> pureInt n))
            (runRIO (u <*> (v <*> pureInt n)))

    describe "Monad laws (Phase 1.2)" do
      it "left identity: pure a >>= f = f a" $ forSamples \n ->
        let
          f x = pureInt (x * 5)
        in
          checkEq
            (runRIO (pureInt n >>= f))
            (runRIO (f n))

      it "right identity: m >>= pure = m" $ forSamples \n ->
        checkEq
          (runRIO (pureInt n >>= pure))
          (runRIO (pureInt n))

      it "associativity: (m >>= f) >>= g = m >>= (\\x -> f x >>= g)" $ forSamples \n ->
        let
          f x = pureInt (x + 1)
          g x = pureInt (x * 2)
        in
          checkEq
            (runRIO ((pureInt n >>= f) >>= g))
            (runRIO (pureInt n >>= \x -> f x >>= g))

-- Helpers --------------------------------------------------------------------
-- All helpers fix the error row to `()` so test results have a `Show`
-- instance. Tests for non-empty error rows live with `fail`/`catchTag`
-- in later phases.

pureInt :: Int -> RIO () () Int
pureInt = pure

pureIntEnv :: Int -> RIO (x :: Int) () Int
pureIntEnv = pure

pureFn :: forall a b. (a -> b) -> RIO () () (a -> b)
pureFn = pure
