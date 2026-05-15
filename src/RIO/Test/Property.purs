-- | RIO-tuned property-testing harness.
-- |
-- | A thin lift of `Test.QuickCheck.Gen`'s `randomSample'` into `RIO`
-- | so property checks compose with the rest of the test suite via
-- | the same `RIO` machinery the production code uses. Each property
-- | spec in the repository previously redefined a local `forAll :: Gen
-- | a -> (a -> Aff Unit) -> Aff Unit` over and over; this module
-- | standardises that pattern and adapts it to actions that read
-- | services and can raise typed failures.
-- |
-- | The harness intentionally does NOT do shrinking. PureScript's
-- | QuickCheck port (the one bundled with the registry) does not
-- | carry the integrated shrinker that Haskell QuickCheck exposes,
-- | and adding a custom shrinker layer is well out of scope for this
-- | wrapper. If a property fails, you get the first counter-example
-- | the generator produced, along with whatever assertion (typically
-- | `shouldEqual`) blew up. That is the same trade-off the existing
-- | hand-rolled `forAll` makes.
-- |
-- | ```purescript
-- | -- existing pattern (works fine, but every property spec
-- | -- re-defines the same helper):
-- | forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
-- | forAll gen prop = do
-- |   samples <- liftEffect (randomSample' 30 gen)
-- |   for_ samples prop
-- |
-- | -- replacement:
-- | import RIO.Test.Property (forAllRIO)
-- |
-- | it "..." do
-- |   forAllRIO smallNat \n -> do
-- |     ...
-- | ```
module RIO.Test.Property
  ( defaultSampleCount
  , forAllRIO
  , forAllRION
  , generateSamples
  ) where

import Prelude

import Data.Foldable (for_)
import Effect.Class (class MonadEffect, liftEffect)
import Test.QuickCheck.Gen (Gen, randomSample')

-- | The number of samples `forAllRIO` runs per call. Mirrors the
-- | constant the in-repo hand-rolled `forAll` helpers used (30 each),
-- | so swapping in `forAllRIO` is a like-for-like change.
defaultSampleCount :: Int
defaultSampleCount = 30

-- | Generate `n` samples from `gen` inside any `MonadEffect`-capable
-- | monad (which includes `RIO` and `Aff`).
-- |
-- | A direct lift of `Test.QuickCheck.Gen.randomSample'` so callers
-- | can build their own per-spec property loops without re-importing
-- | the QuickCheck modules at every spec site.
-- |
-- | ```purescript
-- | samples <- generateSamples 50 smallNat
-- | for_ samples \n -> do
-- |   ...
-- | ```
generateSamples
  :: forall m a
   . MonadEffect m
  => Int
  -> Gen a
  -> m (Array a)
generateSamples n gen = liftEffect (randomSample' n gen)

-- | Run a property over `defaultSampleCount` samples drawn from
-- | `gen`. The property body executes in any `MonadEffect`-capable
-- | monad, so it works for `Aff`-shaped specs (`it "..." do ...`)
-- | and for RIO programs run through `runRIO` / `runRIO'`.
-- |
-- | The property runs once per sample, in order. If a sample makes
-- | the property throw (typically via `shouldEqual`), the test fails
-- | on that sample and subsequent samples are not exercised.
-- |
-- | ```purescript
-- | it "x + 0 == x" do
-- |   forAllRIO (arbitrary :: Gen Int) \x ->
-- |     (x + 0) `shouldEqual` x
-- | ```
forAllRIO
  :: forall m a
   . MonadEffect m
  => Gen a
  -> (a -> m Unit)
  -> m Unit
forAllRIO = forAllRION defaultSampleCount

-- | Variant of `forAllRIO` that takes the sample count explicitly.
-- | Use when a property is cheap enough to exercise at a larger
-- | budget, or expensive enough (e.g. spawns fibers, does I/O) to
-- | warrant a smaller one.
-- |
-- | ```purescript
-- | it "sort idempotent (heavy)" do
-- |   forAllRION 5 (arbitrary :: Gen (Array Int)) \xs ->
-- |     sort (sort xs) `shouldEqual` sort xs
-- | ```
forAllRION
  :: forall m a
   . MonadEffect m
  => Int
  -> Gen a
  -> (a -> m Unit)
  -> m Unit
forAllRION n gen prop = do
  samples <- generateSamples n gen
  for_ samples prop
