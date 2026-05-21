-- | RIO-tuned property-testing harness.
-- |
-- | A thin lift of `Test.QuickCheck.Gen`'s `randomSample'` into `RIO`
-- | so property checks compose with the rest of the test suite via
-- | the same `RIO` machinery the production code uses.
-- |
-- | The harness intentionally does NOT do shrinking. PureScript's
-- | QuickCheck port (the one bundled with the registry) does not
-- | carry the integrated shrinker that Haskell QuickCheck exposes,
-- | and adding a custom shrinker layer is well out of scope for this
-- | wrapper. If a property fails, you get the first counter-example
-- | the generator produced, along with whatever assertion (typically
-- | `shouldEqual`) blew up.
-- |
-- | ```purescript
-- | import RIO.Fiber.Test.Property (forAllRIO)
-- |
-- | itRIO "x + 0 == x" do
-- |   forAllRIO (arbitrary :: Gen Int) \x ->
-- |     liftEffect ((x + 0) `shouldEqual` x)
-- | ```
module RIO.Fiber.Test.Property
  ( defaultSampleCount
  , forAllRIO
  , forAllRION
  , generateSamples
  ) where

import Prelude

import Data.Foldable (for_)
import Effect.Class (class MonadEffect, liftEffect)
import Test.QuickCheck.Gen (Gen, randomSample')

-- | The number of samples `forAllRIO` runs per call.
defaultSampleCount :: Int
defaultSampleCount = 30

-- | Generate `n` samples from `gen` inside any `MonadEffect`-capable
-- | monad (which includes `RIO` and `Aff`).
-- |
-- | A direct lift of `Test.QuickCheck.Gen.randomSample'` so callers
-- | can build their own per-spec property loops without re-importing
-- | the QuickCheck modules at every spec site.
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
-- | and for `RIO` programs run through `runRIO` / `itRIO`.
-- |
-- | The property runs once per sample, in order. If a sample makes
-- | the property throw (typically via `shouldEqual`), the test fails
-- | on that sample and subsequent samples are not exercised.
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
