module Test.RIO.STM.TSemaphore.PropertiesSpec (spec) where

import Prelude

import Data.Foldable (for_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen, randomSample')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.STM (atomically)
import RIO.STM.TSemaphore
  ( acquireN
  , availableTSemaphore
  , newTSemaphore
  , releaseN
  )

forAll :: forall a. Gen a -> (a -> Aff Unit) -> Aff Unit
forAll gen prop = do
  samples <- liftEffect (randomSample' 30 gen)
  for_ samples prop

smallNat :: Gen Int
smallNat = (\k -> (if k < 0 then -k else k) `mod` 21) <$> arbitrary

spec :: Spec Unit
spec = describe "RIO.STM.TSemaphore (property tests)" do
  -- The unit pin uses `newTSemaphore 5` with `acquireN 3` /
  -- `releaseN 3`. Generalize across small naturals so a
  -- regression at zero, small-`n`, or larger `n` is caught.

  it "newTSemaphore n exposes n available permits" do
    forAll smallNat \n -> do
      let
        program :: RIO () () Int
        program = do
          sem <- atomically (newTSemaphore n)
          atomically (availableTSemaphore sem)
      r <- runRIO' program
      r `shouldEqual` n

  it "acquireN k decreases available by k (when k <= n)" do
    -- Bound `k <= n` so `acquireN` does not block. The
    -- always-available regime is enough to pin the arithmetic;
    -- the blocking case is unit-tested separately.
    let
      gen :: Gen { n :: Int, k :: Int }
      gen = do
        n <- smallNat
        kBase <- smallNat
        let k = if n == 0 then 0 else kBase `mod` (n + 1)
        pure { n, k }
    forAll gen \{ n, k } -> do
      let
        program :: RIO () () Int
        program = do
          sem <- atomically (newTSemaphore n)
          atomically (acquireN k sem)
          atomically (availableTSemaphore sem)
      r <- runRIO' program
      r `shouldEqual` (n - k)

  it "acquireN k then releaseN k restores the count to n" do
    let
      gen :: Gen { n :: Int, k :: Int }
      gen = do
        n <- smallNat
        kBase <- smallNat
        let k = if n == 0 then 0 else kBase `mod` (n + 1)
        pure { n, k }
    forAll gen \{ n, k } -> do
      let
        program :: RIO () () Int
        program = do
          sem <- atomically (newTSemaphore n)
          atomically (acquireN k sem)
          atomically (releaseN k sem)
          atomically (availableTSemaphore sem)
      r <- runRIO' program
      r `shouldEqual` n
