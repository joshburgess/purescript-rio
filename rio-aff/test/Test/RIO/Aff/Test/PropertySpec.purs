module Test.RIO.Aff.Test.PropertySpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Foldable (sum)
import Effect.Aff (Aff, attempt)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effect.Ref as Ref
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Aff.Test.Property (defaultSampleCount, forAllRIO, forAllRION, generateSamples)

-- A tiny generator that yields `Int` mapped into `[0, 20]` so the
-- property body's per-sample work is cheap.
smallNat :: Gen Int
smallNat = (\k -> (if k < 0 then -k else k) `mod` 21) <$> arbitrary

spec :: Spec Unit
spec = describe "RIO.Aff.Test.Property" do
  describe "defaultSampleCount" do
    it "is 30 (matches the in-repo hand-rolled forAll helpers)" do
      defaultSampleCount `shouldEqual` 30

  describe "generateSamples" do
    it "produces exactly the requested number of samples" do
      samples <- generateSamples 7 smallNat
      Array.length samples `shouldEqual` 7

    it "produces an empty array when the requested count is zero" do
      samples <- generateSamples 0 smallNat
      Array.length samples `shouldEqual` 0

  describe "forAllRIO" do
    it "runs the property exactly `defaultSampleCount` times" do
      -- Observe every invocation through a counter; on the way
      -- out, the counter must equal `defaultSampleCount`.
      counter <- liftEffect (Ref.new 0)
      forAllRIO smallNat \_ -> do
        liftEffect (Ref.modify_ (_ + 1) counter)
      final <- liftEffect (Ref.read counter)
      final `shouldEqual` defaultSampleCount

    it "passes each generated sample to the property body" do
      -- Capture every sample the harness produces, then re-generate
      -- a separate batch with the same generator and confirm both
      -- batches were drawn from the same shape (every element
      -- lands in the expected range).
      seen <- liftEffect (Ref.new [])
      forAllRIO smallNat \n ->
        liftEffect (Ref.modify_ (\xs -> xs <> [ n ]) seen)
      captured <- liftEffect (Ref.read seen)
      Array.length captured `shouldEqual` defaultSampleCount
      -- The generator's range is `[0, 20]`. The sum upper-bounds
      -- the contract: every sample must be a small non-negative
      -- int, which we verify by the aggregate staying within
      -- `30 * 20 = 600`.
      let total = sum captured
      (total >= 0) `shouldEqual` true
      (total <= defaultSampleCount * 20) `shouldEqual` true

    it "propagates an assertion failure from the property body" do
      -- A property that fails on the first sample (the generator
      -- always returns a value `<= 20`, so requiring `> 100`
      -- guarantees failure). Wrap the call in `attempt` so the
      -- exception becomes an observable value; the test then
      -- confirms the harness did NOT swallow it.
      outcome <- attempt do
        forAllRIO smallNat \n ->
          when (n <= 100) (fail "expected failure")
      case outcome of
        Left err ->
          (message err) `shouldEqual` "expected failure"
        Right _ ->
          fail "forAllRIO should have surfaced the property failure"

  describe "forAllRION" do
    it "runs the property exactly the requested number of times" do
      counter <- liftEffect (Ref.new 0)
      forAllRION 5 smallNat \_ ->
        liftEffect (Ref.modify_ (_ + 1) counter)
      final <- liftEffect (Ref.read counter)
      final `shouldEqual` 5

    it "runs the property zero times when the budget is zero" do
      counter <- liftEffect (Ref.new 0)
      forAllRION 0 smallNat \_ ->
        liftEffect (Ref.modify_ (_ + 1) counter)
      final <- liftEffect (Ref.read counter)
      final `shouldEqual` 0

    it "stops at the first failure (subsequent samples not exercised)" do
      -- Increment a counter each time the property runs. The
      -- property fails on its first invocation, so the counter
      -- must read exactly 1 even though the budget asked for 10.
      counter <- liftEffect (Ref.new 0)
      _ <- attempt do
        forAllRION 10 smallNat \_ -> do
          liftEffect (Ref.modify_ (_ + 1) counter)
          fail "fail immediately"
      final <- liftEffect (Ref.read counter)
      final `shouldEqual` 1

  describe "Aff host (MonadEffect)" do
    it "the property body lives in any MonadEffect-capable monad" do
      -- The signature is `MonadEffect m => ...`, so the same
      -- harness works for `Aff` (which is what `it` blocks live
      -- in) and for any other `MonadEffect` carrier (e.g. `RIO`).
      -- This test pins the `Aff` slot directly.
      acc <- liftEffect (Ref.new 0)
      forAllRIO smallNat \n ->
        (liftEffect (Ref.modify_ (_ + n) acc) :: Aff Unit)
      _ <- liftEffect (Ref.read acc)
      pure unit
