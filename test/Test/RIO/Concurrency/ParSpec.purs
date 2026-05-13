module Test.RIO.Concurrency.ParSpec (spec) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now) as Now
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, runRIO)
import RIO.Concurrency.Par as Par

spec :: Spec Unit
spec = do
  describe "RIO.Concurrency.Par" do
    describe "Par.ado" do
      it "runs branches concurrently (3x 100ms in ~one slot)" do
        let
          slow ms label = do
            liftAff (delay (Milliseconds ms))
            pure label

          program :: RIO () () { a :: String, b :: String, c :: String }
          program = Par.ado
            a <- slow 100.0 "A"
            b <- slow 100.0 "B"
            c <- slow 100.0 "C"
            in { a, b, c }

        start <- liftEffect Now.now
        result <- runRIO program
        end <- liftEffect Now.now
        let elapsed = unwrap (unInstant end) - unwrap (unInstant start)
        result `shouldEqual` Right { a: "A", b: "B", c: "C" }
        -- generous upper bound: parallel should finish well under the
        -- 300ms a sequential version would take
        when (elapsed >= 250.0) do
          Spec.fail
            ( "expected parallel block to finish in well under 250ms, took "
                <> show elapsed
                <> "ms"
            )

      it "leftmost typed failure wins; later branches still run to completion" do
        let
          program :: RIO () (boom :: String) Int
          program = Par.ado
            a <- fail (Proxy :: Proxy "boom") "first"
            b <- (liftAff (delay (Milliseconds 10.0)) *> pure 2)
            in a + b

        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected typed failure"
