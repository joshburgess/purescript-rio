-- | A tiny Aff-aware benchmark harness.
-- |
-- | `minibench` works on synchronous `Unit -> a`. RIO's computations are
-- | wrapped in `Aff`, so this module provides a parallel set of helpers
-- | that time an `Aff` action by sampling `process.hrtime()` before and
-- | after each invocation. The numbers reported are mean nanoseconds
-- | per iteration plus min / max / stddev.
-- |
-- | The harness does not force GC; pin a node version with `--expose-gc`
-- | and call `Performance.Minibench.bench` for finer-grained
-- | synchronous numbers.
module Benchmarks.Harness
  ( benchAff
  , benchAffWith
  , benchAffResult
  , AffBenchResult
  , withUnits
  ) where

import Prelude

import Benchmarks.Hrtime (hrtimeNs)
import Data.Array (replicate)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Number (infinity, max, min, sqrt)
import Data.Number.Format (toStringWith, fixed)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref

type AffBenchResult =
  { mean :: Number
  , stdDev :: Number
  , min :: Number
  , max :: Number
  , iterations :: Int
  }

-- | Run an `Aff` action `n` times and print mean / stddev / min / max.
benchAffWith :: Int -> String -> Aff Unit -> Aff Unit
benchAffWith n label action = do
  res <- benchAffResult n action
  liftEffect do
    log ("--- " <> label <> " (" <> show n <> " iterations) ---")
    log ("mean   = " <> withUnits res.mean)
    log ("stddev = " <> withUnits res.stdDev)
    log ("min    = " <> withUnits res.min)
    log ("max    = " <> withUnits res.max)

-- | `benchAffWith` with a default of 1000 iterations.
benchAff :: String -> Aff Unit -> Aff Unit
benchAff = benchAffWith 1000

-- | Same shape as `benchAffWith`, but returns the raw stats rather than
-- | printing them. Useful when a caller wants to format its own table.
benchAffResult :: Int -> Aff Unit -> Aff AffBenchResult
benchAffResult n action = do
  sumRef <- liftEffect (Ref.new 0.0)
  sum2Ref <- liftEffect (Ref.new 0.0)
  minRef <- liftEffect (Ref.new infinity)
  maxRef <- liftEffect (Ref.new 0.0)
  for_ (replicate n unit) \_ -> do
    startNs <- liftEffect hrtimeNs
    action
    endNs <- liftEffect hrtimeNs
    let elapsedNs = endNs - startNs
    liftEffect do
      _ <- Ref.modify (_ + elapsedNs) sumRef
      _ <- Ref.modify (_ + elapsedNs * elapsedNs) sum2Ref
      _ <- Ref.modify (_ `min` elapsedNs) minRef
      _ <- Ref.modify (_ `max` elapsedNs) maxRef
      pure unit
  sumNs <- liftEffect (Ref.read sumRef)
  sum2Ns <- liftEffect (Ref.read sum2Ref)
  minNs <- liftEffect (Ref.read minRef)
  maxNs <- liftEffect (Ref.read maxRef)
  let
    n' = toNumber n
    mean = sumNs / n'
    variance = (sum2Ns - n' * mean * mean) / (n' - 1.0)
    stdDev = if variance < 0.0 then 0.0 else sqrt variance
  pure
    { mean
    , stdDev
    , min: minNs
    , max: maxNs
    , iterations: n
    }

withUnits :: Number -> String
withUnits t
  | t < 1.0e3 = toStringWith (fixed 2) t <> " ns"
  | t < 1.0e6 = toStringWith (fixed 2) (t / 1.0e3) <> " μs"
  | t < 1.0e9 = toStringWith (fixed 2) (t / 1.0e6) <> " ms"
  | otherwise = toStringWith (fixed 2) (t / 1.0e9) <> " s"
