-- | Performance regression gate.
-- |
-- | Runs the same scenarios as `Benchmarks.Main`, compares each
-- | one's mean wall-clock per iteration against a hard-coded
-- | baseline (taken from `docs/performance.md`), and exits with a
-- | non-zero code when any scenario exceeds its tolerance.
-- |
-- | The baseline was captured on an Apple M1 Pro with node 20.
-- | The tolerance is set to a deliberately generous 3x of the
-- | baseline mean so the gate catches catastrophic regressions
-- | (an accidental O(n^2), a re-wrapping bug doubling per-bind
-- | overhead) without flapping on machine-to-machine variance.
-- |
-- | If you're running this on a slower box than the M1 Pro and
-- | every scenario shows up as a regression, that's expected.
-- | The gate is intended as a developer-runnable check on the
-- | reference hardware, not a per-environment guarantee. See
-- | `docs/performance.md` for how to update the baseline.
module Benchmarks.Gate
  ( main
  ) where

import Prelude

import Benchmarks.Exit (exit)
import Benchmarks.Harness (benchAffResult, withUnits)
import Data.Array (filter, length, range, replicate) as Array
import Data.Foldable (for_)
import Data.Number.Format (toStringWith, fixed)
import Data.String (length) as String
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import RIO.Core
  ( RIO
  , ask
  , catchTag
  , fail
  , provideAll
  , runRIO
  , runRIO'
  )
import RIO.Concurrency (parTraverse)
import Type.Proxy (Proxy(..))

type Scenario =
  { label :: String
  , action :: Aff Unit
  , baselineMeanNs :: Number
  }

type Result =
  { label :: String
  , meanNs :: Number
  , baselineMeanNs :: Number
  , ratio :: Number
  , regressed :: Boolean
  }

-- | Multiplier on the baseline mean above which we call a
-- | regression. 3.0 = "must not be slower than 3x the M1 Pro
-- | baseline." Tunable in one place if we recapture baselines.
tolerance :: Number
tolerance = 3.0

-- | Iteration count for each scenario. Smaller than the headline
-- | suite (1000) to keep the gate fast; the mean is still stable
-- | at this count.
iterations :: Int
iterations = 500

type BenchService = { lookup :: Int -> Int }

stubService :: BenchService
stubService = { lookup: \n -> n + 1 }

bindChain :: forall r e. Int -> RIO r e Int
bindChain n = go 0 n
  where
  go :: Int -> Int -> RIO r e Int
  go acc 0 = pure acc
  go acc k = do
    x <- pure (acc + 1)
    go x (k - 1)

serviceLoop :: forall r' e. Int -> RIO (svc :: BenchService | r') e Int
serviceLoop n = go 0 n
  where
  go :: Int -> Int -> RIO (svc :: BenchService | r') e Int
  go acc 0 = pure acc
  go acc k = do
    svc <- ask (Proxy :: Proxy "svc")
    go (svc.lookup acc) (k - 1)

failCatchOnce :: forall r. RIO r () Int
failCatchOnce =
  catchTag (Proxy :: Proxy "oops") (\n -> pure (n + 1))
    (fail (Proxy :: Proxy "oops") (1 :: Int))

arr32 :: Array Int
arr32 = Array.range 1 32

-- | Hard-coded baselines (mean wall-clock per iteration, in
-- | nanoseconds) captured on Apple M1 Pro / node 20.15.1, taken
-- | from `docs/performance.md`. Update these values when you
-- | intentionally change a hot-path implementation.
scenarios :: Array Scenario
scenarios =
  [ { label: "bind chain (100 binds)"
    , action: void (runRIO' (bindChain 100))
    , baselineMeanNs: 17600.0
    }
  , { label: "bind chain (10000 binds)"
    , action: void (runRIO' (bindChain 10000))
    , baselineMeanNs: 936000.0
    }
  , { label: "ask + Record.get (100 iterations)"
    , action: void
        (runRIO (provideAll { svc: stubService } (serviceLoop 100)))
    , baselineMeanNs: 11800.0
    }
  , { label: "traverse (32 elements, pure work)"
    , action: void
        (runRIO' (traverseRIO arr32))
    , baselineMeanNs: 9900.0
    }
  , { label: "parTraverse (32 elements, pure work)"
    , action: void
        (runRIO' (parTraverse pureInc arr32))
    , baselineMeanNs: 28400.0
    }
  , { label: "fail + catchTag (1 round-trip)"
    , action: void (runRIO' failCatchOnce)
    , baselineMeanNs: 930.0
    }
  , { label: "baseline runRIO' (pure unit)"
    , action: void (runRIO' (pure unit))
    , baselineMeanNs: 265.0
    }
  , { label: "baseline Aff (pure unit)"
    , action: pure unit
    , baselineMeanNs: 150.0
    }
  ]
  where
  pureInc :: Int -> RIO () () Int
  pureInc n = pure (n + 1)

  traverseRIO :: Array Int -> RIO () () (Array Int)
  traverseRIO = traverse pureInc

runOne :: Scenario -> Aff Result
runOne s = do
  res <- benchAffResult iterations s.action
  let
    ratio = res.mean / s.baselineMeanNs
  pure
    { label: s.label
    , meanNs: res.mean
    , baselineMeanNs: s.baselineMeanNs
    , ratio
    , regressed: ratio > tolerance
    }

main :: Effect Unit
main = launchAff_ do
  liftEffect do
    log ""
    log "================================================================"
    log "  rio perf regression gate"
    log "================================================================"
    log ("  iterations per scenario: " <> show iterations)
    log ("  tolerance: " <> showFixed 2 tolerance <> "x of baseline mean")
    log "  baseline: M1 Pro / node 20 (from docs/performance.md)"
    log ""

  results <- traverse runOne scenarios

  liftEffect do
    log "scenario                                              mean         baseline     ratio  status"
    log "---------------------------------------------------------------------------------------------"
    for_ results printRow
    log ""
    let regressions = Array.filter _.regressed results
    case Array.length regressions of
      0 -> do
        log "OK: all scenarios within tolerance."
        exit 0
      n -> do
        log ("FAIL: " <> show n <> " scenario(s) regressed past tolerance.")
        log ""
        for_ regressions \r -> do
          log
            ( "  - "
                <> r.label
                <> ": "
                <> withUnits r.meanNs
                <> " is "
                <> showFixed 2 r.ratio
                <> "x baseline ("
                <> withUnits r.baselineMeanNs
                <> ")"
            )
        exit 1

printRow :: Result -> Effect Unit
printRow r =
  log
    ( padLabel r.label
        <> " "
        <> padCell (withUnits r.meanNs)
        <> " "
        <> padCell (withUnits r.baselineMeanNs)
        <> " "
        <> padRatio (showFixed 2 r.ratio <> "x")
        <> " "
        <> (if r.regressed then "FAIL" else "ok")
    )

padLabel :: String -> String
padLabel = padRight 52

padCell :: String -> String
padCell = padRight 12

padRatio :: String -> String
padRatio = padRight 6

padRight :: Int -> String -> String
padRight n s =
  let len = String.length s
  in if len >= n then s
     else s <> fromCharArray (Array.replicate (n - len) ' ')

showFixed :: Int -> Number -> String
showFixed digits = toStringWith (fixed digits)
