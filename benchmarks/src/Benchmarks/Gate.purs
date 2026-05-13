-- | Performance regression gate.
-- |
-- | Runs the same scenarios as `Benchmarks.Main`, compares each
-- | one's mean wall-clock per iteration against a baseline picked
-- | by profile, and exits with a non-zero code when any scenario
-- | exceeds its tolerance.
-- |
-- | The profile is selected via the `RIO_GATE_PROFILE` environment
-- | variable. Two profiles ship with the gate:
-- |
-- |   * `local-m1-pro` (default) -- captured on an Apple M1 Pro
-- |     with node 20. This is the developer-runnable check.
-- |   * `ci-ubuntu-latest` -- captured on the GitHub Actions
-- |     `ubuntu-latest` runner with node 20. This is what CI runs.
-- |
-- | The tolerance is a deliberately generous 3x of the baseline
-- | mean so the gate catches catastrophic regressions (an
-- | accidental O(n^2), a re-wrapping bug doubling per-bind
-- | overhead) without flapping on machine-to-machine variance.
-- |
-- | The gate also prints a `BASELINE_JSON` line for every run
-- | that contains the observed means per scenario. CI mines that
-- | line out of the workflow log when capturing or updating the
-- | `ci-ubuntu-latest` baseline. See `docs/performance.md` for the
-- | full procedure.
module Benchmarks.Gate
  ( main
  ) where

import Prelude

import Benchmarks.Exit (exit)
import Benchmarks.Harness (benchAffResult, withUnits)
import Data.Array (filter, length, range, replicate, uncons) as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toStringWith, fixed)
import Data.String (length) as String
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.Process (lookupEnv)
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
  , key :: String
  , action :: Aff Unit
  }

type Result =
  { label :: String
  , key :: String
  , meanNs :: Number
  , baselineMeanNs :: Number
  , ratio :: Number
  , regressed :: Boolean
  }

type Profile =
  { name :: String
  , describe :: String
  , lookupBaseline :: String -> Maybe Number
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

-- | Scenarios under measurement. Each carries a stable `key` that
-- | the profile uses to look up its baseline value; a missing key
-- | is treated as "unbaselined" and shown as `n/a` in the report
-- | without contributing to the regression count.
scenarios :: Array Scenario
scenarios =
  [ { key: "bindChain.100"
    , label: "bind chain (100 binds)"
    , action: void (runRIO' (bindChain 100))
    }
  , { key: "bindChain.10000"
    , label: "bind chain (10000 binds)"
    , action: void (runRIO' (bindChain 10000))
    }
  , { key: "ask.100"
    , label: "ask + Record.get (100 iterations)"
    , action: void
        (runRIO (provideAll { svc: stubService } (serviceLoop 100)))
    }
  , { key: "traverse.32"
    , label: "traverse (32 elements, pure work)"
    , action: void
        (runRIO' (traverseRIO arr32))
    }
  , { key: "parTraverse.32"
    , label: "parTraverse (32 elements, pure work)"
    , action: void
        (runRIO' (parTraverse pureInc arr32))
    }
  , { key: "failCatch.1"
    , label: "fail + catchTag (1 round-trip)"
    , action: void (runRIO' failCatchOnce)
    }
  , { key: "runRIO.unit"
    , label: "baseline runRIO' (pure unit)"
    , action: void (runRIO' (pure unit))
    }
  , { key: "aff.unit"
    , label: "baseline Aff (pure unit)"
    , action: pure unit
    }
  ]
  where
  pureInc :: Int -> RIO () () Int
  pureInc n = pure (n + 1)

  traverseRIO :: Array Int -> RIO () () (Array Int)
  traverseRIO = traverse pureInc

-- | Baseline mean wall-clock per iteration, in nanoseconds,
-- | captured on Apple M1 Pro / node 20.15.1. Update these when
-- | you intentionally change a hot-path implementation.
localM1ProBaseline :: String -> Maybe Number
localM1ProBaseline = case _ of
  "bindChain.100" -> Just 17600.0
  "bindChain.10000" -> Just 936000.0
  "ask.100" -> Just 11800.0
  "traverse.32" -> Just 9900.0
  "parTraverse.32" -> Just 28400.0
  "failCatch.1" -> Just 930.0
  "runRIO.unit" -> Just 265.0
  "aff.unit" -> Just 150.0
  _ -> Nothing

-- | Baseline for the GitHub Actions `ubuntu-latest` runner on
-- | node 20. Captured by mining the `BASELINE_JSON` line from a
-- | CI run; see `docs/performance.md` for the procedure. Keys
-- | without a value fall through to `Nothing` and the gate skips
-- | them (informational, no regression).
ciUbuntuLatestBaseline :: String -> Maybe Number
ciUbuntuLatestBaseline = case _ of
  _ -> Nothing

profiles :: String -> Maybe Profile
profiles = case _ of
  "local-m1-pro" -> Just
    { name: "local-m1-pro"
    , describe: "Apple M1 Pro / node 20 (from docs/performance.md)"
    , lookupBaseline: localM1ProBaseline
    }
  "ci-ubuntu-latest" -> Just
    { name: "ci-ubuntu-latest"
    , describe: "GitHub Actions ubuntu-latest / node 20"
    , lookupBaseline: ciUbuntuLatestBaseline
    }
  _ -> Nothing

defaultProfileName :: String
defaultProfileName = "local-m1-pro"

runOne :: Profile -> Scenario -> Aff Result
runOne profile s = do
  res <- benchAffResult iterations s.action
  let
    baseline = profile.lookupBaseline s.key
    ratio = case baseline of
      Just b -> res.mean / b
      Nothing -> 0.0
    regressed = case baseline of
      Just _ -> ratio > tolerance
      Nothing -> false
  pure
    { label: s.label
    , key: s.key
    , meanNs: res.mean
    , baselineMeanNs: fromMaybe 0.0 baseline
    , ratio
    , regressed
    }

main :: Effect Unit
main = launchAff_ do
  profile <- liftEffect resolveProfile
  liftEffect do
    log ""
    log "================================================================"
    log "  rio perf regression gate"
    log "================================================================"
    log ("  profile: " <> profile.name)
    log ("  iterations per scenario: " <> show iterations)
    log ("  tolerance: " <> showFixed 2 tolerance <> "x of baseline mean")
    log ("  baseline: " <> profile.describe)
    log ""

  results <- traverse (runOne profile) scenarios

  liftEffect do
    log
      "scenario                                              mean         baseline     ratio  status"
    log
      "---------------------------------------------------------------------------------------------"
    for_ results (printRow profile)
    log ""
    log (renderBaselineJson profile.name results)
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

-- | Resolve the active profile from `RIO_GATE_PROFILE`. An unset
-- | or empty value falls back to `local-m1-pro`. An unknown name
-- | is fatal so a typo on CI can't silently degrade the gate to a
-- | no-op.
resolveProfile :: Effect Profile
resolveProfile = do
  raw <- lookupEnv "RIO_GATE_PROFILE"
  let
    requested = case raw of
      Just s | s /= "" -> s
      _ -> defaultProfileName
  case profiles requested of
    Just p -> pure p
    Nothing -> do
      log
        ( "ERROR: unknown RIO_GATE_PROFILE: "
            <> requested
            <> ". Known profiles: local-m1-pro, ci-ubuntu-latest."
        )
      exit 2
      pure
        { name: requested
        , describe: ""
        , lookupBaseline: \_ -> Nothing
        }

printRow :: Profile -> Result -> Effect Unit
printRow _profile r =
  let
    baselineCell = case r.baselineMeanNs of
      0.0 -> "n/a"
      b -> withUnits b
    ratioCell = case r.baselineMeanNs of
      0.0 -> "-"
      _ -> showFixed 2 r.ratio <> "x"
    statusCell = case r.baselineMeanNs of
      0.0 -> "(no baseline)"
      _ -> if r.regressed then "FAIL" else "ok"
  in
    log
      ( padLabel r.label
          <> " "
          <> padCell (withUnits r.meanNs)
          <> " "
          <> padCell baselineCell
          <> " "
          <> padRatio ratioCell
          <> " "
          <> statusCell
      )

-- | Emit a single-line JSON-ish blob containing the observed
-- | means per scenario key. CI workflows extract this line from
-- | the job log to capture or refresh a profile's baseline.
renderBaselineJson :: String -> Array Result -> String
renderBaselineJson profileName rs =
  "BASELINE_JSON {"
    <> "\"profile\":\""
    <> profileName
    <> "\",\"means\":{"
    <> joinComma (map oneEntry rs)
    <> "}}"
  where
  oneEntry :: Result -> String
  oneEntry r =
    "\"" <> r.key <> "\":" <> showFixed 1 r.meanNs

joinComma :: Array String -> String
joinComma = go ""
  where
  go acc xs = case Array.uncons xs of
    Nothing -> acc
    Just { head, tail } ->
      if acc == "" then go head tail
      else go (acc <> "," <> head) tail

padLabel :: String -> String
padLabel = padRight 52

padCell :: String -> String
padCell = padRight 12

padRatio :: String -> String
padRatio = padRight 6

padRight :: Int -> String -> String
padRight n s =
  let
    len = String.length s
  in
    if len >= n then s
    else s <> fromCharArray (Array.replicate (n - len) ' ')

showFixed :: Int -> Number -> String
showFixed digits = toStringWith (fixed digits)
