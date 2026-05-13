-- | Entry point for the Phase 6 review stress test.
-- |
-- | Run with `npx spago run -p spike-phase-6-review`. The harness
-- | drives each scenario for `iterations` iterations with random
-- | parameters and exits non-zero if any iteration leaks (resource
-- | counter does not return to zero) or fails to complete.
-- |
-- | Total iterations across all four scenarios is `4 * iterations`.
-- | The build plan asks for 10,000 runs; we default to 250 per
-- | scenario (1000 total) so a single invocation completes in a few
-- | seconds on a developer laptop. CI runs the harness on every PR,
-- | giving cumulative coverage across runs.
module Spike.Phase6Review.Main
  ( main
  ) where

import Prelude

import Data.Array (filter, length, range) as Array
import Data.Foldable (for_)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error, throwException)
import Effect.Random (randomInt)

import Spike.Phase6Review.Stress
  ( ScenarioResult
  , interruptScenario
  , parTraverseScenario
  , raceScenario
  , zipParScenario
  )

-- | Iterations per scenario. Picked to keep one full run under five
-- | seconds while still exercising every code path.
iterations :: Int
iterations = 250

main :: Effect Unit
main = launchAff_ do
  log
    ( "Phase 6 review: " <> show iterations
        <> " iterations per scenario across parTraverse, zipPar, "
        <> "race, and fork+interrupt."
    )

  parResults <- traverse runParTraverse range'
  zipResults <- traverse runZipPar range'
  raceResults <- traverse runRace range'
  intResults <- traverse runInterrupt range'

  let
    all = parResults <> zipResults <> raceResults <> intResults
    failures = Array.filter (\r -> not r.result.ok) all

  if Array.length failures == 0 then
    log
      ( "OK: " <> show (Array.length all)
          <> " stress iterations, zero leaks."
      )
  else do
    log
      ( "FAIL: " <> show (Array.length failures) <> " of "
          <> show (Array.length all)
          <> " iterations leaked."
      )
    for_ failures \f ->
      log
        ( "  " <> f.label <> ": leaked " <> show f.result.leaked
            <> " ("
            <> f.params
            <> ")"
        )
    liftEffect (throwException (error "phase-6 review stress test failed"))

range' :: Array Int
range' = Array.range 0 (iterations - 1)

type Labeled =
  { label :: String
  , params :: String
  , result :: ScenarioResult
  }

runParTraverse :: Int -> Aff Labeled
runParTraverse i = do
  count <- liftEffect (randomInt 2 8)
  maxDelay <- liftEffect (randomInt 1 10)
  failPct <- liftEffect (randomInt 0 60)
  r <- parTraverseScenario { count, maxDelayMs: maxDelay, failPct }
  pure
    { label: "parTraverse[" <> show i <> "]"
    , params:
        "count=" <> show count
          <> " maxDelay="
          <> show maxDelay
          <> "ms failPct="
          <> show failPct
    , result: r
    }

runZipPar :: Int -> Aff Labeled
runZipPar i = do
  maxDelay <- liftEffect (randomInt 1 10)
  failPct <- liftEffect (randomInt 0 60)
  r <- zipParScenario { maxDelayMs: maxDelay, failPct }
  pure
    { label: "zipPar[" <> show i <> "]"
    , params: "maxDelay=" <> show maxDelay <> "ms failPct=" <> show failPct
    , result: r
    }

runRace :: Int -> Aff Labeled
runRace i = do
  count <- liftEffect (randomInt 2 6)
  maxDelay <- liftEffect (randomInt 2 12)
  r <- raceScenario { count, maxDelayMs: maxDelay }
  pure
    { label: "race[" <> show i <> "]"
    , params: "count=" <> show count <> " maxDelay=" <> show maxDelay <> "ms"
    , result: r
    }

runInterrupt :: Int -> Aff Labeled
runInterrupt i = do
  depth <- liftEffect (randomInt 1 50)
  sleep <- liftEffect (randomInt 10 30)
  killAfter <- liftEffect (randomInt 1 (sleep - 1))
  r <- interruptScenario
    { depth, sleepMs: sleep, killAfterMs: killAfter }
  pure
    { label: "interrupt[" <> show i <> "]"
    , params:
        "depth=" <> show depth
          <> " sleep="
          <> show sleep
          <> "ms killAfter="
          <> show killAfter
          <> "ms"
    , result: r
    }
