-- | Entry point for the Phase 4 review stress test.
-- |
-- | Run with `npx spago run -p spike-phase-4-review`. Exits non-zero
-- | if any iteration's finalizer invariant fails.
module Spike.Phase4Review.Main
  ( main
  , runScenario
  , runKillScenario
  ) where

import Prelude

import Data.Array (filter, length, mapMaybe, range, reverse) as Array
import Data.Foldable (for_)
import Data.Int (toNumber, fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), stripPrefix)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay, joinFiber, killFiber, launchAff_)
import Effect.Aff (forkAff) as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error, throwException)
import Effect.Random (randomInt)
import Effect.Ref as Ref

import Spike.Phase4Review.Stress
  ( Termination(..)
  , runDeepNested
  , runDeepNestedSleep
  )

-- | Total iterations for each scenario kind. Picked to give a useful
-- | sample while keeping the run under a second on a developer laptop.
deepDepth :: Int
deepDepth = 1000

deepIterations :: Int
deepIterations = 50

killIterations :: Int
killIterations = 50

-- | Maximum sleep used by the kill scenario. The kill fires at a
-- | uniformly-random time within this window so we exercise both
-- | early and late kills.
killMaxMillis :: Int
killMaxMillis = 40

main :: Effect Unit
main = launchAff_ do
  log
    ( "Phase 4 review: " <> show deepIterations
        <> " deep-nested runs at depth "
        <> show deepDepth
        <> ", then "
        <> show killIterations
        <> " kill runs."
    )

  deepResults <- traverse runDeep (Array.range 0 (deepIterations - 1))
  killResults <- traverse runKill (Array.range 0 (killIterations - 1))

  let
    allResults = deepResults <> killResults
    failures = Array.filter (\r -> not r.ok) allResults

  if Array.length failures == 0 then
    log
      ( "OK: " <> show (Array.length allResults)
          <> " stress runs, zero leaks."
      )
  else do
    log
      ( "FAIL: " <> show (Array.length failures) <> " of "
          <> show (Array.length allResults)
          <> " runs leaked or violated LIFO."
      )
    for_ failures \f ->
      log ("  " <> f.label <> ": " <> f.reason)
    liftEffect (throwException (error "phase-4 review stress test failed"))

type RunResult =
  { label :: String
  , ok :: Boolean
  , reason :: String
  }

runDeep :: Int -> Aff RunResult
runDeep i = do
  term <- liftEffect randomTermination
  failAt <- liftEffect (randomInt 0 (deepDepth - 1))
  runScenario { iteration: i, target: deepDepth, termination: term, failAt }

runKill :: Int -> Aff RunResult
runKill i = do
  sleepMs <- liftEffect (randomInt 5 (killMaxMillis - 1))
  killAt <- liftEffect (randomInt 1 (killMaxMillis - 4))
  runKillScenario
    { iteration: i
    , target: deepDepth
    , sleepMs
    , killAfterMs: killAt
    }

-- | Drive one deep-nested iteration. Reports whether the recorded
-- | event log matches the expected register/finalize pairing.
runScenario
  :: { iteration :: Int
     , target :: Int
     , termination :: Termination
     , failAt :: Int
     }
  -> Aff RunResult
runScenario opts = do
  events <- liftEffect (Ref.new [])
  _ <- attempt
    (runDeepNested opts.target opts.termination opts.failAt events)
  log' <- liftEffect (Ref.read events)
  let
    deepest = opts.failAt
    expectedRegisters = Array.range 0 deepest
    expectedFinalizes = Array.reverse expectedRegisters
    actualRegisters = collect "register-" log'
    actualFinalizes = collect "finalize-" log'
    label =
      "deep[" <> show opts.iteration <> "] term="
        <> showTerm opts.termination
        <> " failAt="
        <> show opts.failAt
  pure $ check label
    { expectedRegisters
    , expectedFinalizes
    , actualRegisters
    , actualFinalizes
    }

-- | Drive one kill-mid-flight iteration. Fork the program, wait
-- | `killAfterMs`, then kill. The kill must arrive during the
-- | innermost sleep; check that every register has a matching
-- | finalize and that finalize order is LIFO.
runKillScenario
  :: { iteration :: Int
     , target :: Int
     , sleepMs :: Int
     , killAfterMs :: Int
     }
  -> Aff RunResult
runKillScenario opts = do
  events <- liftEffect (Ref.new [])
  fib <- Aff.forkAff
    ( runDeepNestedSleep opts.target
        (Milliseconds (toNumber opts.sleepMs))
        events
    )
  delay (Milliseconds (toNumber opts.killAfterMs))
  killFiber (error "phase-4-review-kill") fib
  _ <- attempt (joinFiber fib)
  log' <- liftEffect (Ref.read events)
  let
    actualRegisters = collect "register-" log'
    actualFinalizes = collect "finalize-" log'
    expectedRegisters = Array.range 0 (opts.target - 1)
    expectedFinalizes = Array.reverse expectedRegisters
    label =
      "kill[" <> show opts.iteration <> "] sleepMs="
        <> show opts.sleepMs
        <> " killAfterMs="
        <> show opts.killAfterMs
  pure $ check label
    { expectedRegisters
    , expectedFinalizes
    , actualRegisters
    , actualFinalizes
    }

check
  :: String
  -> { expectedRegisters :: Array Int
     , expectedFinalizes :: Array Int
     , actualRegisters :: Array Int
     , actualFinalizes :: Array Int
     }
  -> RunResult
check label r
  | r.actualRegisters /= r.expectedRegisters =
      { label
      , ok: false
      , reason: "register order mismatch (saw "
          <> show r.actualRegisters
          <> ")"
      }
  | r.actualFinalizes /= r.expectedFinalizes =
      { label
      , ok: false
      , reason: "finalize order mismatch (saw "
          <> show r.actualFinalizes
          <> ")"
      }
  | otherwise = { label, ok: true, reason: "" }

collect :: String -> Array String -> Array Int
collect prefix = Array.mapMaybe \s ->
  case stripPrefix (Pattern prefix) s of
    Nothing -> Nothing
    Just rest -> fromString rest

randomTermination :: Effect Termination
randomTermination = do
  k <- randomInt 0 2
  pure case k of
    0 -> Succeed
    1 -> TypedFail
    _ -> Defect

showTerm :: Termination -> String
showTerm = case _ of
  Succeed -> "Succeed"
  TypedFail -> "TypedFail"
  Defect -> "Defect"
