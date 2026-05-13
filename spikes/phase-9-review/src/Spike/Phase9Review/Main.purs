-- | Entry point for the Phase 9 (v0.3) review stress test.
-- |
-- | Run with `npx spago run -p spike-phase-9-review`. The harness
-- | drives each scenario for `iterations` iterations with random
-- | parameters and exits non-zero if any invariant is violated
-- | (annotation residue in the Logger backend, drifted `Local`
-- | cell, count/sum mismatch on the `TQueue` or `THub` consumers).
-- |
-- | Total iterations across all four scenarios is `4 * iterations`.
-- | Defaults to 250 per scenario (1000 total) so a single run
-- | completes in a few seconds; CI runs the harness on every PR
-- | for cumulative coverage.
module Spike.Phase9Review.Main
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

import Spike.Phase9Review.Stress
  ( ScenarioResult
  , hubBoundedScenario
  , hubDroppingScenario
  , hubScenario
  , hubSlidingScenario
  , localScenario
  , loggerScenario
  , queueScenario
  , semaphoreScenario
  )

-- | Iterations per scenario. Picked to keep one full run well
-- | under five seconds while still exercising every code path.
iterations :: Int
iterations = 250

main :: Effect Unit
main = launchAff_ do
  log
    ( "Phase 9 review: " <> show iterations
        <> " iterations per scenario across Logger, Local, "
        <> "TQueue, THub (Unbounded / Bounded / Sliding / Dropping), "
        <> "and TSemaphore."
    )

  loggerResults <- traverse runLogger range'
  localResults <- traverse runLocal range'
  queueResults <- traverse runQueue range'
  hubResults <- traverse runHub range'
  hubBoundedResults <- traverse runHubBounded range'
  hubSlidingResults <- traverse runHubSliding range'
  hubDroppingResults <- traverse runHubDropping range'
  semResults <- traverse runSem range'

  let
    all = loggerResults <> localResults <> queueResults
      <> hubResults
      <> hubBoundedResults
      <> hubSlidingResults
      <> hubDroppingResults
      <> semResults
    failures = Array.filter (\r -> not r.result.ok) all

  if Array.length failures == 0 then
    log
      ( "OK: " <> show (Array.length all)
          <> " stress iterations, every invariant held."
      )
  else do
    log
      ( "FAIL: " <> show (Array.length failures) <> " of "
          <> show (Array.length all)
          <> " iterations violated their invariant."
      )
    for_ failures \f ->
      log
        ( "  " <> f.label <> ": detail=" <> show f.result.detail
            <> " ("
            <> f.params
            <> ")"
        )
    liftEffect (throwException (error "phase-9 review stress test failed"))

range' :: Array Int
range' = Array.range 0 (iterations - 1)

type Labeled =
  { label :: String
  , params :: String
  , result :: ScenarioResult
  }

runLogger :: Int -> Aff Labeled
runLogger i = do
  depth <- liftEffect (randomInt 1 8)
  failPct <- liftEffect (randomInt 0 50)
  forkPct <- liftEffect (randomInt 0 50)
  r <- loggerScenario { depth, failPct, forkPct }
  pure
    { label: "logger[" <> show i <> "]"
    , params:
        "depth=" <> show depth
          <> " failPct="
          <> show failPct
          <> " forkPct="
          <> show forkPct
    , result: r
    }

runLocal :: Int -> Aff Labeled
runLocal i = do
  depth <- liftEffect (randomInt 1 8)
  failPct <- liftEffect (randomInt 0 50)
  forkPct <- liftEffect (randomInt 0 50)
  killPct <- liftEffect (randomInt 0 50)
  r <- localScenario { depth, failPct, forkPct, killPct }
  pure
    { label: "local[" <> show i <> "]"
    , params:
        "depth=" <> show depth
          <> " failPct="
          <> show failPct
          <> " forkPct="
          <> show forkPct
          <> " killPct="
          <> show killPct
    , result: r
    }

runQueue :: Int -> Aff Labeled
runQueue i = do
  producers <- liftEffect (randomInt 1 4)
  consumers <- liftEffect (randomInt 1 4)
  perProducer <- liftEffect (randomInt 4 16)
  r <- queueScenario { producers, consumers, perProducer }
  pure
    { label: "queue[" <> show i <> "]"
    , params:
        "producers=" <> show producers
          <> " consumers="
          <> show consumers
          <> " perProducer="
          <> show perProducer
    , result: r
    }

runHub :: Int -> Aff Labeled
runHub i = do
  subscribers <- liftEffect (randomInt 1 5)
  publishCount <- liftEffect (randomInt 4 20)
  r <- hubScenario { subscribers, publishCount }
  pure
    { label: "hub[" <> show i <> "]"
    , params:
        "subscribers=" <> show subscribers
          <> " publishCount="
          <> show publishCount
    , result: r
    }

runHubBounded :: Int -> Aff Labeled
runHubBounded i = do
  buffer <- liftEffect (randomInt 2 6)
  publishCount <- liftEffect (randomInt (buffer + 4) (buffer * 4))
  r <- hubBoundedScenario { buffer, publishCount }
  pure
    { label: "hubBounded[" <> show i <> "]"
    , params:
        "buffer=" <> show buffer
          <> " publishCount="
          <> show publishCount
    , result: r
    }

runHubSliding :: Int -> Aff Labeled
runHubSliding i = do
  buffer <- liftEffect (randomInt 2 6)
  publishCount <- liftEffect (randomInt (buffer + 2) (buffer * 3))
  r <- hubSlidingScenario { buffer, publishCount }
  pure
    { label: "hubSliding[" <> show i <> "]"
    , params:
        "buffer=" <> show buffer
          <> " publishCount="
          <> show publishCount
    , result: r
    }

runHubDropping :: Int -> Aff Labeled
runHubDropping i = do
  buffer <- liftEffect (randomInt 2 6)
  publishCount <- liftEffect (randomInt (buffer + 2) (buffer * 3))
  r <- hubDroppingScenario { buffer, publishCount }
  pure
    { label: "hubDropping[" <> show i <> "]"
    , params:
        "buffer=" <> show buffer
          <> " publishCount="
          <> show publishCount
    , result: r
    }

runSem :: Int -> Aff Labeled
runSem i = do
  permits <- liftEffect (randomInt 2 5)
  workers <- liftEffect (randomInt 3 12)
  failPct <- liftEffect (randomInt 0 40)
  killPct <- liftEffect (randomInt 0 40)
  holdMs <- liftEffect (randomInt 1 6)
  r <- semaphoreScenario { permits, workers, failPct, killPct, holdMs }
  pure
    { label: "sem[" <> show i <> "]"
    , params:
        "permits=" <> show permits
          <> " workers="
          <> show workers
          <> " failPct="
          <> show failPct
          <> " killPct="
          <> show killPct
          <> " holdMs="
          <> show holdMs
    , result: r
    }
