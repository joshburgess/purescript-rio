-- | Entry point for the Phase 5 review layered application.
-- |
-- | Run with `npx spago run -p spike-phase-5-review`. Exits non-zero
-- | if any scenario's recorded event log does not match expectation.
-- |
-- | Three scenarios drive `appLayer` via `provideLayer`:
-- |
-- |   * Scenario A (happy path). Valid config; program greets three
-- |     users twice each. Asserts:
-- |       - layer init events in order (clock:init, cache-open,
-- |         db-open)
-- |       - six `greet@n` log lines from the user service, with
-- |         monotonic clock indices 1..6
-- |       - layer finalizers run after the program (db-close then
-- |         cache-flush, LIFO with respect to registration)
-- |
-- |   * Scenario B (failing layer). Config with empty `databaseUrl`;
-- |     `dataLayer` fails with `dbConnect`. Asserts:
-- |       - the runner returns `Left dbConnect`
-- |       - no `cache-open` / `db-open` events were recorded (the
-- |         failure happens before either resource opens)
-- |       - the program body never ran
-- |
-- |   * Scenario C (program failure after success). Valid config;
-- |     program uses the service successfully then fails with a
-- |     typed `progBoom`. Asserts:
-- |       - the runner returns `Left progBoom`
-- |       - layer finalizers still ran in LIFO order after the
-- |         program-side failure
module Spike.Phase5Review.Main
  ( main
  , runScenarioA
  , runScenarioB
  , runScenarioC
  ) where

import Prelude hiding ((>>>))

import Data.Array (filter, length, range) as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, fail, provideLayer, runRIO)

import Spike.Phase5Review.Layers (appLayer, push)
import Spike.Phase5Review.Services (Config, UserService)

-- | Pretty checkable result of one scenario.
type ScenarioResult =
  { label :: String
  , ok :: Boolean
  , reason :: String
  , events :: Array String
  }

main :: Effect Unit
main = launchAff_ do
  log "Phase 5 review: layered application, three scenarios."

  rA <- runScenarioA
  rB <- runScenarioB
  rC <- runScenarioC

  let
    all = [ rA, rB, rC ]
    failures = Array.filter (\r -> not r.ok) all

  for_ all (\r -> log (formatResult r))

  if Array.length failures == 0 then
    log ("OK: " <> show (Array.length all) <> " scenarios passed.")
  else do
    log
      ( "FAIL: "
          <> show (Array.length failures)
          <> " of "
          <> show (Array.length all)
          <> " scenarios failed."
      )
    liftEffect (throwException (error "phase-5 review failed"))

runScenarioA :: Aff ScenarioResult
runScenarioA = do
  events <- liftEffect (Ref.new [])
  let
    cfg :: Config
    cfg = { databaseUrl: "postgres://demo/db", cacheCap: 32 }

    program
      :: RIO (userService :: UserService) () Unit
    program = do
      us <- ask (Proxy :: Proxy "userService")
      _ <- traverse (\uid -> liftAff (us.greet uid)) (Array.range 1 3)
      _ <- traverse (\uid -> liftAff (us.greet uid)) (Array.range 1 3)
      pure unit

  result <- runRIO (provideLayer (appLayer events cfg) program)
  history <- liftEffect (Ref.read events)
  pure case result of
    Left v ->
      { label: "A: happy path"
      , ok: false
      , reason: "expected Right, got Left " <> showDbConnect v
      , events: history
      }
    Right _ ->
      checkScenarioA history

checkScenarioA :: Array String -> ScenarioResult
checkScenarioA history =
  let
    expected =
      [ "clock:init"
      , "cache-open cap=32"
      , "db-open url=postgres://demo/db"
      , "log:greet@1 uid=1"
      , "log:greet@2 uid=2"
      , "log:greet@3 uid=3"
      , "log:greet@4 uid=1"
      , "log:greet@5 uid=2"
      , "log:greet@6 uid=3"
      , "db-close"
      , "cache-flush"
      ]
  in
    if history /= expected then
      { label: "A: happy path"
      , ok: false
      , reason: "event log mismatch"
      , events: history
      }
    else
      { label: "A: happy path", ok: true, reason: "", events: history }

runScenarioB :: Aff ScenarioResult
runScenarioB = do
  events <- liftEffect (Ref.new [])
  let
    cfg :: Config
    cfg = { databaseUrl: "", cacheCap: 32 }

    program :: RIO (userService :: UserService) () Unit
    program = do
      _ <- ask (Proxy :: Proxy "userService")
      liftAff (push events "should-not-run")

  result <- runRIO (provideLayer (appLayer events cfg) program)
  history <- liftEffect (Ref.read events)
  pure case result of
    Right _ ->
      { label: "B: failing layer"
      , ok: false
      , reason: "expected Left dbConnect, got Right"
      , events: history
      }
    Left v ->
      checkScenarioB history v

checkScenarioB
  :: Array String -> Variant (dbConnect :: String) -> ScenarioResult
checkScenarioB history v
  | history /= [ "clock:init" ] =
      { label: "B: failing layer"
      , ok: false
      , reason:
          "expected only [clock:init] events before failure, saw "
            <> show history
      , events: history
      }
  | otherwise =
      { label: "B: failing layer"
      , ok: true
      , reason: "got " <> showDbConnect v
      , events: history
      }

runScenarioC :: Aff ScenarioResult
runScenarioC = do
  events <- liftEffect (Ref.new [])
  let
    cfg :: Config
    cfg = { databaseUrl: "postgres://demo/db", cacheCap: 32 }

    program
      :: RIO (userService :: UserService) (progBoom :: Unit) Unit
    program = do
      us <- ask (Proxy :: Proxy "userService")
      _ <- liftAff (us.greet 7)
      fail (Proxy :: Proxy "progBoom") unit

    provided
      :: RIO () (dbConnect :: String, progBoom :: Unit) Unit
    provided = provideLayer (appLayer events cfg) program

  result <- runRIO provided
  history <- liftEffect (Ref.read events)
  pure case result of
    Right _ ->
      { label: "C: program failure after use"
      , ok: false
      , reason: "expected Left progBoom, got Right"
      , events: history
      }
    Left _ ->
      checkScenarioC history

checkScenarioC :: Array String -> ScenarioResult
checkScenarioC history =
  let
    expected =
      [ "clock:init"
      , "cache-open cap=32"
      , "db-open url=postgres://demo/db"
      , "log:greet@1 uid=7"
      , "db-close"
      , "cache-flush"
      ]
  in
    if history /= expected then
      { label: "C: program failure after use"
      , ok: false
      , reason: "event log mismatch"
      , events: history
      }
    else
      { label: "C: program failure after use"
      , ok: true
      , reason: ""
      , events: history
      }

showDbConnect :: Variant (dbConnect :: String) -> String
showDbConnect = Variant.match
  { dbConnect: \s -> "dbConnect: " <> s
  }

formatResult :: ScenarioResult -> String
formatResult r =
  let
    tag = if r.ok then "OK  " else "FAIL"
    suffix = if r.reason == "" then "" else " (" <> r.reason <> ")"
  in
    tag <> "  " <> r.label <> suffix
