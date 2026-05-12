module Spike.AffInterruption.Main where

-- This spike exercises Aff's cancellation primitives to find out what
-- guarantees RIO can build on top of. See FINDINGS.md for results.
--
-- Each scenario prints PASS/FAIL with a short message. The harness exits 0
-- if every scenario produced a result we know how to interpret (even
-- "doesn't work, gap identified") and non-zero only on infrastructural
-- failure (e.g. an Aff throws an unexpected exception).

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, bracket, delay, error, forkAff, joinFiber, killFiber, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref

main :: Effect Unit
main = launchAff_ do
  Console.log "=== Spike 0.5: Aff cancellation and interruption ==="
  scenario1
  scenario2
  scenario2b
  scenario3
  scenario4
  scenario5
  scenario6
  scenario7
  Console.log "=== Done ==="

note :: String -> Aff Unit
note s = Console.log $ "  " <> s

header :: String -> Aff Unit
header s = Console.log $ "\n[" <> s <> "]"

-- ---------------------------------------------------------------------------
-- Scenario 1: interrupt during a long sleep.
--
-- Spawns a fiber that sleeps 5s, kills it after 50ms, then attempts to join.
-- Expected: kill takes effect promptly; join surfaces the kill exception.
-- ---------------------------------------------------------------------------
scenario1 :: Aff Unit
scenario1 = do
  header "S1: interrupt during sleep"
  startedAt <- nowMs
  fib <- forkAff do
    delay (Milliseconds 5000.0)
    pure "shouldn't reach"
  delay (Milliseconds 50.0)
  killFiber (error "S1-interrupt") fib
  result <- attempt (joinFiber fib)
  endedAt <- nowMs
  let elapsed = endedAt - startedAt
  note $ "elapsed=" <> show elapsed <> "ms"
  case result of
    Left e -> note $ "join after kill: threw, message=" <> message e
    Right v -> note $ "join after kill: returned " <> show v <> " (UNEXPECTED)"

-- ---------------------------------------------------------------------------
-- Scenario 2: "synchronous" Aff work and interruption.
--
-- Aff is CPS over an interpreter, so even seemingly-synchronous chains have
-- yield points between each bind. We fork a long monadic chain of pure
-- increments, kill it, and observe how far it got.
-- ---------------------------------------------------------------------------
scenario2 :: Aff Unit
scenario2 = do
  header "S2: interrupt 'synchronous' bind chain"
  counter <- liftEffect $ Ref.new 0
  let
    bumpN :: Int -> Aff Unit
    bumpN 0 = pure unit
    bumpN n = do
      liftEffect $ Ref.modify_ (_ + 1) counter
      bumpN (n - 1)
  fib <- forkAff (bumpN 1000000)
  -- Yield immediately so the fiber can start; then kill.
  delay (Milliseconds 0.0)
  killFiber (error "S2-interrupt") fib
  _ <- attempt (joinFiber fib)
  finalCount <- liftEffect $ Ref.read counter
  note $ "counter after kill: " <> show finalCount <> " (of 1000000)"
  note $
    if finalCount < 1000000 then "stopped early -> interruption works between binds"
    else "ran to completion -> no interruption"

-- ---------------------------------------------------------------------------
-- Scenario 2b: same loop, but with an explicit `delay 0` yield every 100
-- iterations. Distinguishes "loop finished before kill arrived" from "Aff
-- doesn't interrupt CPU-bound work."
-- ---------------------------------------------------------------------------
scenario2b :: Aff Unit
scenario2b = do
  header "S2b: interrupt loop WITH explicit yields"
  counter <- liftEffect $ Ref.new 0
  let
    bumpYield :: Int -> Aff Unit
    bumpYield 0 = pure unit
    bumpYield n = do
      liftEffect $ Ref.modify_ (_ + 1) counter
      when (mod n 100 == 0) (delay (Milliseconds 0.0))
      bumpYield (n - 1)
  fib <- forkAff (bumpYield 1000000)
  delay (Milliseconds 5.0)
  killFiber (error "S2b-interrupt") fib
  _ <- attempt (joinFiber fib)
  finalCount <- liftEffect $ Ref.read counter
  note $ "counter after kill: " <> show finalCount <> " (of 1000000)"
  note $
    if finalCount < 1000000 then "stopped early -> yields make loops interruptible"
    else "ran to completion -> kill missed even with yields"

-- ---------------------------------------------------------------------------
-- Scenario 3: bracket release runs when fiber is killed mid-use.
--
-- The release path must run even when the user phase is interrupted.
-- ---------------------------------------------------------------------------
scenario3 :: Aff Unit
scenario3 = do
  header "S3: bracket release on kill during 'use'"
  acquired <- liftEffect $ Ref.new false
  used <- liftEffect $ Ref.new false
  released <- liftEffect $ Ref.new false
  fib <- forkAff $ bracket
    (liftEffect (Ref.write true acquired) *> pure "resource")
    (\_ -> liftEffect (Ref.write true released))
    ( \_ -> do
        liftEffect (Ref.write true used)
        delay (Milliseconds 5000.0)
        pure unit
    )
  delay (Milliseconds 50.0)
  killFiber (error "S3-interrupt") fib
  -- Give the runtime time to run the release; killFiber should wait for
  -- finalizers to complete in Aff's model, but we await it explicitly.
  _ <- attempt (joinFiber fib)
  a <- liftEffect $ Ref.read acquired
  u <- liftEffect $ Ref.read used
  r <- liftEffect $ Ref.read released
  note $ "acquired=" <> show a <> " used=" <> show u <> " released=" <> show r
  note $ if a && u && r then "PASS: release ran" else "FAIL: release did not run"

-- ---------------------------------------------------------------------------
-- Scenario 4: kill of a fiber that has already completed.
--
-- Expected: kill is a no-op (or innocuous); join still returns the result.
-- ---------------------------------------------------------------------------
scenario4 :: Aff Unit
scenario4 = do
  header "S4: kill of already-completed fiber"
  fib <- forkAff (pure 42)
  -- Wait for completion via join (it should resolve immediately).
  r1 <- joinFiber fib
  killFiber (error "S4-late-kill") fib
  r2 <- attempt (joinFiber fib)
  note $ "first join=" <> show r1
  case r2 of
    Left e -> note $ "second join after late-kill: threw " <> message e
    Right v -> note $ "second join after late-kill: returned " <> show v

-- ---------------------------------------------------------------------------
-- Scenario 5: kill of a fiber that has already been killed.
-- ---------------------------------------------------------------------------
scenario5 :: Aff Unit
scenario5 = do
  header "S5: double kill"
  fib <- forkAff (delay (Milliseconds 5000.0))
  delay (Milliseconds 20.0)
  killFiber (error "S5-kill-1") fib
  -- Second kill of the same fiber.
  killFiber (error "S5-kill-2") fib
  r <- attempt (joinFiber fib)
  case r of
    Left e -> note $ "join: threw " <> message e
    Right _ -> note "join: returned unit"
  note "(double kill survived without crashing the runtime)"

-- ---------------------------------------------------------------------------
-- Scenario 6: kill DURING a release/finalizer phase.
--
-- Builds a bracket whose release itself takes time. Kills the fiber, then
-- attempts to kill again while the release is running. We want to know
-- whether the release is uninterruptible by default.
-- ---------------------------------------------------------------------------
scenario6 :: Aff Unit
scenario6 = do
  header "S6: kill during release phase"
  releaseStarted <- liftEffect $ Ref.new false
  releaseFinished <- liftEffect $ Ref.new false
  fib <- forkAff $ bracket
    (pure "resource")
    ( \_ -> do
        liftEffect (Ref.write true releaseStarted)
        delay (Milliseconds 200.0)
        liftEffect (Ref.write true releaseFinished)
    )
    (\_ -> delay (Milliseconds 5000.0))
  delay (Milliseconds 50.0)
  killFiber (error "S6-kill-1") fib
  -- The release is now (presumably) running. Issue a second kill.
  delay (Milliseconds 50.0)
  rs <- liftEffect $ Ref.read releaseStarted
  note $ "after kill+50ms: releaseStarted=" <> show rs
  killFiber (error "S6-kill-2") fib
  -- Wait long enough for the original release window.
  delay (Milliseconds 250.0)
  rf <- liftEffect $ Ref.read releaseFinished
  note $ "releaseFinished=" <> show rf
  note $ if rf then "PASS: release was uninterruptible" else "FAIL: release was interrupted"

-- ---------------------------------------------------------------------------
-- Scenario 7: kill BEFORE the fiber has had a chance to start.
--
-- Aff is CPS, so `forkAff` schedules the computation but does not run any of
-- its body synchronously. If we issue `killFiber` on the same tick as the
-- fork, before yielding, does the fiber execute a step or is the kill
-- effective pre-start?
-- ---------------------------------------------------------------------------
scenario7 :: Aff Unit
scenario7 = do
  header "S7: kill before fiber starts"
  ran <- liftEffect $ Ref.new false
  fib <- forkAff do
    liftEffect (Ref.write true ran)
    delay (Milliseconds 5000.0)
  -- No `delay` here: kill in the same Aff turn as the fork.
  killFiber (error "S7-pre-start") fib
  r <- attempt (joinFiber fib)
  observed <- liftEffect $ Ref.read ran
  note $ "fiber body executed: " <> show observed
  case r of
    Left e -> note $ "join: threw " <> message e
    Right _ -> note "join: returned unit"
  note $
    if not observed then "PASS: kill landed pre-start, body never ran"
    else "NOTE: fiber ran one step before kill"

-- ---------------------------------------------------------------------------
-- Tiny helpers.
-- ---------------------------------------------------------------------------

nowMs :: Aff Number
nowMs = liftEffect now
  where
  now :: Effect Number
  now = nowImpl

foreign import nowImpl :: Effect Number
