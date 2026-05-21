-- | A classic three-state circuit breaker.
-- |
-- | A breaker is a small piece of mutable state that observes the
-- | typed-error outcomes of a wrapped action and changes how it
-- | dispatches subsequent calls based on what it sees:
-- |
-- |   * `Closed`: calls pass through. Consecutive typed failures
-- |     are counted; once the count reaches `maxFailures`, the
-- |     breaker trips to `Open`.
-- |   * `Open`: calls fail fast with a typed `circuitOpen :: Unit`
-- |     error without touching the wrapped action. After
-- |     `resetTimeout` has elapsed since the breaker tripped, the
-- |     next call transitions to `HalfOpen`.
-- |   * `HalfOpen`: a single trial call is allowed through. If it
-- |     succeeds, the breaker returns to `Closed` with a fresh
-- |     failure counter. If it raises a typed error, the breaker
-- |     trips back to `Open` with the timer reset.
-- |
-- | The breaker counts typed errors only. Defects and interrupts
-- | propagate through unchanged and do not affect state; their
-- | failure model is "stop the world", which the breaker's fail-
-- | fast model does not improve on. If you want a defect to count
-- | as a failure, sandbox it at the call site (e.g. via `causeOf`)
-- | and rethrow it as a typed error.
-- |
-- | ```purescript
-- | breaker <- CircuitBreaker.make
-- |   { maxFailures: 5, resetTimeout: Milliseconds 30_000.0 }
-- | result <- CircuitBreaker.withBreaker breaker (outboundCall request)
-- |   `catchTag` (Proxy :: Proxy "circuitOpen") \_ ->
-- |     pure (Left ServiceUnavailable)
-- | ```
-- |
-- | Like `RateLimiter`, the breaker reads time through
-- | `RIO.Fiber.Clock` so tests can drive it deterministically.
module RIO.Fiber.CircuitBreaker
  ( CircuitBreaker
  , Config
  , Phase(..)
  , Snapshot
  , make
  , snapshot
  , reset
  , withBreaker
  , tryWithBreaker
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Fiber.Clock (currentEpoch)
import RIO.Fiber.Core (RIO, catchAll, fail, liftEffect)
import RIO.Fiber.Error (rethrow)

-- | Configuration for a fresh breaker.
-- |
-- |   * `maxFailures`: how many consecutive typed-error
-- |     outcomes flip the breaker from `Closed` to `Open`. Must
-- |     be `> 0`; values `<= 0` are coerced to `1`.
-- |   * `resetTimeout`: how long after tripping before the
-- |     next call is allowed through as a trial (`HalfOpen`).
type Config =
  { maxFailures :: Int
  , resetTimeout :: Milliseconds
  }

-- | The breaker's current phase.
data Phase
  = Closed
  | Open
  | HalfOpen

derive instance eqPhase :: Eq Phase

instance showPhase :: Show Phase where
  show Closed = "Closed"
  show Open = "Open"
  show HalfOpen = "HalfOpen"

-- | An advisory read of the breaker's state. `failures` is the
-- | count of consecutive typed errors observed since the breaker
-- | was last in `Closed` with a clean slate; `openedAt` is the
-- | virtual time at which it most recently tripped to `Open`
-- | (`Nothing` when the breaker has never opened or has since
-- | been manually reset).
type Snapshot =
  { phase :: Phase
  , failures :: Int
  , openedAt :: Maybe Number
  }

type State =
  { phase :: Phase
  , failures :: Int
  , openedAt :: Maybe Number
  , maxFailures :: Int
  , resetTimeoutMs :: Number
  }

-- | A circuit breaker. Allocate with `make`; use through
-- | `withBreaker` or `tryWithBreaker`.
newtype CircuitBreaker = CircuitBreaker (Ref State)

-- | Allocate a fresh breaker in the `Closed` state with a clean
-- | failure counter.
make :: forall r e. Config -> RIO r e CircuitBreaker
make cfg = liftEffect $ map CircuitBreaker $ Ref.new
  { phase: Closed
  , failures: 0
  , openedAt: Nothing
  , maxFailures: max 1 cfg.maxFailures
  , resetTimeoutMs:
      let
        Milliseconds ms = cfg.resetTimeout
      in
        ms
  }

-- | Read the breaker's current state. Advisory: the state can
-- | change between the read and any subsequent call.
snapshot :: forall r e. CircuitBreaker -> RIO r e Snapshot
snapshot (CircuitBreaker ref) = liftEffect do
  s <- Ref.read ref
  pure { phase: s.phase, failures: s.failures, openedAt: s.openedAt }

-- | Manually reset the breaker to `Closed` with a clean failure
-- | counter. Useful in administrative paths (a deploy completes,
-- | an operator unblocks a downstream).
reset :: forall r e. CircuitBreaker -> RIO r e Unit
reset (CircuitBreaker ref) = liftEffect $
  Ref.modify_ (\s -> s { phase = Closed, failures = 0, openedAt = Nothing }) ref

expirePhase :: Number -> State -> State
expirePhase nowMs s = case s.phase, s.openedAt of
  Open, Just openedAt
    | nowMs - openedAt >= s.resetTimeoutMs ->
        s { phase = HalfOpen }
  _, _ -> s

shouldProceed :: State -> Boolean
shouldProceed s = case s.phase of
  Closed -> true
  HalfOpen -> true
  Open -> false

onSuccess :: State -> State
onSuccess s = s
  { phase = Closed
  , failures = 0
  , openedAt = Nothing
  }

onFailure :: Number -> State -> State
onFailure nowMs s =
  let
    failures' = s.failures + 1
    shouldTrip = s.phase == HalfOpen || failures' >= s.maxFailures
  in
    if shouldTrip then
      s
        { phase = Open
        , failures = failures'
        , openedAt = Just nowMs
        }
    else
      s { failures = failures' }

admit
  :: Number
  -> State
  -> { state :: State, value :: Boolean }
admit nowMs s0 =
  let
    s = expirePhase nowMs s0
  in
    { state: s, value: shouldProceed s }

-- | Wrap an action with the breaker. The breaker may fail-fast
-- | with a typed `circuitOpen :: Unit` error when it is in
-- | `Open`. Otherwise the action runs; if it raises a typed
-- | error, the breaker records the failure (possibly tripping)
-- | and rethrows the original error so callers can still see
-- | what happened.
-- |
-- | The action's error row is extended with `circuitOpen :: Unit`;
-- | the success type is unchanged.
withBreaker
  :: forall r e a
   . CircuitBreaker
  -> RIO r (circuitOpen :: Unit | e) a
  -> RIO r (circuitOpen :: Unit | e) a
withBreaker (CircuitBreaker ref) action = do
  Milliseconds nowMs <- currentEpoch
  proceed <- liftEffect $ Ref.modify' (admit nowMs) ref
  if not proceed then
    fail (Variant.inj (Proxy :: Proxy "circuitOpen") unit)
  else
    catchAll
      ( \v -> do
          Milliseconds errAtMs <- currentEpoch
          liftEffect $ Ref.modify_ (onFailure errAtMs) ref
          rethrow v
      )
      ( do
          a <- action
          liftEffect $ Ref.modify_ onSuccess ref
          pure a
      )

-- | A non-raising variant: on `Open`, return `Nothing` instead of
-- | failing with `circuitOpen`. The action's error row is
-- | unchanged.
tryWithBreaker
  :: forall r e a
   . CircuitBreaker
  -> RIO r e a
  -> RIO r e (Maybe a)
tryWithBreaker (CircuitBreaker ref) action = do
  Milliseconds nowMs <- currentEpoch
  proceed <- liftEffect $ Ref.modify' (admit nowMs) ref
  if not proceed then
    pure Nothing
  else
    catchAll
      ( \v -> do
          Milliseconds errAtMs <- currentEpoch
          liftEffect $ Ref.modify_ (onFailure errAtMs) ref
          rethrow v
      )
      ( do
          a <- action
          liftEffect $ Ref.modify_ onSuccess ref
          pure (Just a)
      )
