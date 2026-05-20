-- | A token-bucket rate limiter.
-- |
-- | A `RateLimiter` is a value (a handle on a refilling token
-- | bucket), so the public surface lifts every operation into
-- | `RIO`. Internally a single `Ref` stores the bucket's current
-- | token count and the timestamp of its last refill; reads of
-- | the clock happen through `RIO.Aff.Clock` so the limiter is fully
-- | driven by `RIO.Aff.Test.Clock` in tests.
-- |
-- | `acquire` (and `acquireN`) deduct tokens from the bucket if
-- | enough are available, otherwise compute the wait until
-- | enough have refilled and `sleep` for that long before
-- | retrying. `tryAcquire` returns immediately with a `Boolean`
-- | indicating whether the deduction succeeded. `withPermit`
-- | brackets an action with an acquire (and registers no release
-- | because tokens are spent, not borrowed).
-- |
-- | ## Fairness
-- |
-- | The implementation does not maintain a queue of waiters:
-- | each acquirer independently samples the bucket, sleeps, and
-- | retries. Under contention you should expect approximate
-- | fairness rather than FIFO ordering. If you need strict FIFO
-- | wakeups, layer a `Semaphore` (for the queue) on top of this
-- | (for the token math).
-- |
-- | ```purescript
-- | callApi :: forall r e. RateLimiter -> Request -> RIO (clock :: Clock | r) e Response
-- | callApi rl req =
-- |   RateLimiter.withPermit rl (doCall req)
-- | ```
module RIO.Aff.RateLimiter
  ( RateLimiter
  , Config
  , available
  , make
  , acquire
  , acquireN
  , tryAcquire
  , tryAcquireN
  , withPermit
  , withPermits
  ) where

import Prelude

import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Clock (Clock, now, sleep)
import RIO.Aff.Core (RIO)

-- | Configuration for a fresh rate limiter.
-- |
-- |   * `permitsPerSecond` is the bucket's refill rate.
-- |   * `burst` is the maximum number of tokens the bucket can
-- |     hold; transient bursts up to this size are served
-- |     without delay.
type Config =
  { permitsPerSecond :: Number
  , burst :: Int
  }

type State =
  { tokens :: Number
  , lastRefillMs :: Number
  , refillPerMs :: Number
  , capacity :: Number
  }

-- | A token-bucket rate limiter. Allocate with `make`; use
-- | through `acquire` / `tryAcquire` / `withPermit`.
newtype RateLimiter = RateLimiter (Ref State)

-- | The current (approximate) token count. The value can change
-- | between the read and any subsequent action; treat it as
-- | advisory.
available :: forall r e. RateLimiter -> RIO r e Number
available (RateLimiter ref) =
  liftEffect (_.tokens <$> Ref.read ref)

-- | Allocate a fresh rate limiter. The bucket starts full: the
-- | very first call to `acquire` (up to `burst`) succeeds
-- | without sleeping.
make :: forall r e. Config -> RIO (clock :: Clock | r) e RateLimiter
make cfg = do
  Milliseconds nowMs <- now
  let
    capacity = toNumber (max 1 cfg.burst)
    refillPerMs = cfg.permitsPerSecond / 1000.0
  liftEffect $ map RateLimiter $ Ref.new
    { tokens: capacity
    , lastRefillMs: nowMs
    , refillPerMs
    , capacity
    }

refill :: Number -> State -> State
refill nowMs s =
  let
    elapsed = nowMs - s.lastRefillMs
    earned = elapsed * s.refillPerMs
    tokens' = min s.capacity (s.tokens + earned)
  in
    s { tokens = tokens', lastRefillMs = nowMs }

attemptDeduct
  :: Number
  -> Number
  -> State
  -> { state :: State, value :: Boolean }
attemptDeduct nowMs needed s0 =
  let
    s = refill nowMs s0
  in
    if s.tokens >= needed then
      { state: s { tokens = s.tokens - needed }, value: true }
    else
      { state: s, value: false }

attemptDeductOrWait
  :: Number
  -> Number
  -> State
  -> { state :: State, value :: Number }
attemptDeductOrWait nowMs needed s0 =
  let
    s = refill nowMs s0
  in
    if s.tokens >= needed then
      { state: s { tokens = s.tokens - needed }, value: 0.0 }
    else
      let
        missing = needed - s.tokens
        wait = missing / s.refillPerMs
      in
        { state: s, value: wait }

-- | Acquire one permit, blocking until the bucket has a token.
acquire :: forall r e. RateLimiter -> RIO (clock :: Clock | r) e Unit
acquire = acquireN 1

-- | Acquire `n` permits, blocking until the bucket has accrued
-- | at least `n` tokens. If `n` exceeds the bucket's capacity,
-- | the call will never succeed; callers should keep `n` within
-- | the configured `burst`.
acquireN
  :: forall r e
   . Int
  -> RateLimiter
  -> RIO (clock :: Clock | r) e Unit
acquireN n (RateLimiter ref) =
  let
    needed = toNumber (max 1 n)
    go = do
      Milliseconds nowMs <- now
      waitMs <- liftEffect $
        Ref.modify' (attemptDeductOrWait nowMs needed) ref
      if waitMs <= 0.0 then pure unit
      else do
        sleep (Milliseconds waitMs)
        go
  in
    go
  where
  _ = Proxy :: Proxy "clock"

-- | Try to acquire one permit without sleeping. Returns `true`
-- | if a token was available and was deducted.
tryAcquire :: forall r e. RateLimiter -> RIO (clock :: Clock | r) e Boolean
tryAcquire = tryAcquireN 1

-- | Try to acquire `n` permits without sleeping.
tryAcquireN
  :: forall r e
   . Int
  -> RateLimiter
  -> RIO (clock :: Clock | r) e Boolean
tryAcquireN n (RateLimiter ref) = do
  Milliseconds nowMs <- now
  liftEffect $ Ref.modify' (attemptDeduct nowMs (toNumber n)) ref

-- | Run `action` after acquiring one permit. The permit is
-- | spent, not borrowed: there is no matching release.
withPermit
  :: forall r e a
   . RateLimiter
  -> RIO (clock :: Clock | r) e a
  -> RIO (clock :: Clock | r) e a
withPermit = withPermits 1

-- | Run `action` after acquiring `n` permits.
withPermits
  :: forall r e a
   . Int
  -> RateLimiter
  -> RIO (clock :: Clock | r) e a
  -> RIO (clock :: Clock | r) e a
withPermits n rl action = acquireN n rl *> action
