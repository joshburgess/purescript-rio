-- | A counting semaphore built on a single `TRef Int`.
-- |
-- | `acquireN` retries when fewer permits than requested are
-- | available; `releaseN` adds permits back and wakes any waiting
-- | acquirers. `withTSemaphore` brackets a permit acquisition
-- | against an `RIO` action so the permit is released on every
-- | termination path (success, typed failure, defect, kill).
module RIO.STM.TSemaphore
  ( TSemaphore
  , acquireN
  , acquireTSemaphore
  , availableTSemaphore
  , newTSemaphore
  , releaseN
  , releaseTSemaphore
  , withTSemaphore
  ) where

import Prelude

import RIO.Core (RIO, acquireRelease)
import RIO.STM (STM, TRef, atomically, check, modifyTRef, newTRef, readTRef, writeTRef)

-- | A counting semaphore. Constructor hidden; identity is the
-- | underlying `TRef`.
newtype TSemaphore = TSemaphore (TRef Int)

-- | Allocate a fresh semaphore with `n` permits initially
-- | available. `n` may be zero; negative initial counts are not
-- | guarded against and produce a semaphore that no `acquire` can
-- | ever satisfy.
newTSemaphore :: forall e. Int -> STM e TSemaphore
newTSemaphore n = TSemaphore <$> newTRef n

-- | Acquire one permit. Retries (waits) when no permits are
-- | available.
acquireTSemaphore :: forall e. TSemaphore -> STM e Unit
acquireTSemaphore = acquireN 1

-- | Acquire `n` permits at once. Retries until at least `n` are
-- | available, then deducts them atomically.
acquireN :: forall e. Int -> TSemaphore -> STM e Unit
acquireN n (TSemaphore ref) = do
  available <- readTRef ref
  check (available >= n)
  writeTRef ref (available - n)

-- | Release one permit.
releaseTSemaphore :: forall e. TSemaphore -> STM e Unit
releaseTSemaphore = releaseN 1

-- | Release `n` permits.
releaseN :: forall e. Int -> TSemaphore -> STM e Unit
releaseN n (TSemaphore ref) = modifyTRef ref (_ + n)

-- | Current number of available permits.
availableTSemaphore :: forall e. TSemaphore -> STM e Int
availableTSemaphore (TSemaphore ref) = readTRef ref

-- | Acquire one permit, run `action`, release the permit on every
-- | termination path. Equivalent to `acquireRelease` over a single
-- | atomic acquire/release pair.
withTSemaphore :: forall r e a. TSemaphore -> RIO r e a -> RIO r e a
withTSemaphore sem action =
  acquireRelease
    (atomically (acquireTSemaphore sem))
    (\_ -> atomically (releaseTSemaphore sem))
    (\_ -> action)
