-- | A counting semaphore that lives inside an STM transaction.
-- |
-- | Unlike `RIO.Fiber.Semaphore`, which works at the `RIO` level
-- | (acquire is a fiber-suspending action), `TSemaphore` exposes
-- | its acquire / release through `STM`. That lets a single
-- | transaction atomically acquire permits and observe other STM
-- | state in one step: if the transaction retries, no permits
-- | were "spent". Use this when "available permits + some other
-- | atomic state" needs to be observed as a unit.
module RIO.Fiber.STM.TSemaphore
  ( TSemaphore
  , make
  , available
  , acquire
  , acquireN
  , release
  , releaseN
  , tryAcquire
  , tryAcquireN
  ) where

import Prelude

import Effect (Effect)
import RIO.Fiber.STM (STM, TVar)
import RIO.Fiber.STM as STM

-- | A counting semaphore backed by a single `TVar Int`. The held
-- | count is the number of permits currently available; transactions
-- | acquire by decrementing and release by incrementing.
newtype TSemaphore = TSemaphore (TVar Int)

-- | A fresh semaphore with `n` permits. Negative counts are
-- | clamped to zero.
make :: Int -> Effect TSemaphore
make n = TSemaphore <$> STM.newTVar (max 0 n)

-- | How many permits are available right now?
available :: TSemaphore -> STM Int
available (TSemaphore tv) = STM.readTVar tv

-- | Acquire one permit. Retries when the semaphore is empty.
acquire :: TSemaphore -> STM Unit
acquire = acquireN 1

-- | Acquire `n` permits atomically. Retries until at least `n`
-- | permits are available. Counts <= 0 are no-ops.
acquireN :: Int -> TSemaphore -> STM Unit
acquireN n (TSemaphore tv)
  | n <= 0 = pure unit
  | otherwise = do
      have <- STM.readTVar tv
      if have < n then STM.retry
      else STM.writeTVar tv (have - n)

-- | Release one permit.
release :: TSemaphore -> STM Unit
release = releaseN 1

-- | Release `n` permits. Counts <= 0 are no-ops.
releaseN :: Int -> TSemaphore -> STM Unit
releaseN n (TSemaphore tv)
  | n <= 0 = pure unit
  | otherwise = STM.modifyTVar tv (_ + n)

-- | Try to acquire one permit without retrying. Returns `true`
-- | if the permit was taken.
tryAcquire :: TSemaphore -> STM Boolean
tryAcquire = tryAcquireN 1

-- | Try to acquire `n` permits atomically. Returns `true` only if
-- | every permit was taken; otherwise leaves the semaphore unchanged
-- | and returns `false`. Counts <= 0 always return `true`.
tryAcquireN :: Int -> TSemaphore -> STM Boolean
tryAcquireN n (TSemaphore tv)
  | n <= 0 = pure true
  | otherwise = do
      have <- STM.readTVar tv
      if have < n then pure false
      else do
        STM.writeTVar tv (have - n)
        pure true
