-- | A counting semaphore.
-- |
-- | A `Semaphore` holds a non-negative integer count of permits.
-- | `acquireN` suspends until enough permits are available and then
-- | atomically reserves them; `releaseN` returns permits and wakes
-- | the next waiter in FIFO order (head-of-line: if the front waiter
-- | needs more permits than are available, no later waiter is
-- | satisfied either).
-- |
-- | Use `withPermit` / `withPermits` for the common acquire-then-release
-- | pattern. Both pair release through `bracket`, so a typed failure
-- | or interrupt inside the body still returns the permits.
module RIO.Fiber.Semaphore
  ( Semaphore
  , make
  , acquireN
  , releaseN
  , available
  , withPermit
  , withPermits
  ) where

import Prelude

import Data.Array (filter, snoc, uncons)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, async, bracket, liftEffect)

type State =
  { available :: Int
  , queue :: Array Waiter
  , nextId :: Int
  }

type Waiter =
  { id :: Int
  , n :: Int
  , fire :: Effect Unit
  }

newtype Semaphore = Semaphore (Ref State)

-- | Allocate a semaphore with `n` initial permits. Negative inputs
-- | are clamped to zero.
make :: Int -> Effect Semaphore
make n = Semaphore <$>
  Ref.new { available: max 0 n, queue: [], nextId: 0 }

-- | Current permit count. Useful for diagnostics; the value is
-- | observed without taking a lock so treat it as advisory.
available :: forall r e. Semaphore -> RIO r e Int
available (Semaphore ref) = liftEffect (_.available <$> Ref.read ref)

-- | Acquire `n` permits, blocking until they are available. `n <= 0`
-- | succeeds immediately. Cancelling an awaiting fiber removes its
-- | request from the queue cleanly so it never receives a stale
-- | permit.
acquireN :: forall r e. Int -> Semaphore -> RIO r e Unit
acquireN n (Semaphore ref)
  | n <= 0 = pure unit
  | otherwise = async \cb -> do
      st <- Ref.read ref
      if st.available >= n then do
        Ref.write (st { available = st.available - n }) ref
        cb (Right unit)
        pure (pure unit)
      else do
        let
          id = st.nextId
          waiter = { id, n, fire: cb (Right unit) }
        Ref.write
          (st { queue = snoc st.queue waiter, nextId = id + 1 })
          ref
        pure
          -- canceller: drop the waiter from the queue. By the time a
          -- release would call `fire`, the interpreter has already
          -- cleared this canceller (see Fiber._resumeAsync), so the
          -- two paths never run together.
          ( Ref.modify_
              (\s -> s { queue = filter (\w -> w.id /= id) s.queue })
              ref
          )

-- | Release `n` permits. Any waiters that can be satisfied with the
-- | new total are woken in FIFO order. `n <= 0` is a no-op.
releaseN :: forall r e. Int -> Semaphore -> RIO r e Unit
releaseN n (Semaphore ref)
  | n <= 0 = pure unit
  | otherwise = liftEffect (releaseImpl n ref)

releaseImpl :: Int -> Ref State -> Effect Unit
releaseImpl n ref = do
  st <- Ref.read ref
  let
    drain avail q fires = case uncons q of
      Nothing -> { avail, queue: [], fires }
      Just { head: w, tail }
        | avail >= w.n -> drain (avail - w.n) tail (snoc fires w.fire)
        | otherwise -> { avail, queue: q, fires }
    result = drain (st.available + n) st.queue []
  Ref.write
    (st { available = result.avail, queue = result.queue })
    ref
  for_ result.fires identity

-- | Acquire `n` permits, run the action, and release them whether
-- | the action succeeds, fails, defects, or is interrupted.
withPermits :: forall r e a. Int -> Semaphore -> RIO r e a -> RIO r e a
withPermits n s body =
  bracket (acquireN n s) (\_ -> releaseN n s) (\_ -> body)

-- | `withPermits 1`.
withPermit :: forall r e a. Semaphore -> RIO r e a -> RIO r e a
withPermit = withPermits 1
