-- | A single-shot count-down latch.
-- |
-- | A `Latch` starts with a non-negative count. `await` suspends until
-- | the count reaches zero; `countDown` decrements it (and wakes every
-- | waiter on the transition to zero). Once the latch has fired,
-- | further `countDown` calls are no-ops and `await` returns
-- | immediately.
-- |
-- | Use it to fan out N concurrent jobs and wait for all of them to
-- | reach a checkpoint (each job calls `countDown` on completion;
-- | the supervisor blocks on `await`).
module RIO.Aff.Latch
  ( Latch
  , await
  , count
  , countDown
  , isOpen
  , make
  ) where

import Prelude

import Data.Array (filter, snoc)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Concurrency (asyncInterrupt)
import RIO.Aff.Core (RIO)

type State =
  { remaining :: Int
  , waiters :: Array Waiter
  , nextId :: Int
  }

type Waiter = { id :: Int, fire :: Effect Unit }

newtype Latch = Latch (Ref State)

-- | Allocate a latch with the given count. Negative inputs are
-- | clamped to zero, in which case the latch is open from birth.
make :: Int -> Effect Latch
make n = Latch <$> Ref.new { remaining: max 0 n, waiters: [], nextId: 0 }

-- | Suspend until the latch has counted down to zero. Returns
-- | immediately if the latch is already open. Cancelling a waiting
-- | fiber removes its entry from the queue cleanly.
await :: forall r e. Latch -> RIO r e Unit
await (Latch ref) = asyncInterrupt \cb -> do
  st <- Ref.read ref
  if st.remaining <= 0 then do
    cb (Right unit)
    pure (pure unit)
  else do
    let
      id = st.nextId
      waiter = { id, fire: cb (Right unit) }
    Ref.write
      (st { waiters = snoc st.waiters waiter, nextId = id + 1 })
      ref
    pure
      ( Ref.modify_
          (\s -> s { waiters = filter (\w -> w.id /= id) s.waiters })
          ref
      )

-- | Decrement the count by one. If the count reaches zero on this
-- | call, every queued waiter is resumed. Once open, `countDown` is
-- | a no-op.
countDown :: forall r e. Latch -> RIO r e Unit
countDown (Latch ref) = liftEffect do
  st <- Ref.read ref
  if st.remaining <= 0 then pure unit
  else do
    let next = st.remaining - 1
    if next > 0 then
      Ref.write (st { remaining = next }) ref
    else do
      Ref.write (st { remaining = 0, waiters = [] }) ref
      for_ st.waiters \w -> w.fire

-- | Current remaining count. Advisory; reads no lock.
count :: forall r e. Latch -> RIO r e Int
count (Latch ref) = liftEffect (_.remaining <$> Ref.read ref)

-- | True once the count has reached zero. Cheaper than `count`
-- | when callers only need the binary state.
isOpen :: forall r e. Latch -> RIO r e Boolean
isOpen (Latch ref) = liftEffect ((_ <= 0) <<< _.remaining <$> Ref.read ref)
