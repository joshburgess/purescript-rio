-- | A counting semaphore for concurrency limiting.
-- |
-- | A `Semaphore` carries an integer permit count. `withPermit`
-- | acquires one permit before running its body and releases it
-- | afterwards, blocking when no permits are available. The
-- | release is registered through `Effect.Aff.finally`, so it runs
-- | on every termination path (success, typed failure, defect,
-- | fiber interruption).
-- |
-- | This is the non-STM counterpart to `RIO.STM.TSemaphore`.
-- | Reach for this one when you just want concurrency limiting;
-- | reach for the STM version when the acquire needs to compose
-- | with other transactional operations.
-- |
-- | ```purescript
-- | concurrentBatch :: forall r e a.
-- |   Semaphore -> Array (RIO r e a) -> RIO r e (Array a)
-- | concurrentBatch sem actions =
-- |   parTraverse (\a -> withPermit sem a) actions
-- | ```
module RIO.Semaphore
  ( Semaphore
  , available
  , make
  , withPermit
  , withPermits
  ) where

import Prelude

import Data.Array (filter, snoc, uncons) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), finally, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Internal (RIO(..), unsafeUnRIO)

-- | A pending acquirer.
type Waiter =
  { tag :: Int
  , permits :: Int
  , resume :: Effect Unit
  }

type State =
  { available :: Int
  , waiters :: Array Waiter
  , nextTag :: Int
  }

-- | A counting semaphore. Construct with `make`; use through
-- | `withPermit` / `withPermits`. The constructor is hidden.
newtype Semaphore = Semaphore (Ref State)

-- | Allocate a fresh semaphore with `n` permits available.
make :: Int -> Effect Semaphore
make n = do
  ref <- Ref.new { available: max 0 n, waiters: [], nextTag: 0 }
  pure (Semaphore ref)

-- | Read the current permit count. The value can change between
-- | the read and any subsequent action; treat it as advisory.
available :: Semaphore -> Effect Int
available (Semaphore ref) = _.available <$> Ref.read ref

-- | Acquire one permit for the dynamic extent of `action`.
withPermit :: forall r e a. Semaphore -> RIO r e a -> RIO r e a
withPermit = withPermits 1

-- | Acquire `n` permits for the dynamic extent of `action`. The
-- | request blocks until `n` permits are simultaneously available.
withPermits
  :: forall r e a
   . Int
  -> Semaphore
  -> RIO r e a
  -> RIO r e a
withPermits n sem action = RIO \r -> do
  acquire sem n
  finally
    (liftEffect (release sem n))
    (unsafeUnRIO action r)

-- The acquire is `Aff`-valued because it may block. Implemented
-- with `makeAff` so a fiber interrupted while waiting is removed
-- from the waiter list cleanly.
acquire :: Semaphore -> Int -> Aff Unit
acquire (Semaphore ref) n = makeAff \resume -> do
  state <- Ref.read ref
  if state.available >= n then do
    Ref.write (state { available = state.available - n }) ref
    resume (Right unit)
    pure nonCanceler
  else do
    let tag = state.nextTag
    let
      waiter =
        { tag
        , permits: n
        , resume: resume (Right unit)
        }
    Ref.write
      ( state
          { nextTag = state.nextTag + 1
          , waiters = Array.snoc state.waiters waiter
          }
      )
      ref
    pure
      ( Canceler \_ -> liftEffect do
          s2 <- Ref.read ref
          Ref.write
            (s2 { waiters = Array.filter (\w -> w.tag /= tag) s2.waiters })
            ref
      )

-- Returning permits. If a waiter at the head of the queue can be
-- satisfied with the now-available count, wake it. Repeat as long
-- as the head fits.
release :: Semaphore -> Int -> Effect Unit
release (Semaphore ref) n = do
  s <- Ref.read ref
  Ref.write (s { available = s.available + n }) ref
  drain ref

drain :: Ref State -> Effect Unit
drain ref = do
  s <- Ref.read ref
  case Array.uncons s.waiters of
    Nothing -> pure unit
    Just { head, tail } ->
      if s.available >= head.permits then do
        Ref.write
          ( s
              { available = s.available - head.permits
              , waiters = tail
              }
          )
          ref
        head.resume
        drain ref
      else
        pure unit
