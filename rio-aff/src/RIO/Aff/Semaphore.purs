-- | A counting semaphore for concurrency limiting.
-- |
-- | A `Semaphore` carries an integer permit count. `withPermit`
-- | acquires one permit before running its body and releases it
-- | afterwards, blocking when no permits are available. The
-- | release is registered through `Effect.Aff.finally`, so it runs
-- | on every termination path (success, typed failure, defect,
-- | fiber interruption).
-- |
-- | This is the non-STM counterpart to `RIO.Aff.STM.TSemaphore`.
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
module RIO.Aff.Semaphore
  ( Semaphore
  , acquireN
  , available
  , make
  , parTraverseN
  , releaseN
  , validateParN
  , withPermit
  , withPermits
  ) where

import Prelude

import Data.Array (filter, snoc, uncons) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), finally, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Concurrency (parTraverse, partitionPar) as Concurrency
import RIO.Aff.Internal (RIO(..), mkRIO, unsafeUnRIO)

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

-- | Acquire `n` permits without releasing them. Blocks until `n`
-- | permits are simultaneously available. `n <= 0` is a no-op.
-- |
-- | The released-by-finalizer pairing that `withPermits` does
-- | automatically is *not* present here: callers are responsible
-- | for ensuring a matching `releaseN` runs (typically by wrapping
-- | the use site in a `bracket`-style helper). Use the
-- | scope-respecting `withPermits` unless you specifically need to
-- | manage permits manually, e.g. for handoff across fibers.
acquireN :: forall r e. Int -> Semaphore -> RIO r e Unit
acquireN n sem
  | n <= 0 = pure unit
  | otherwise = mkRIO \_ -> acquire sem n

-- | Release `n` permits. Any waiters that can be satisfied with
-- | the new total are woken in FIFO order. `n <= 0` is a no-op.
-- | Releasing more permits than were ever acquired is allowed and
-- | simply raises the available count.
releaseN :: forall r e. Int -> Semaphore -> RIO r e Unit
releaseN n sem
  | n <= 0 = pure unit
  | otherwise = liftEffect (release sem n)

-- | Bounded-concurrency parallel traversal that gates progress on
-- | a caller-supplied semaphore rather than allocating a fresh
-- | one. Each worker acquires one permit before running `f` and
-- | releases it whether `f` succeeds, fails, defects, or is
-- | interrupted.
-- |
-- | Use this when several call sites share a single concurrency
-- | budget (e.g. one shared HTTP-client semaphore across multiple
-- | fan-outs). For one-shot bounded fan-out with a private budget,
-- | reach for `RIO.Aff.Concurrency.parTraverseN`.
-- |
-- | Failure semantics match `parTraverse`: the first typed failure
-- | cancels the siblings and surfaces on the parent's row. A
-- | sibling still waiting on a permit when the cancel lands is
-- | removed from the waiter queue cleanly.
parTraverseN
  :: forall r e a b
   . Semaphore
  -> (a -> RIO r e b)
  -> Array a
  -> RIO r e (Array b)
parTraverseN sem f xs =
  Concurrency.parTraverse (\a -> withPermit sem (f a)) xs

-- | Bounded-concurrency error-accumulating traversal. Caps the
-- | number of concurrently running workers by gating each branch
-- | on `sem` and runs every branch to completion (not fail-fast).
-- |
-- | On all-success, yields `Right` of the result array in input
-- | order. If any branch fails with a typed error, every typed
-- | failure observed is collected into the `Left` of a
-- | `NonEmptyArray`, also in input order.
-- |
-- | This is the bounded-concurrency sibling of
-- | `RIO.Aff.Concurrency.validatePar`, combining the
-- | error-accumulation policy with a shared concurrency budget.
-- | Defects still propagate as defects on the underlying `Aff`
-- | channel; only typed failures are accumulated.
-- |
-- | ```purescript
-- | -- validate every form field, at most 4 in flight at once, and
-- | -- report every problem, not just the first
-- | result <- validateParN sem checkField fields
-- | case result of
-- |   Right values -> useAll values
-- |   Left errors -> reportEvery errors
-- | ```
validateParN
  :: forall r e e' a b
   . Semaphore
  -> (a -> RIO r e b)
  -> Array a
  -> RIO r e' (Either (NonEmptyArray (Variant e)) (Array b))
validateParN sem f xs = do
  Tuple errs succs <- Concurrency.partitionPar (\a -> withPermit sem (f a)) xs
  pure case NEA.fromArray errs of
    Nothing -> Right succs
    Just nea -> Left nea

-- | Acquire `n` permits for the dynamic extent of `action`. The
-- | request blocks until `n` permits are simultaneously available.
withPermits
  :: forall r e a
   . Int
  -> Semaphore
  -> RIO r e a
  -> RIO r e a
withPermits n sem action = mkRIO \r -> do
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
