-- | Fork-based concurrency for `RIO`.
-- |
-- | Phase 6.1 covers `Fiber`, `fork`, `join`, and `interrupt`. Later
-- | phases will add parallel combinators (`parTraverse`, `zipPar`),
-- | racing, and the concurrency doc.
-- |
-- | `Fiber e a` wraps an underlying `Effect.Aff.Fiber` that carries
-- | the `Either (Variant e) a` shape `unRIO` produces. Typed failures
-- | from inside the fiber surface on `join` as `Left v`; defects
-- | (uncaught `Aff` exceptions, including the interrupt exception
-- | from `interrupt`) propagate through `Aff` and reach the joiner
-- | as `Aff` defects, observable via `RIO.Error.sandbox`.
-- |
-- | All of this is a thin wrapper over `Effect.Aff.forkAff` /
-- | `killFiber` / `joinFiber`; the underlying cancellation
-- | guarantees come from the Phase 0.5 spike (scenarios S1, S3).
module RIO.Concurrency
  ( Fiber
  , fork
  , interrupt
  , join
  , parSequence
  , parTraverse
  , race
  , raceAll
  , zipPar
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Parallel (parTraverse) as Parallel
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Foldable (foldl)
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect.Aff (Fiber, error, forkAff, joinFiber, killFiber, parallel, sequential) as Aff

import RIO.Internal (RIO(..), unRIO)

-- | An in-flight `RIO` computation forked into its own fiber.
-- |
-- | The wrapped `Aff.Fiber` produces the same `Either (Variant e) a`
-- | shape `unRIO` does, so a typed failure inside the forked
-- | computation surfaces on `join` as `Left v` and a defect surfaces
-- | as an `Aff` exception during the join.
newtype Fiber :: Row Type -> Type -> Type
newtype Fiber e a = Fiber (Aff.Fiber (Either (Variant e) a))

-- | Fork an `RIO` computation into a new fiber.
-- |
-- | The parent's view of the fork is infallible: the resulting
-- | `RIO r e' (Fiber e a)` is polymorphic in the parent's error row
-- | `e'` because it never produces a typed failure. The child's
-- | typed errors live inside the returned `Fiber` and are surfaced
-- | only by a subsequent `join`. The child runs in the same
-- | environment record as the parent.
-- |
-- | The error row on the parent side is left free (rather than
-- | pinned to `()`) so `fork` composes cleanly inside a do-block
-- | whose surrounding row is non-empty.
-- |
-- | ```purescript
-- | program = do
-- |   fib <- fork (longRunning 42)
-- |   doSomeOtherWork
-- |   result <- join fib
-- |   pure result
-- | ```
fork :: forall r e e' a. RIO r e a -> RIO r e' (Fiber e a)
fork inner = RIO \r -> do
  fib <- Aff.forkAff (unRIO inner r)
  pure (Right (Fiber fib))

-- | Wait for a forked fiber to finish and surface its result.
-- |
-- | A typed failure from inside the fiber is returned as `Left v` in
-- | the joiner's `e`. A defect (uncaught `Aff` exception, including
-- | the interrupt exception raised by `interrupt`) propagates through
-- | `Aff` and is observable at the joiner only via
-- | `RIO.Error.sandbox`. Joining a fiber that has already completed
-- | returns its cached result; joining a fiber more than once is
-- | safe.
-- |
-- | ```purescript
-- | -- start two workers in parallel and join them later
-- | program = do
-- |   a <- fork worker1
-- |   b <- fork worker2
-- |   ra <- join a
-- |   rb <- join b
-- |   pure (ra + rb)
-- | ```
join :: forall r e a. Fiber e a -> RIO r e a
join (Fiber fib) = RIO \_ -> Aff.joinFiber fib

-- | Interrupt a running fiber.
-- |
-- | Sends a kill exception to the fiber. Pending `Aff.delay`s and
-- | other awaiting points abort promptly; a tight synchronous loop
-- | with no async boundary will not be preempted until it yields
-- | (see the Phase 0.5 spike's S2 / S2b for the canonical
-- | cooperative-cancellation caveat).
-- |
-- | Resources held by the fiber via `RIO.Resource.acquireRelease`
-- | or `Scope` are released; the release path runs uninterruptibly
-- | (Phase 0.5 scenario S3).
-- |
-- | This is infallible from the caller's perspective. Killing an
-- | already-completed or already-killed fiber is a no-op. The
-- | error row on the caller side is left free (rather than pinned
-- | to `()`) for the same reason as `fork`.
-- |
-- | ```purescript
-- | -- supervise a worker; abort it after a timeout
-- | program = do
-- |   fib <- fork worker
-- |   liftAff (delay (Milliseconds 1000.0))
-- |   interrupt fib
-- | ```
interrupt :: forall r e e' a. Fiber e a -> RIO r e' Unit
interrupt (Fiber fib) = RIO \_ -> do
  Aff.killFiber (Aff.error "RIO.interrupt") fib
  pure (Right unit)

-- | Run every action in an array in parallel and collect the
-- | results. Built on `Effect.Aff`'s `ParAff` applicative, so two
-- | 100ms actions complete in ~100ms rather than ~200ms.
-- |
-- | Failure semantics: all actions run to completion before this
-- | returns. If any returned `Left v`, the first such failure (in
-- | array order) is surfaced as `Left v` on the parent's row. The
-- | other actions are not interrupted on failure; first-failure
-- | racing semantics live in `RIO.Concurrency.race` (Phase 6.3).
-- |
-- | ```purescript
-- | -- fetch every URL concurrently and collect the bodies
-- | bodies :: Array URL -> RIO r e (Array Body)
-- | bodies urls = parTraverse fetch urls
-- | ```
parTraverse
  :: forall r e a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e (Array b)
parTraverse f as = RIO \r -> do
  rows <- Parallel.parTraverse (\a -> unRIO (f a) r) as
  pure (sequence rows)

-- | The identity case of `parTraverse`: run an array of actions
-- | concurrently and collect their results.
-- |
-- | ```purescript
-- | results <- parSequence [ jobA, jobB, jobC ]
-- | ```
parSequence
  :: forall r e a
   . Array (RIO r e a)
  -> RIO r e (Array a)
parSequence = parTraverse identity

-- | Run two actions concurrently and pair their results.
-- |
-- | Same failure semantics as `parTraverse`: both actions run to
-- | completion, and the first `Left` (favouring the left action on
-- | ties) is surfaced on the parent's row. For first-failure racing
-- | semantics use `race` (Phase 6.3).
-- |
-- | ```purescript
-- | -- fetch user record and audit log in parallel
-- | Tuple user audit <- zipPar (fetchUser uid) (fetchAudit uid)
-- | ```
zipPar
  :: forall r e a b
   . RIO r e a
  -> RIO r e b
  -> RIO r e (Tuple a b)
zipPar ra rb = RIO \r ->
  let
    pairAff =
      Aff.sequential
        (Tuple <$> Aff.parallel (unRIO ra r) <*> Aff.parallel (unRIO rb r))
  in
    do
      Tuple ea eb <- pairAff
      pure case ea, eb of
        Left v, _ -> Left v
        _, Left v -> Left v
        Right a, Right b -> Right (Tuple a b)

-- | Race two actions: the first to complete wins, regardless of
-- | whether it succeeds or fails with a typed error. The loser is
-- | interrupted by the underlying `Aff` runtime, and any resources
-- | it holds via `acquireRelease` or `Scope` are released (Phase
-- | 0.5 scenario S3).
-- |
-- | Defects propagate from whichever side raises them; the loser
-- | is interrupted as usual.
-- |
-- | ```purescript
-- | -- whichever cache responds first wins; the loser is cancelled
-- | record <- race (fromCacheA key) (fromCacheB key)
-- | ```
race
  :: forall r e a
   . RIO r e a
  -> RIO r e a
  -> RIO r e a
race ra rb = RIO \r ->
  Aff.sequential
    (Aff.parallel (unRIO ra r) <|> Aff.parallel (unRIO rb r))

-- | Race a non-empty array of actions. First to complete wins; the
-- | others are interrupted and their resources released. The
-- | non-empty input is what makes the result type total: with an
-- | empty array there would be nothing to return.
-- |
-- | ```purescript
-- | -- fastest of three replicas wins
-- | record <- raceAll (NEArray.cons' (fetch r1) [ fetch r2, fetch r3 ])
-- | ```
raceAll
  :: forall r e a
   . NonEmptyArray (RIO r e a)
  -> RIO r e a
raceAll arr =
  let
    { head, tail } = NEArray.uncons arr
  in
    foldl race head tail
