-- | Fork-based concurrency for `RIO`.
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
  , forkScoped
  , interrupt
  , join
  , parSequence
  , parTraverse
  , parTraverseN
  , race
  , raceAll
  , timeout
  , uninterruptible
  , zipPar
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Parallel (parOneOfMap, parTraverse) as Parallel
import Data.Array (concat, drop, length, take) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect.Aff (Fiber, attempt, delay, error, forkAff, invincible, joinFiber, killFiber, parallel, sequential) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Internal (RIO(..), unRIO)
import RIO.Resource (Scope, addFinalizer)

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
-- | A fiber forked with plain `fork` has unbounded lifetime: it
-- | outlives its parent unless explicitly joined or interrupted.
-- | For scope-bounded lifetimes, prefer `forkScoped`.
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

-- | Fork an `RIO` computation into a new fiber whose lifetime is
-- | bounded by the given `Scope`. When the scope exits (success,
-- | typed failure, defect, or kill), an `interrupt` is sent to the
-- | fiber as part of the scope's LIFO finalizer pass.
-- |
-- | This is the structured-concurrency counterpart of `fork`: the
-- | child cannot outlive its enclosing scope, so a "supervising"
-- | parent can never accidentally leak a runaway worker.
-- |
-- | ```purescript
-- | scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   worker <- forkScoped scope (poll endpoint)
-- |   -- ... use the worker; when this block exits, the worker is
-- |   -- interrupted automatically before scoped returns.
-- |   pure unit
-- | ```
forkScoped :: forall r e e' a. Scope -> RIO r e a -> RIO r e' (Fiber e a)
forkScoped scope inner = RIO \r -> do
  fib <- Aff.forkAff (unRIO inner r)
  let cleanup = Aff.killFiber (Aff.error "RIO.forkScoped: scope exit") fib
  _ <- unRIO (addFinalizer scope cleanup) r
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

-- | Run an `RIO` action in an uninterruptible region. While the
-- | inner action is running, any `interrupt` sent to the enclosing
-- | fiber is queued; it fires only after the region completes.
-- |
-- | Use this around critical sections that must not be torn down
-- | mid-flight: a sequence of `Ref` mutations that has to commit
-- | atomically, a multi-step release whose intermediate state would
-- | be observable to other fibers, or a handover where the act of
-- | recording "we have the resource" must not race the kill.
-- |
-- | `acquireRelease` already runs its release phase uninterruptibly;
-- | reach for `uninterruptible` only when the action itself (not its
-- | finalizer) is what must not be killed.
-- |
-- | ```purescript
-- | -- both ref mutations either both happen or neither does;
-- | -- no kill can land between them
-- | uninterruptible do
-- |   liftEffect (Ref.write True commitFlag)
-- |   liftEffect (Ref.modify_ (_ + 1) commitCounter)
-- | ```
uninterruptible :: forall r e a. RIO r e a -> RIO r e a
uninterruptible inner = RIO \r -> Aff.invincible (unRIO inner r)

-- | A short-circuiting error used inside `parTraverse` to abort
-- | sibling fibers as soon as one branch fails. The first failure is
-- | captured into a shared `Ref`; throwing this sentinel from the
-- | branch fires the `ParAff` alternative semantics that interrupt
-- | the rest. The outer `attempt` swallows this sentinel and reads
-- | the ref to recover the typed failure for the parent.
shortCircuitMessage :: String
shortCircuitMessage = "RIO.parTraverse: short-circuit"

-- | Run every action in an array in parallel and collect the
-- | results. Built on `Effect.Aff`'s `ParAff` applicative, so two
-- | 100ms actions complete in ~100ms rather than ~200ms.
-- |
-- | Failure semantics: the first typed failure cancels every
-- | sibling fiber and surfaces as `Left v` on the parent's row.
-- | If multiple branches fail concurrently the one whose failure
-- | is observed first wins; the rest are interrupted before they
-- | complete. This matches ZIO `foreachPar` and Effect-TS `forEach`
-- | with `concurrency: "unbounded"`.
-- |
-- | Defects from any branch propagate as `Aff` defects (observable
-- | via `RIO.Error.sandbox`); they also interrupt the siblings.
-- |
-- | ```purescript
-- | -- fetch every URL concurrently and collect the bodies; one
-- | -- failure cancels the rest
-- | bodies :: Array URL -> RIO r e (Array Body)
-- | bodies urls = parTraverse fetch urls
-- | ```
parTraverse
  :: forall r e a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e (Array b)
parTraverse f as = RIO \r -> do
  failureRef <- liftEffect (Ref.new Nothing)
  let
    run a = do
      res <- unRIO (f a) r
      case res of
        Right b -> pure b
        Left v -> do
          liftEffect
            ( Ref.modify_
                ( case _ of
                    Nothing -> Just v
                    existing -> existing
                )
                failureRef
            )
          throwError (Aff.error shortCircuitMessage)
  outcome <- Aff.attempt (Parallel.parTraverse run as)
  case outcome of
    Right values -> pure (Right values)
    Left err -> do
      first <- liftEffect (Ref.read failureRef)
      case first of
        Just v -> pure (Left v)
        Nothing -> throwError err

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

-- | Bounded-concurrency parallel traversal. At most `n` actions
-- | run concurrently; the input array is split into chunks of size
-- | `n` and each chunk is `parTraverse`d in turn.
-- |
-- | `n <= 0` is treated as `1` (sequential). Short-circuit semantics
-- | match `parTraverse`: the first typed failure inside a chunk
-- | cancels its siblings and aborts the remaining chunks.
-- |
-- | Use this when each action is heavy enough (memory, file
-- | handles, outbound connections) that you want to cap how many
-- | run at once, rather than letting the unbounded `parTraverse`
-- | spawn one fiber per input.
-- |
-- | ```purescript
-- | -- fetch a list of URLs, at most 8 in flight at once
-- | bodies :: Array URL -> RIO r e (Array Body)
-- | bodies urls = parTraverseN 8 fetch urls
-- | ```
parTraverseN
  :: forall r e a b
   . Int
  -> (a -> RIO r e b)
  -> Array a
  -> RIO r e (Array b)
parTraverseN n f as =
  let
    size = if n <= 1 then 1 else n
    chunks = chunksOf size as
  in
    Array.concat <$> traverse (parTraverse f) chunks

-- | Split an array into chunks of (up to) `n` elements each.
-- | Assumes `n >= 1`.
chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n as
  | Array.length as == 0 = []
  | otherwise = [ Array.take n as ] <> chunksOf n (Array.drop n as)

-- | Run two actions concurrently and pair their results.
-- |
-- | Same failure semantics as `parTraverse`: the first `Left`
-- | cancels the other action and is surfaced on the parent's row.
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
zipPar ra rb = RIO \r -> do
  failureRef <- liftEffect (Ref.new Nothing)
  let
    runA = do
      res <- unRIO ra r
      case res of
        Right a -> pure a
        Left v -> do
          liftEffect (Ref.modify_ (case _ of
            Nothing -> Just v
            existing -> existing) failureRef)
          throwError (Aff.error shortCircuitMessage)
    runB = do
      res <- unRIO rb r
      case res of
        Right b -> pure b
        Left v -> do
          liftEffect (Ref.modify_ (case _ of
            Nothing -> Just v
            existing -> existing) failureRef)
          throwError (Aff.error shortCircuitMessage)
  outcome <- Aff.attempt
    ( Aff.sequential (Tuple <$> Aff.parallel runA <*> Aff.parallel runB)
    )
  case outcome of
    Right t -> pure (Right t)
    Left err -> do
      first <- liftEffect (Ref.read failureRef)
      case first of
        Just v -> pure (Left v)
        Nothing -> throwError err

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

-- | Race a non-empty array of actions with true N-way concurrency
-- | (built on `Control.Parallel.parOneOfMap`). First to complete
-- | wins; the others are interrupted and their resources released.
-- |
-- | The non-empty input is what makes the result type total: with
-- | an empty array there would be nothing to return.
-- |
-- | ```purescript
-- | -- fastest of three replicas wins
-- | record <- raceAll (NEArray.cons' (fetch r1) [ fetch r2, fetch r3 ])
-- | ```
raceAll
  :: forall r e a
   . NonEmptyArray (RIO r e a)
  -> RIO r e a
raceAll arr = RIO \r ->
  Parallel.parOneOfMap (\rio -> unRIO rio r) arr

-- | Run an action with a deadline. If `action` completes within
-- | `ms`, the result is `Just a`; if the deadline fires first, the
-- | action is interrupted and `Nothing` is returned.
-- |
-- | Typed failures from the action surface unchanged on the parent's
-- | row (the timeout never converts an error into a `Nothing`). If
-- | the action holds resources via `acquireRelease` or `Scope`,
-- | they're released as part of the interrupt (Phase 0.5 S3).
-- |
-- | ```purescript
-- | -- treat anything slower than 500ms as a miss
-- | result <- timeout (Milliseconds 500.0) (fetchFromCache key)
-- | case result of
-- |   Just hit -> pure hit
-- |   Nothing -> fetchFromSource key
-- | ```
timeout
  :: forall r e a
   . Milliseconds
  -> RIO r e a
  -> RIO r e (Maybe a)
timeout ms action =
  race
    (map Just action)
    (RIO \_ -> Aff.delay ms *> pure (Right Nothing))
