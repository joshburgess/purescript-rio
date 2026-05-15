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
  , async
  , asyncInterrupt
  , filterPar
  , forever
  , fork
  , forkScoped
  , interrupt
  , join
  , never
  , parSequence
  , parTraverse
  , parTraverseN
  , partition
  , partitionPar
  , race
  , raceAll
  , raceEither
  , timeout
  , timeoutFail
  , uninterruptible
  , validate
  , validatePar
  , zipPar
  , zipWithPar
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Parallel (parOneOfMap, parTraverse) as Parallel
import Data.Array (concat, drop, length, take) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Canceler(..), Fiber, attempt, delay, error, forkAff, invincible, joinFiber, killFiber, makeAff, never, nonCanceler, parallel, sequential) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy)

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

-- | Run every action in an array concurrently and accumulate
-- | typed failures instead of short-circuiting. Every branch runs
-- | to completion: if all succeed the result is `Right` with every
-- | value in input order; otherwise it is `Left` with every typed
-- | failure observed, also in input order.
-- |
-- | The parent's error row `e'` is left free because `validatePar`
-- | itself never raises a typed failure on the parent: all branch
-- | errors are reflected into the result's `Either`.
-- |
-- | Defects (uncaught `Aff` exceptions) still propagate as defects,
-- | matching the rest of the module. This is the "accumulate all
-- | validation errors" counterpart of `parTraverse`, equivalent in
-- | spirit to ZIO `validatePar` and Effect-TS `Effect.validateAll`.
-- |
-- | ```purescript
-- | -- validate every form field concurrently; report every problem,
-- | -- not just the first one
-- | result <- validatePar checkField fields
-- | case result of
-- |   Right values -> useAll values
-- |   Left errors -> reportEvery errors
-- | ```
validatePar
  :: forall r e e' a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e' (Either (NonEmptyArray (Variant e)) (Array b))
validatePar f as = RIO \r -> do
  results <- Parallel.parTraverse (\a -> unRIO (f a) r) as
  let Tuple errs succs = partitionEithers results
  case NEA.fromArray errs of
    Nothing -> pure (Right (Right succs))
    Just nea -> pure (Right (Left nea))

-- | Run every action in an array concurrently and split the results
-- | into typed failures and successes, preserving input order within
-- | each side. Unlike `parTraverse`, no branch is cancelled when
-- | another fails: every action runs to completion.
-- |
-- | This is the lower-level partner of `validatePar`: callers that
-- | want to keep partial successes (rather than fail the batch on
-- | any error) reach for this. It is total - it never raises a
-- | typed failure on the parent's row.
-- |
-- | ```purescript
-- | -- process every input, then decide what to do with the failures
-- | Tuple errs okays <- partitionPar handle inputs
-- | report errs
-- | continueWith okays
-- | ```
partitionPar
  :: forall r e e' a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e' (Tuple (Array (Variant e)) (Array b))
partitionPar f as = RIO \r -> do
  results <- Parallel.parTraverse (\a -> unRIO (f a) r) as
  pure (Right (partitionEithers results))

-- | Sequential sibling of `validatePar`: same accumulating-errors
-- | semantics, but actions run one after another in input order
-- | rather than concurrently. Use this when ordering matters (the
-- | first failure's diagnostics depend on side effects from earlier
-- | items) or when parallelism is undesired.
-- |
-- | The error order is deterministic: input order, not finish order.
-- |
-- | ```purescript
-- | -- run migrations in order; report every failure but don't stop
-- | -- at the first one
-- | result <- validate runMigration migrations
-- | ```
validate
  :: forall r e e' a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e' (Either (NonEmptyArray (Variant e)) (Array b))
validate f as = RIO \r -> do
  results <- traverse (\a -> unRIO (f a) r) as
  let Tuple errs succs = partitionEithers results
  case NEA.fromArray errs of
    Nothing -> pure (Right (Right succs))
    Just nea -> pure (Right (Left nea))

-- | Sequential sibling of `partitionPar`. Runs each action in order
-- | and partitions the results into typed failures and successes,
-- | preserving input order within each side.
-- |
-- | ```purescript
-- | -- process each input in order, then route the failures
-- | Tuple errs okays <- partition handle inputs
-- | report errs
-- | continueWith okays
-- | ```
partition
  :: forall r e e' a b
   . (a -> RIO r e b)
  -> Array a
  -> RIO r e' (Tuple (Array (Variant e)) (Array b))
partition f as = RIO \r -> do
  results <- traverse (\a -> unRIO (f a) r) as
  pure (Right (partitionEithers results))

partitionEithers
  :: forall a b
   . Array (Either a b)
  -> Tuple (Array a) (Array b)
partitionEithers = foldr step (Tuple [] [])
  where
  step (Left a) (Tuple ls rs) = Tuple ([ a ] <> ls) rs
  step (Right b) (Tuple ls rs) = Tuple ls ([ b ] <> rs)

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
          liftEffect
            ( Ref.modify_
                ( case _ of
                    Nothing -> Just v
                    existing -> existing
                )
                failureRef
            )
          throwError (Aff.error shortCircuitMessage)
    runB = do
      res <- unRIO rb r
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

-- | Run two actions concurrently and combine their results with a
-- | pure function. Equivalent to `Tuple <$> zipPar` followed by the
-- | combiner, but spares the caller the destructuring boilerplate.
-- |
-- | Same failure semantics as `zipPar` / `parTraverse`: the first
-- | `Left` cancels the other action and surfaces on the parent's row.
-- |
-- | Mirrors ZIO `ZIO.zipWithPar` / Effect-TS `Effect.zipWith` (with
-- | the parallel evaluation strategy).
-- |
-- | ```purescript
-- | -- fetch user record and audit log in parallel, then combine
-- | report :: RIO r e Report
-- | report = zipWithPar mkReport (fetchUser uid) (fetchAudit uid)
-- | ```
zipWithPar
  :: forall r e a b c
   . (a -> b -> c)
  -> RIO r e a
  -> RIO r e b
  -> RIO r e c
zipWithPar f ra rb = map (\(Tuple a b) -> f a b) (zipPar ra rb)

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

-- | Race two actions and preserve which arm won. The first arm
-- | becomes `Left`; the second becomes `Right`. The loser is
-- | interrupted under the usual `race` semantics.
-- |
-- | Useful when the two branches have different result types or when
-- | downstream code needs to know which side fired without inspecting
-- | the value itself. Mirrors ZIO `ZIO.raceEither` / Effect
-- | `Effect.raceEither`.
-- |
-- | ```purescript
-- | -- whichever finishes first; downstream knows which source it was
-- | outcome <- raceEither (fromCache key) (fromRemote key)
-- | case outcome of
-- |   Left hit -> recordCacheHit hit
-- |   Right res -> recordRemoteHit res
-- | ```
raceEither
  :: forall r e a b
   . RIO r e a
  -> RIO r e b
  -> RIO r e (Either a b)
raceEither ra rb = race (map Left ra) (map Right rb)

-- | Repeat an action indefinitely. The return type is polymorphic
-- | because `forever` never produces a value: it loops until the
-- | fiber is interrupted (via `interrupt`, `race`, `timeout`, or
-- | scope exit) or the action raises a typed failure / defect.
-- |
-- | Idiomatic with `forkScoped`: a supervised background worker
-- | that runs for the lifetime of its scope.
-- |
-- | ```purescript
-- | scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   _ <- forkScoped scope (forever pollOnce)
-- |   doForeground
-- | ```
forever :: forall r e a b. RIO r e a -> RIO r e b
forever m = RIO \r ->
  let
    go = do
      res <- unRIO m r
      case res of
        Left v -> pure (Left v)
        Right _ -> go
  in
    go

-- | An action that never completes on its own. The success type is
-- | polymorphic because `never` cannot return a value: it only
-- | exits when the fiber it runs in is interrupted, killed, or
-- | beaten by another participant in a `race` / `timeout`.
-- |
-- | The canonical use is the "wait for something else" half of a
-- | race: `race never something` waits until `something` completes
-- | without imposing a sleep deadline.
never :: forall r e a. RIO r e a
never = RIO \_ -> Aff.never

-- | Build a `RIO` action from a callback-style effect. The callback
-- | (`Either (Variant e) a -> Effect Unit`) is invoked by the user-
-- | supplied register function exactly once: `Right a` for success,
-- | `Left v` for a typed failure. Subsequent invocations are
-- | ignored by the underlying `Aff` machinery.
-- |
-- | This is the bridge primitive for callback-based or event-emitter
-- | APIs (Node.js callbacks, browser APIs, third-party promise
-- | libraries). Mirrors `ZIO.async` and `Effect.async`.
-- |
-- | The action is not cancellable: if the fiber it runs in is
-- | interrupted while waiting for the callback, the underlying
-- | resource keeps running and its eventual callback is dropped.
-- | For cancellable bridges, use `asyncInterrupt`.
-- |
-- | ```purescript
-- | -- wrap a Node.js-style (err, value) callback
-- | readFile :: String -> RIO r (fs :: FsError) Buffer
-- | readFile path = async \resume ->
-- |   Fs.readFile path \err value -> case toMaybe err of
-- |     Just e -> resume (Left (Variant.inj _fs e))
-- |     Nothing -> resume (Right value)
-- | ```
async
  :: forall r e a
   . ((Either (Variant e) a -> Effect Unit) -> Effect Unit)
  -> RIO r e a
async register = RIO \_ -> Aff.makeAff \resume -> do
  register \resolution -> resume (Right resolution)
  pure Aff.nonCanceler

-- | Like `async`, but the register function returns an `Effect Unit`
-- | that will be invoked if the fiber is interrupted before the
-- | callback fires. Use this to wire cancellation through to the
-- | underlying API (clearing a timer, aborting a fetch, removing an
-- | event listener).
-- |
-- | The cancellation effect runs once, on the interrupting fiber.
-- | It must be idempotent and non-blocking; if it raises, the
-- | exception is reported as an `Aff` defect on the interrupter.
-- |
-- | ```purescript
-- | -- a fetch with AbortController-backed cancellation
-- | fetchJSON :: URL -> RIO r (http :: HttpError) Json
-- | fetchJSON url = asyncInterrupt \resume -> do
-- |   controller <- newAbortController
-- |   doFetch url controller resume
-- |   pure (abort controller)
-- | ```
asyncInterrupt
  :: forall r e a
   . ((Either (Variant e) a -> Effect Unit) -> Effect (Effect Unit))
  -> RIO r e a
asyncInterrupt register = RIO \_ -> Aff.makeAff \resume -> do
  cancel <- register \resolution -> resume (Right resolution)
  pure (Aff.Canceler \_ -> liftEffect cancel)

-- | Filter an array using an effectful predicate, running every
-- | predicate call concurrently. Preserves input order on the
-- | survivors.
-- |
-- | Failure semantics match `parTraverse`: the first typed failure
-- | from a predicate call cancels the rest and surfaces on the
-- | parent row.
-- |
-- | ```purescript
-- | -- keep only the URLs that respond, with a 1s timeout each
-- | reachable :: Array URL -> RIO r e (Array URL)
-- | reachable urls = filterPar
-- |   (\u -> map (_ /= Nothing) (timeout (Milliseconds 1000.0) (ping u)))
-- |   urls
-- | ```
filterPar
  :: forall r e a
   . (a -> RIO r e Boolean)
  -> Array a
  -> RIO r e (Array a)
filterPar pred as = do
  flags <- parTraverse (\a -> map (Tuple a) (pred a)) as
  pure (filterKept flags)
  where
  filterKept = map (\(Tuple a _) -> a) <<< filterTrue

  filterTrue :: Array (Tuple a Boolean) -> Array (Tuple a Boolean)
  filterTrue = foldr step []
    where
    step (Tuple a true) acc = [ Tuple a true ] <> acc
    step _ acc = acc

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

-- | A timeout that produces a typed failure on expiry rather than
-- | wrapping the result in `Maybe`. The caller supplies the failure
-- | the row should see when the deadline fires, so the call site
-- | doesn't need a `case _ of Just x -> ...; Nothing -> fail ...`
-- | shim.
-- |
-- | Mirrors ZIO `ZIO.timeoutFail` / Effect `Effect.timeoutFail`. The
-- | losing action is interrupted under the usual `race` semantics,
-- | and any resources it holds via `acquireRelease` or `Scope` are
-- | released.
-- |
-- | ```purescript
-- | -- treat >500ms as a typed `slow` failure rather than a Maybe
-- | row <- timeoutFail
-- |   (Proxy :: Proxy "slow") "fetchFromCache"
-- |   (Milliseconds 500.0)
-- |   (fetchFromCache key)
-- | ```
timeoutFail
  :: forall r e sym a tail b
   . Row.Cons sym a tail e
  => IsSymbol sym
  => Proxy sym
  -> a
  -> Milliseconds
  -> RIO r e b
  -> RIO r e b
timeoutFail sym a ms action = do
  result <- timeout ms action
  case result of
    Just b -> pure b
    Nothing -> RIO \_ -> pure (Left (Variant.inj sym a))
