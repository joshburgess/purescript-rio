-- | Fork-based concurrency for `rio-fiber`.
-- |
-- | The runtime-native primitives live in `RIO.Fiber.Core` (`fork` /
-- | `join` / `interrupt` / `race` / `parTraverse` / `validatePar` /
-- | `awaitOutcome` / `poll` / `timeout` / ...) and in
-- | `RIO.Fiber.Scope` (`forkScoped` / `forkSupervised`). This module
-- | re-exports that surface and fills in the aff-compatible names
-- | that don't yet have a direct counterpart, so code ported from
-- | `RIO.Aff.Concurrency` keeps the same call sites with a one-line
-- | module change.
-- |
-- | What rio-fiber adds on top of the aff surface:
-- |
-- |   * Every fiber is "untracked" in the aff sense: there is no
-- |     per-fiber `Ref` of an `Exit` shadowing `Outcome`. `poll` reads
-- |     the runtime's authoritative state directly, so the
-- |     `forkUntracked` / `forkAllUntracked` distinction collapses;
-- |     we keep the names as aliases for portability.
-- |   * `await` returns an `Exit e a` (via `RIO.Fiber.Exit.fromOutcome`)
-- |     so the shape matches aff. The native variant that surfaces
-- |     the four-case `Outcome` is `awaitOutcome` (re-exported from
-- |     `Core`).
-- |   * `joinAllPar` composes failures with `Cause.both`, including
-- |     interrupts (aff's `combineParallel` only walked typed-failure
-- |     and defect causes; the fiber `Cause` carries `Interrupt` as a
-- |     first-class case, and we keep it).
-- |   * `async` matches the aff signature (register returns
-- |     `Effect Unit`, non-cancellable). `asyncInterrupt` matches the
-- |     aff signature too (register returns `Effect (Effect Unit)`)
-- |     and is an alias for the runtime-native `Core.async`.
module RIO.Fiber.Concurrency
  ( module Exports
  , await
  , awaitAll
  , async
  , asyncInterrupt
  , fiberId
  , filterPar
  , forkAllUntracked
  , forkUntracked
  , joinAllPar
  , parSequence
  , parTraverseN
  , partitionPar
  , raceEither
  , timeoutFail
  , validate
  ) where

import Prelude

import Data.Array (concat, drop, length, take) as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy)

import RIO.Fiber.Cause as Cause
import RIO.Fiber.Core
  ( RIO
  , async
  , awaitAllOutcomes
  , awaitOutcome
  , catchAll
  , fail
  , fork
  , forkAll
  , parTraverse
  , race
  , timeout
  ) as F
import RIO.Fiber.Core
  ( RIO
  , awaitOutcome
  , awaitAllOutcomes
  , bracket
  , causeOf
  , die
  , ensuring
  , ensuringWith
  , fail
  , failCause
  , firstSuccessOf
  , forever
  , fork
  , forkAll
  , forkAllInline
  , forkInline
  , interrupt
  , join
  , joinAll
  , liftEffect
  , loop
  , never
  , parTraverse
  , partition
  , poll
  , race
  , raceAll
  , timeout
  , uninterruptible
  , uninterruptibleMask
  , validatePar
  , zipFiber
  , zipPar
  , zipWithFiber
  , zipWithPar
  ) as Exports
import RIO.Fiber.Exit (Exit(..), fromOutcome)
import RIO.Fiber.FiberId (FiberId(..))
import RIO.Fiber.FiberId (FiberId) as Exports
import RIO.Fiber.Internal (Fiber)
import RIO.Fiber.Internal (Fiber) as Exports
import RIO.Fiber.Internal as Internal
import RIO.Fiber.Scope (forkScoped, forkSupervised) as Exports

-- | The stable identity of a forked fiber, allocated by the runtime
-- | at fork time and unique across the host process.
fiberId :: forall e a. Fiber e a -> FiberId
fiberId fib = FiberId (Internal._fiberId fib)

-- | Wait for a fiber to finish and surface its terminal `Exit`,
-- | including the `Cause` tree for any failure. Unlike `join`,
-- | `await` does not unwind the typed error into the caller's row:
-- | success and failure (typed, defect, or interrupt) both reach
-- | the caller as a plain value.
-- |
-- | For the four-case `Outcome` shape, use `awaitOutcome` directly.
await :: forall r e e' a. Fiber e a -> F.RIO r e' (Exit e a)
await fib = map fromOutcome (F.awaitOutcome fib)

-- | Await every fiber in the array and return their `Exit`s in
-- | input order. Failures (typed, defect, or interrupt) are
-- | reported as `Failure (Cause e)` slots within the returned
-- | array; nothing escapes to the parent row.
awaitAll
  :: forall r e e' a
   . Array (Fiber e a)
  -> F.RIO r e' (Array (Exit e a))
awaitAll fibs = map (map fromOutcome) (F.awaitAllOutcomes fibs)

-- | Wait for every fiber in the array and combine their outcomes
-- | into a single `Exit`. If every fiber succeeded, the result is
-- | `Success` of the value array in input order. Otherwise the
-- | result is `Failure c` where `c` is the left-leaning `Both`
-- | composition of every failed branch's cause, so no failure is
-- | lost when several siblings fail concurrently.
joinAllPar
  :: forall r e e' a
   . Array (Fiber e a)
  -> F.RIO r e' (Exit e (Array a))
joinAllPar fibs = do
  exits <- awaitAll fibs
  pure (foldl step (Success []) exits)
  where
  step :: Exit e (Array a) -> Exit e a -> Exit e (Array a)
  step acc exit = case acc, exit of
    Success xs, Success a -> Success (xs <> [ a ])
    Success _, Failure c -> Failure c
    Failure c, Success _ -> Failure c
    Failure c1, Failure c2 -> Failure (Cause.both c1 c2)

-- | Fork an `RIO` computation into a new fiber. Alias for `fork`.
-- |
-- | Kept for source-level compatibility with aff. In aff there was a
-- | distinction between `fork` (with per-fiber `Ref` state tracking
-- | for `poll`) and `forkUntracked` (no tracking). The fiber
-- | runtime owns the state directly, so the distinction collapses;
-- | both names route to the same primitive.
forkUntracked :: forall r e a. F.RIO r e a -> F.RIO r e (Fiber e a)
forkUntracked = F.fork

-- | Fork an array of `RIO` computations into fresh fibers in one
-- | pass. Alias for `forkAll`; see `forkUntracked` for why the
-- | "untracked" suffix is a no-op in rio-fiber.
forkAllUntracked
  :: forall r e a
   . Array (F.RIO r e a)
  -> F.RIO r e (Array (Fiber e a))
forkAllUntracked = F.forkAll

-- | Build an `RIO` action from a callback-style effect with no
-- | cancellation. The register function receives a resume callback
-- | (called once with `Right` for success or `Left` for a typed
-- | failure) and returns `Effect Unit`.
-- |
-- | If the fiber is interrupted while waiting for the callback, the
-- | underlying resource keeps running and its eventual callback is
-- | dropped. For cancellable bridges use `asyncInterrupt`.
async
  :: forall r e a
   . ((Either (Variant e) a -> Effect Unit) -> Effect Unit)
  -> F.RIO r e a
async register = F.async \resume -> do
  register resume
  pure (pure unit)

-- | Like `async`, but the register function returns an
-- | `Effect (Effect Unit)` whose inner effect runs if the fiber is
-- | interrupted before the callback fires. Use this to wire
-- | cancellation through to the underlying API (clearing a timer,
-- | aborting a fetch, removing an event listener).
-- |
-- | Identical to the runtime-native `RIO.Fiber.Core.async`; kept
-- | under this name so call sites ported from
-- | `RIO.Aff.Concurrency.asyncInterrupt` keep compiling.
asyncInterrupt
  :: forall r e a
   . ((Either (Variant e) a -> Effect Unit) -> Effect (Effect Unit))
  -> F.RIO r e a
asyncInterrupt = F.async

-- | Run two actions concurrently and pick the first to finish.
-- | Preserves which arm won: the left arm becomes `Left`, the right
-- | arm becomes `Right`. The losing arm is interrupted under the
-- | usual `race` semantics.
raceEither
  :: forall r e a b
   . F.RIO r e a
  -> F.RIO r e b
  -> F.RIO r e (Either a b)
raceEither ra rb = F.race (map Left ra) (map Right rb)

-- | A timeout that produces a typed failure on expiry rather than
-- | wrapping the result in `Maybe`. The caller supplies the failure
-- | the row should see when the deadline fires, so the call site
-- | doesn't need a `case _ of Just x -> ...; Nothing -> fail ...`
-- | shim.
timeoutFail
  :: forall r e sym a tail b
   . Row.Cons sym a tail e
  => IsSymbol sym
  => Proxy sym
  -> a
  -> Milliseconds
  -> F.RIO r e b
  -> F.RIO r e b
timeoutFail sym a ms action = do
  result <- F.timeout ms action
  case result of
    Just b -> pure b
    Nothing -> F.fail (Variant.inj sym a)

-- | Run every action in an array concurrently and collect the
-- | results. The identity case of `parTraverse`.
parSequence :: forall r e a. Array (F.RIO r e a) -> F.RIO r e (Array a)
parSequence = F.parTraverse identity

-- | Bounded-concurrency parallel traversal. At most `n` actions
-- | run concurrently; the input array is split into chunks of size
-- | `n` and each chunk is `parTraverse`d in turn.
-- |
-- | `n <= 0` is treated as `1` (sequential). Short-circuit semantics
-- | match `parTraverse`: the first typed failure inside a chunk
-- | cancels its siblings and aborts the remaining chunks.
parTraverseN
  :: forall r e a b
   . Int
  -> (a -> F.RIO r e b)
  -> Array a
  -> F.RIO r e (Array b)
parTraverseN n f as =
  let
    size = if n <= 1 then 1 else n
    chunks = chunksOf size as
  in
    Array.concat <$> traverse (F.parTraverse f) chunks

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n as
  | Array.length as == 0 = []
  | otherwise = [ Array.take n as ] <> chunksOf n (Array.drop n as)

-- | Run every action in an array concurrently and split the results
-- | into typed failures and successes, preserving input order within
-- | each side. Unlike `parTraverse`, no branch is cancelled when
-- | another fails: every action runs to completion.
-- |
-- | Defects and interrupts are not caught: they escape just like
-- | they would from any other concurrent walk. Use this when each
-- | element's *typed* failure is independently meaningful.
partitionPar
  :: forall r e e' a b
   . (a -> F.RIO r e b)
  -> Array a
  -> F.RIO r e' (Tuple (Array (Variant e)) (Array b))
partitionPar f as = do
  results <- F.parTraverse
    (\a -> F.catchAll (\v -> pure (Left v)) (Right <$> f a))
    as
  pure (foldl step (Tuple [] []) results)
  where
  step (Tuple ls rs) (Left v) = Tuple (ls <> [ v ]) rs
  step (Tuple ls rs) (Right b) = Tuple ls (rs <> [ b ])

-- | Sequential sibling of `validatePar`: same accumulating-typed-
-- | error semantics, but actions run one after another in input
-- | order rather than concurrently. Use when ordering matters (the
-- | first failure's diagnostics depend on side effects from earlier
-- | items) or when parallelism is undesired.
-- |
-- | The error order is deterministic: input order, not finish order.
validate
  :: forall r e e' a b
   . (a -> F.RIO r e b)
  -> Array a
  -> F.RIO r e' (Either (Array (Variant e)) (Array b))
validate f as = do
  results <- traverse
    (\a -> F.catchAll (\v -> pure (Left v)) (Right <$> f a))
    as
  let
    Tuple errs succs = foldl step (Tuple [] []) results
  pure if Array.length errs == 0 then Right succs else Left errs
  where
  step (Tuple ls rs) (Left v) = Tuple (ls <> [ v ]) rs
  step (Tuple ls rs) (Right b) = Tuple ls (rs <> [ b ])

-- | Filter an array using an effectful predicate, running every
-- | predicate call concurrently. Preserves input order on the
-- | survivors. Failure semantics match `parTraverse`.
filterPar
  :: forall r e a
   . (a -> F.RIO r e Boolean)
  -> Array a
  -> F.RIO r e (Array a)
filterPar pred as = do
  flagged <- F.parTraverse (\a -> map (Tuple a) (pred a)) as
  pure (foldl keep [] flagged)
  where
  keep acc (Tuple a true) = acc <> [ a ]
  keep acc _ = acc
