-- | Core `RIO` type and runners.
-- |
-- | This is the entry-point module for the library. It re-exports `RIO`
-- | as an opaque type (the data constructor is hidden) along with the two
-- | runners users will reach for most often: `runRIO` for the general case
-- | and `runRIO'` for the fully-handled, no-error case.
-- |
-- | `unsafeRunRIO` is also exported for advanced use cases (custom runners,
-- | testing harnesses, FFI bridges). It is the raw inverse of the newtype
-- | and bypasses every guarantee `runRIO` provides; reach for it only when
-- | the safer runners cannot express what you need.
module RIO.Core
  ( module Exports
  , ifM
  , iterate
  , replicateM
  , replicateM_
  , runRIO
  , runRIO'
  , unlessM
  , unsafeRunRIO
  , whenM
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import RIO.Env (ask, asks, provide, provideAll) as Exports
import RIO.Error (catchAll, catchSome, catchTag, die, fail, foldRIO, mapBoth, mapError, option, orDie, orElse, orElseFail, orElseSucceed, refineOrDie, refineOrDieWith, rethrow, sandbox, tap, tapBoth, tapDefect, tapError, unsandbox) as Exports
import RIO.Internal (RIO, unRIO)
import RIO.Internal (RIO) as Exports
import RIO.Concurrency (Fiber, FiberId, async, asyncInterrupt, await, awaitAll, fiberId, filterPar, forever, fork, forkScoped, interrupt, join, joinAllPar, never, parSequence, parTraverse, parTraverseN, partition, partitionPar, poll, race, raceAll, raceEither, timeout, timeoutFail, uninterruptible, validate, validatePar, zipPar, zipWithPar) as Exports
import RIO.Deferred (Deferred, awaitDeferred, failDeferred, makeDeferred, pollDeferred, succeedDeferred) as Exports
import RIO.Layer (Layer, andThen, buildLayer, combine, fromRIO, fromRecord, passthrough, provideLayer) as Exports
import RIO.Memo (memoize) as Exports
import RIO.Resource (Scope, acquireRelease, addFinalizer, bracket, ensuring, onInterrupt, scoped) as Exports

-- | Run an `RIO` whose environment row is empty, surfacing the error
-- | channel as the `Left` branch of an `Either`.
-- |
-- | This is the runner to use when your program may still fail with a
-- | typed error and you want to inspect or pattern-match on the failure.
-- |
-- | ```purescript
-- | example :: Aff (Either (Variant (parse :: ParseError)) Int)
-- | example = runRIO program
-- | ```
runRIO :: forall e a. RIO () e a -> Aff (Either (Variant e) a)
runRIO m = unRIO m {}

-- | Run an `RIO` whose environment *and* error rows are both empty.
-- | The error row `()` is uninhabited, so this runner can return the
-- | success value directly without an `Either` wrapper.
-- |
-- | Use this after every required service has been `provide`d and every
-- | possible failure has been handled with `catchTag` / `catchAll`.
-- |
-- | ```purescript
-- | example :: Aff Int
-- | example = runRIO' fullyHandledProgram
-- | ```
runRIO' :: forall a. RIO () () a -> Aff a
runRIO' m = do
  res <- unRIO m {}
  case res of
    Right a -> pure a
    Left v -> Variant.case_ v

-- | The raw inverse of the `RIO` newtype: given an `RIO r e a` and a
-- | concrete `Record r`, hand back the underlying
-- | `Aff (Either (Variant e) a)`.
-- |
-- | Exposed for internal use (custom runners, test harnesses, FFI shims)
-- | where neither `runRIO` nor `runRIO'` fits. Prefer the safer runners
-- | whenever possible.
-- |
-- | ```purescript
-- | -- a custom runner that records the environment record before running
-- | runWithTrace
-- |   :: forall r e a. Record r -> RIO r e a -> Aff (Either (Variant e) a)
-- | runWithTrace env program = do
-- |   liftEffect (Console.log ("env: " <> show (Record.keys env)))
-- |   unsafeRunRIO program env
-- | ```
unsafeRunRIO :: forall r e a. RIO r e a -> Record r -> Aff (Either (Variant e) a)
unsafeRunRIO = unRIO

-- | Run the body when the effectful predicate returns `true`. The
-- | predicate is `RIO`, not pure `Boolean`, so it can read services,
-- | check refs, or raise typed failures of its own.
-- |
-- | Mirrors ZIO's `ZIO.whenZIO` and Haskell's `Control.Monad.whenM`.
-- |
-- | ```purescript
-- | flushIfDirty :: RIO Env e Unit
-- | flushIfDirty = whenM isDirty flush
-- | ```
whenM :: forall r e. RIO r e Boolean -> RIO r e Unit -> RIO r e Unit
whenM cond body = do
  b <- cond
  when b body

-- | The dual of `whenM`: run the body when the effectful predicate
-- | returns `false`.
unlessM :: forall r e. RIO r e Boolean -> RIO r e Unit -> RIO r e Unit
unlessM cond body = do
  b <- cond
  unless b body

-- | Branch on an effectful predicate. Both arms have the same result
-- | type, so this is the value-returning sibling of `whenM` /
-- | `unlessM`. Mirrors ZIO's `ZIO.ifZIO`.
ifM
  :: forall r e a
   . RIO r e Boolean
  -> RIO r e a
  -> RIO r e a
  -> RIO r e a
ifM cond thenBranch elseBranch = do
  b <- cond
  if b then thenBranch else elseBranch

-- | Run `action` `n` times and collect every result in input order.
-- | Non-positive `n` returns an empty array without running the
-- | action.
-- |
-- | Mirrors ZIO `ZIO.replicateM` / Effect-TS `Effect.replicate`.
-- | Sequential: each invocation runs after the previous one
-- | finishes. For concurrent replication, reach for
-- | `parSequence (Array.replicate n action)`.
-- |
-- | ```purescript
-- | -- sample the random source 100 times in a row
-- | samples <- replicateM 100 nextSample
-- | ```
replicateM :: forall r e a. Int -> RIO r e a -> RIO r e (Array a)
replicateM n action
  | n <= 0 = pure []
  | otherwise = do
      a <- action
      rest <- replicateM (n - 1) action
      pure ([ a ] <> rest)

-- | The discard sibling of `replicateM`: run `action` `n` times and
-- | throw the results away. Non-positive `n` is a no-op.
-- |
-- | ```purescript
-- | -- warm up the cache with 10 dummy reads
-- | replicateM_ 10 (load "warmup-key")
-- | ```
replicateM_ :: forall r e a. Int -> RIO r e a -> RIO r e Unit
replicateM_ n action
  | n <= 0 = pure unit
  | otherwise = do
      _ <- action
      replicateM_ (n - 1) action

-- | Iterate a stateful step: starting from `seed`, repeatedly apply
-- | `step` while `cont` holds, threading the accumulated state.
-- | Returns the final state.
-- |
-- | The predicate is checked *before* each step, so a `cont` that is
-- | `false` on the seed returns the seed unchanged without invoking
-- | `step`.
-- |
-- | Mirrors ZIO `ZIO.iterate`. Use this for bounded effectful loops
-- | that thread state through each iteration; for unbounded loops
-- | with the same body, reach for `forever` instead.
-- |
-- | ```purescript
-- | -- consume the queue until it is empty, counting drained items
-- | final <- iterate { queue, drained: 0 } (\s -> s.queue /= []) \s -> do
-- |   case Array.uncons s.queue of
-- |     Nothing -> pure s
-- |     Just { head: _, tail } ->
-- |       pure { queue: tail, drained: s.drained + 1 }
-- | ```
iterate
  :: forall r e a
   . a
  -> (a -> Boolean)
  -> (a -> RIO r e a)
  -> RIO r e a
iterate seed cont step =
  if cont seed then do
    next <- step seed
    iterate next cont step
  else pure seed
