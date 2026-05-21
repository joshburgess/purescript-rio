-- | A reusable runtime: an environment record bundled with a runner.
-- |
-- | `runRIO` and `runRIO'` from `RIO.Fiber.Core`, plus the bridges in
-- | `RIO.Fiber.Aff`, are the standard top-of-`main` entry points:
-- | build the environment with `provideAt` / `provideAll`, then hand
-- | the resulting `RIO () e a` to the runner.
-- |
-- | `Runtime r` is the same idea factored out so it can be reused.
-- | You build the environment record once, wrap it in a `Runtime r`,
-- | and execute many programs against it. The shape pairs well with
-- | embedding RIO in a larger `Aff` codebase, with long-lived host
-- | processes that handle many requests against a stable service
-- | graph, and with test suites that want a fixture-style "give me a
-- | runtime" hook instead of re-`provide`ing every service per
-- | assertion.
-- |
-- | ```purescript
-- | -- main: build the runtime once
-- | main :: Effect Unit
-- | main = launchAff_ do
-- |   db <- openDatabase
-- |   logger <- pure stdoutLogger
-- |   let runtime = Runtime.make { db, logger }
-- |
-- |   -- run many programs against the same env
-- |   _ <- Runtime.runOrThrow runtime greet
-- |   _ <- Runtime.run runtime processRequest
-- |   pure unit
-- | ```
-- |
-- | `Runtime ()` is the degenerate case: no services required;
-- | `Runtime.run runtime` is then equivalent to `runAffEither`. The
-- | `unitRuntime` constant captures this for convenience.
module RIO.Fiber.Runtime
  ( Runtime
  , make
  , env
  , run
  , runExit
  , runOutcome
  , runOrThrow
  , unitRuntime
  ) where

import Prelude

import Data.Either (Either)
import Data.Variant (Variant)
import Effect.Aff (Aff)

import RIO.Fiber.Aff (runAff, runAffEither, runAffThrow)
import RIO.Fiber.Exit (Exit, fromOutcome)
import RIO.Fiber.Internal (Outcome, RIO)

-- | A built environment record paired with the implicit promise
-- | that it satisfies the row `r`. Construct with `make`; execute
-- | against it with `run` / `runExit` / `runOrThrow`.
newtype Runtime :: Row Type -> Type
newtype Runtime r = Runtime (Record r)

-- | Bundle an environment record into a runtime.
make :: forall r. Record r -> Runtime r
make = Runtime

-- | Recover the underlying environment record. Useful for one-off
-- | diagnostic logging or for handing the record to another runner
-- | without going through `run`.
env :: forall r. Runtime r -> Record r
env (Runtime r) = r

-- | A trivial runtime over the empty row. `run unitRuntime` is
-- | equivalent to `runAffEither`.
unitRuntime :: Runtime ()
unitRuntime = Runtime {}

-- | Run a program against the runtime. The typed-error channel
-- | surfaces on the `Left` branch of the `Either`; defects and
-- | interrupts become `Aff` exceptions, matching `runAffEither`.
run
  :: forall r e a
   . Runtime r
  -> RIO r e a
  -> Aff (Either (Variant e) a)
run (Runtime r) program = runAffEither program r

-- | Run a program against the runtime, surfacing the full failure
-- | structure as an `Exit`. Typed failures, defects, and interrupts
-- | all land in the `Failure` branch via the `Cause` tree; nothing
-- | escapes to the `Aff` error channel.
runExit
  :: forall r e a
   . Runtime r
  -> RIO r e a
  -> Aff (Exit e a)
runExit (Runtime r) program = fromOutcome <$> runAff program r

-- | Run a program against the runtime, returning the raw fiber
-- | `Outcome` (with `Interrupted` as a dedicated case). Use this
-- | when the four-case shape is more convenient than `Exit`'s
-- | `Cause`-tree projection.
runOutcome
  :: forall r e a
   . Runtime r
  -> RIO r e a
  -> Aff (Outcome e a)
runOutcome (Runtime r) program = runAff program r

-- | Run a program whose error row is empty. The runtime guarantees
-- | the env is satisfied; the empty error row guarantees no typed
-- | failure can reach this point, so the result comes back as a raw
-- | `a`. Defects and interrupts become `Aff` exceptions, matching
-- | `runAffThrow`.
-- |
-- | Use this after every typed failure has been handled via
-- | `catchTag` / `catchAll`.
runOrThrow
  :: forall a
   . Runtime ()
  -> RIO () () a
  -> Aff a
runOrThrow _ program = runAffThrow program
