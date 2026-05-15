-- | `Exit e a`: the canonical shape of a finished program.
-- |
-- | An RIO program either completes successfully with a value of
-- | type `a`, or fails with a `Cause e` (a tree that records typed
-- | failures, defects, and how they composed under sequential or
-- | parallel combinators). `Exit` is just the sum of those two
-- | outcomes.
-- |
-- | Mirrors ZIO `Exit` and Effect-TS `Exit`. Reach for it when you
-- | want a single value that pattern-matches on "did the program
-- | succeed", rather than nesting the typed-failure `Variant` inside
-- | an outer `Either`.
-- |
-- | ```purescript
-- | exit <- runRIOExit program
-- | case exit of
-- |   Success a -> handleResult a
-- |   Failure c -> logCause c
-- | ```
module RIO.Exit
  ( Exit(..)
  , succeed
  , fail
  , die
  , isSuccess
  , isFailure
  , map_
  , mapCause
  , foldExit
  , toEither
  , fromEither
  , runRIOExit
  , runRIOExit'
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)

import RIO.Cause (Cause(..), fromOutcome)
import RIO.Internal (RIO, unRIO)

-- | The result of a finished program.
-- |
-- |   * `Success a` carries the success value.
-- |   * `Failure c` carries the failure cause: a typed failure, a
-- |     defect, or a tree of either composed in parallel or
-- |     sequence.
data Exit e a
  = Success a
  | Failure (Cause e)

-- | Wrap a value as a successful exit.
succeed :: forall e a. a -> Exit e a
succeed = Success

-- | Wrap a typed failure as a failed exit. The cause carries a
-- | single `Fail` node with the supplied `Variant`.
fail :: forall e a. Variant e -> Exit e a
fail = Failure <<< Fail

-- | Wrap a defect as a failed exit. The cause carries a single
-- | `Die` node with the supplied `Error`.
die :: forall e a. Error -> Exit e a
die = Failure <<< Die

-- | `true` when the exit is a `Success`.
isSuccess :: forall e a. Exit e a -> Boolean
isSuccess = case _ of
  Success _ -> true
  Failure _ -> false

-- | `true` when the exit is a `Failure`.
isFailure :: forall e a. Exit e a -> Boolean
isFailure = case _ of
  Success _ -> false
  Failure _ -> true

-- | Transform the success value. The cause (if any) is left
-- | untouched.
map_ :: forall e a b. (a -> b) -> Exit e a -> Exit e b
map_ f = case _ of
  Success a -> Success (f a)
  Failure c -> Failure c

-- | Transform the failure cause. The success value (if any) is
-- | left untouched.
mapCause :: forall e e' a. (Cause e -> Cause e') -> Exit e a -> Exit e' a
mapCause f = case _ of
  Success a -> Success a
  Failure c -> Failure (f c)

-- | Collapse an `Exit` into a single value by handling both
-- | sides.
foldExit :: forall e a b. (Cause e -> b) -> (a -> b) -> Exit e a -> b
foldExit onFailure onSuccess = case _ of
  Success a -> onSuccess a
  Failure c -> onFailure c

-- | Project an `Exit` into the `Either (Cause e) a` shape. Useful
-- | when working with library code that expects the `Either` form.
toEither :: forall e a. Exit e a -> Either (Cause e) a
toEither = case _ of
  Success a -> Right a
  Failure c -> Left c

-- | Inverse of `toEither`: lift an `Either (Cause e) a` back to
-- | the named `Exit` shape.
fromEither :: forall e a. Either (Cause e) a -> Exit e a
fromEither = case _ of
  Right a -> Success a
  Left c -> Failure c

-- | Run an `RIO` whose environment row is empty, surfacing the
-- | full result as an `Exit`. Defects raised through `die` or any
-- | uncaught `Aff` exception are captured as `Failure (Die err)`
-- | rather than aborting the host fiber.
-- |
-- | The dual of `RIO.Core.runRIO`: instead of an
-- | `Either (Variant e) a` with defects propagating, you receive
-- | a single value that distinguishes typed failures from
-- | defects.
runRIOExit :: forall e a. RIO () e a -> Aff (Exit e a)
runRIOExit m = do
  outcome <- attempt (unRIO m {})
  pure (fromEither (fromOutcome outcome))

-- | `runRIOExit` for a program whose environment row is empty and
-- | whose error row is also empty: a fully-handled program can
-- | still fail with a defect, so the result type is still
-- | `Exit () a`. Use this when every typed failure has been
-- | caught but you still want to observe defects.
runRIOExit' :: forall a. RIO () () a -> Aff (Exit () a)
runRIOExit' = runRIOExit
