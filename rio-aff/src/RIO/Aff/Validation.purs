-- | `Validation e a`: an Applicative that accumulates typed
-- | failures into a `NonEmptyArray (Variant e)` instead of
-- | short-circuiting on the first one.
-- |
-- | RIO's monadic chain short-circuits on the first typed failure;
-- | that's the right shape when a later step genuinely depends on
-- | an earlier one. For parsers, form validators, and config
-- | loaders, the better shape is "report every error you can find
-- | in one pass". That's what `Validation` is for.
-- |
-- | Mirrors ZIO `Validation` and `Effect.Validation`. Compose
-- | with `<*>`, `<$>`, and `Apply.tuple`-style combinators to
-- | accumulate errors across independent fields; lift back into
-- | `RIO` with `toRIO` at the boundary where you want short-
-- | circuit behaviour to resume.
-- |
-- | ```purescript
-- | type Errs = (badName :: String, badAge :: String)
-- |
-- | validateUser
-- |   :: { name :: String, age :: Int }
-- |   -> Validation Errs { name :: String, age :: Int }
-- | validateUser raw =
-- |   { name: _, age: _ }
-- |     <$> nameField raw.name
-- |     <*> ageField raw.age
-- | ```
module RIO.Aff.Validation
  ( Validation(..)
  , success
  , failure
  , fromEither
  , toEither
  , fromVariant
  , toRIO
  , map_
  , apply_
  , collectAll
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEArray
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Variant (Variant)

import RIO.Aff.Error (rethrow)
import RIO.Aff.Internal (RIO)

-- | The result of an accumulating-error validation.
-- |
-- |   * `Success a` carries the validated value.
-- |   * `Failure errs` carries the non-empty list of typed
-- |     failures observed so far. `Apply` combines two failures
-- |     by concatenation, so `<*>` sees every error encountered.
data Validation e a
  = Success a
  | Failure (NonEmptyArray (Variant e))

-- | Build a successful validation.
success :: forall e a. a -> Validation e a
success = Success

-- | Build a failed validation from a single typed failure.
failure :: forall e a. Variant e -> Validation e a
failure v = Failure (NEArray.singleton v)

-- | Lift `Either (Variant e) a` into a `Validation`. The result
-- | carries at most one failure.
fromEither :: forall e a. Either (Variant e) a -> Validation e a
fromEither = case _ of
  Right a -> Success a
  Left v -> failure v

-- | Project a `Validation` back into `Either`. If multiple
-- | failures were accumulated, only the first is surfaced (the
-- | rest are dropped); use `toEither` only when you've already
-- | observed the full list via pattern-matching on `Failure`.
toEither
  :: forall e a
   . Validation e a
  -> Either (NonEmptyArray (Variant e)) a
toEither = case _ of
  Success a -> Right a
  Failure es -> Left es

-- | Alias for `failure` that reads more naturally at injection
-- | sites: `fromVariant (Variant.inj _ "oops")`.
fromVariant :: forall e a. Variant e -> Validation e a
fromVariant = failure

-- | Lift a `Validation` back into `RIO`. On `Success` the value
-- | flows through; on `Failure`, only the first accumulated
-- | failure is rethrown via the error row (the rest are dropped).
-- |
-- | Use this at the boundary where you want to "stop accumulating
-- | and resume short-circuiting": typically after validating an
-- | input record into a domain type and before running the rest
-- | of the program against that domain type.
toRIO :: forall r e a. Validation e a -> RIO r e a
toRIO = case _ of
  Success a -> pure a
  Failure es -> rethrow (NEArray.head es)

-- | Transform the success value of a validation. Errors pass
-- | through unchanged.
map_ :: forall e a b. (a -> b) -> Validation e a -> Validation e b
map_ f = case _ of
  Success a -> Success (f a)
  Failure es -> Failure es

-- | Apply a function in a validation to a value in a validation,
-- | concatenating failures from both sides. This is the
-- | accumulating-error analogue of `Apply.apply`.
apply_
  :: forall e a b
   . Validation e (a -> b)
  -> Validation e a
  -> Validation e b
apply_ = case _, _ of
  Success f, Success a -> Success (f a)
  Failure es, Success _ -> Failure es
  Success _, Failure es -> Failure es
  Failure es1, Failure es2 -> Failure (es1 <> es2)

instance functorValidation :: Functor (Validation e) where
  map = map_

instance applyValidation :: Apply (Validation e) where
  apply = apply_

instance applicativeValidation :: Applicative (Validation e) where
  pure = Success

-- | Accumulate over an array of validations. On all-success,
-- | returns the array of values. Otherwise, returns the
-- | concatenated list of every accumulated failure across the
-- | input.
-- |
-- | The order of accumulated failures matches the order of the
-- | input array.
collectAll
  :: forall e a
   . Array (Validation e a)
  -> Validation e (Array a)
collectAll = foldl step (Success [])
  where
  step :: Validation e (Array a) -> Validation e a -> Validation e (Array a)
  step acc x = (\xs a -> xs <> [ a ]) <$> acc <*> x
