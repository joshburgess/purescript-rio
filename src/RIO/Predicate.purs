-- | A tiny standalone predicate combinator library.
-- |
-- | A `Predicate a` wraps a function `a -> Boolean` together with a
-- | small algebra: boolean combinators (`and`, `or`, `not`), input
-- | adaptation (`contramap`), and constant builders (`always`,
-- | `never`). The runtime representation is just the function, so
-- | `runPredicate` is free; the wrapper exists so the type-class
-- | combinators have something to dispatch on.
-- |
-- | This module is intentionally independent of `RIO.Schema`:
-- | `Schema.refine` takes a refinement `a -> Maybe String` (a
-- | failure with a message), whereas `Predicate a` is the pure
-- | yes/no shape. The two compose: build a `Predicate` for the
-- | rule, then turn it into a refinement at the call site.
-- |
-- | ```purescript
-- | positive :: Predicate Int
-- | positive = mkPredicate (_ > 0)
-- |
-- | even' :: Predicate Int
-- | even' = mkPredicate (\n -> mod n 2 == 0)
-- |
-- | positiveAndEven :: Predicate Int
-- | positiveAndEven = positive `and` even'
-- |
-- | runPredicate positiveAndEven 4   -- true
-- | runPredicate positiveAndEven 3   -- false
-- | ```
-- |
-- | Adapting the input is the usual way to reuse a predicate across
-- | record fields:
-- |
-- | ```purescript
-- | type User = { age :: Int, name :: String }
-- | adult :: Predicate User
-- | adult = contramap _.age (mkPredicate (_ >= 18))
-- | ```
module RIO.Predicate
  ( Predicate
  , mkPredicate
  , runPredicate
  , always
  , never
  , and
  , or
  , not
  , contramap
  , any
  , all
  ) where

import Prelude hiding (not)
import Prelude as P

import Data.Array (foldr)

-- | A function `a -> Boolean` wrapped so combinators dispatch on it.
-- | Runtime cost over the raw function is zero: `runPredicate` is
-- | the inverse of `mkPredicate`.
newtype Predicate a = Predicate (a -> Boolean)

-- | Wrap a `a -> Boolean` as a `Predicate`.
mkPredicate :: forall a. (a -> Boolean) -> Predicate a
mkPredicate = Predicate

-- | Apply a predicate to a value.
runPredicate :: forall a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

-- | The predicate that accepts every value. Identity for `and`.
always :: forall a. Predicate a
always = Predicate (\_ -> true)

-- | The predicate that rejects every value. Identity for `or`.
never :: forall a. Predicate a
never = Predicate (\_ -> false)

-- | Conjunction: both predicates must hold. Short-circuits on the
-- | first `false`.
and :: forall a. Predicate a -> Predicate a -> Predicate a
and (Predicate p) (Predicate q) = Predicate (\a -> p a && q a)

-- | Disjunction: at least one predicate must hold. Short-circuits
-- | on the first `true`.
or :: forall a. Predicate a -> Predicate a -> Predicate a
or (Predicate p) (Predicate q) = Predicate (\a -> p a || q a)

-- | Negation: flip the verdict.
not :: forall a. Predicate a -> Predicate a
not (Predicate p) = Predicate (\a -> P.not (p a))

-- | Adapt the input. `contramap f p` runs `f` and then `p`, so
-- | `Predicate` is a `Contravariant` functor.
-- |
-- | ```purescript
-- | adult :: Predicate User
-- | adult = contramap _.age (mkPredicate (_ >= 18))
-- | ```
contramap :: forall a b. (b -> a) -> Predicate a -> Predicate b
contramap f (Predicate p) = Predicate (\b -> p (f b))

-- | The disjunction of every predicate in the array. Empty input
-- | gives `never`, matching `foldr or never`.
any :: forall a. Array (Predicate a) -> Predicate a
any = foldr or never

-- | The conjunction of every predicate in the array. Empty input
-- | gives `always`, matching `foldr and always`.
all :: forall a. Array (Predicate a) -> Predicate a
all = foldr and always
