-- | A standalone type-level brand. Use it to tag a value of
-- | some carrier type (typically a primitive like `Int` or
-- | `String`) with a `Symbol` so the type checker can distinguish
-- | otherwise-identical carriers.
-- |
-- | This is the same shape `RIO.Schema.Branded` carries, but
-- | extracted so application code can adopt brands without
-- | depending on `RIO.Schema`. The two are interoperable: a
-- | `Brand tag a` and a `Schema.Branded tag a` have the same
-- | runtime representation. (They are not the same nominal
-- | type, however, so a conversion goes through `unbrand` plus
-- | re-wrapping; the `coerce` family from `Data.Newtype` works
-- | on both sides.)
-- |
-- | ## Why brand?
-- |
-- | Brands let you communicate invariants in the type system
-- | that the carrier cannot enforce on its own:
-- |
-- | ```purescript
-- | type UserId = Brand "UserId" Int
-- | type OrderId = Brand "OrderId" Int
-- |
-- | -- The two are distinct types even though both wrap Int:
-- | lookupUser :: UserId -> Aff (Maybe User)
-- | -- lookupUser (mkBrand 7 :: OrderId)  -- type error
-- | ```
-- |
-- | ## Minting branded values
-- |
-- | `mkBrand` is the constructor. It is *not* a smart
-- | constructor: there is no validation. If the brand is meant
-- | to carry an invariant beyond "this `Int` is a user id" - say,
-- | "this `String` is non-empty" - validate the carrier first,
-- | then call `mkBrand`. Pair `Brand` with `RIO.Schema.refine`
-- | for the validation-flavoured brand.
-- |
-- | ```purescript
-- | userIdFromInt :: Int -> Maybe UserId
-- | userIdFromInt n
-- |   | n > 0 = Just (mkBrand n)
-- |   | otherwise = Nothing
-- | ```
-- |
-- | ## Reading the carrier
-- |
-- | `unbrand` strips the tag. `reflectBrand` returns the tag
-- | itself as a `String`, useful for diagnostics and serialisers.
-- |
-- | ```purescript
-- | renderError :: forall tag. IsSymbol tag => Brand tag Int -> String
-- | renderError b = reflectBrand b <> "(" <> show (unbrand b) <> ")"
-- | ```
module RIO.Brand
  ( Brand
  , mkBrand
  , unbrand
  , reflectBrand
  , retagBrand
  ) where

import Prelude

import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

-- | A value of type `a` tagged at the type level with a
-- | `Symbol`. Carries no runtime structure beyond `a`; the tag
-- | flows in the type system.
newtype Brand :: Symbol -> Type -> Type
newtype Brand tag a = Brand a

derive newtype instance eqBrand :: Eq a => Eq (Brand tag a)
derive newtype instance ordBrand :: Ord a => Ord (Brand tag a)
derive newtype instance showBrand :: Show a => Show (Brand tag a)
derive newtype instance semigroupBrand :: Semigroup a => Semigroup (Brand tag a)
derive newtype instance monoidBrand :: Monoid a => Monoid (Brand tag a)

-- | Wrap a value with a brand. The brand is inferred from the
-- | call-site's type ascription:
-- |
-- | ```purescript
-- | type UserId = Brand "UserId" Int
-- | uid :: UserId
-- | uid = mkBrand 7
-- | ```
mkBrand :: forall tag a. a -> Brand tag a
mkBrand = Brand

-- | Strip the brand and recover the carrier value.
unbrand :: forall tag a. Brand tag a -> a
unbrand (Brand a) = a

-- | Reflect the brand tag itself as a `String`. Useful in
-- | diagnostics, JSON Schema export, and error messages.
reflectBrand
  :: forall tag a
   . IsSymbol tag
  => Brand tag a
  -> String
reflectBrand _ = reflectSymbol (Proxy :: Proxy tag)

-- | Replace one brand with another. Intentionally explicit so
-- | retags show up in code review.
-- |
-- | The runtime representation is identical, so this never
-- | fails; the type-level rename is the whole point.
retagBrand :: forall tag tag' a. Brand tag a -> Brand tag' a
retagBrand (Brand a) = Brand a
