-- | Typed-error primitives for `RIO`.
-- |
-- | Phase 1.3 ships the raising side (`fail`). The handling side
-- | (`catchTag`, `catchAll`, `mapError`) arrives in Phase 3.
module RIO.Error
  ( fail
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant as Variant
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy)

import RIO.Internal (RIO(..))

-- | Raise a typed failure tagged with the symbol `sym`. The tag and its
-- | payload type are added to the inferred error row, where they can be
-- | caught later by `catchTag` (Phase 3) or surfaced via `runRIO`.
-- |
-- | ```purescript
-- | -- Inferred type:
-- | --   forall r b. RIO r (notFound :: { id :: Int } | _) b
-- | notFound id = fail (Proxy :: _ "notFound") { id }
-- | ```
-- |
-- | The result type is polymorphic in `b` because `fail` never returns a
-- | value; downstream binds inherit the failure unchanged.
fail
  :: forall sym a r e' e b
   . IsSymbol sym
  => Row.Cons sym a e' e
  => Proxy sym
  -> a
  -> RIO r e b
fail sym v = RIO \_ -> pure (Left (Variant.inj sym v))
