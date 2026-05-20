-- | Typeclass-driven typed failure for domain error types.
-- |
-- | The primitive `fail` in `RIO.Aff.Error` always asks for the row tag
-- | (a `Proxy`) at the call site:
-- |
-- | ```purescript
-- | fail (Proxy :: Proxy "database") ConnectionLost
-- | ```
-- |
-- | Most codebases settle on a stable name per error type. `FailWith`
-- | encodes that decision once and lets the call site drop the proxy:
-- |
-- | ```purescript
-- | data DatabaseError = ConnectionLost | QueryFailed String
-- |
-- | instance FailWith DatabaseError "database"
-- |
-- | -- ergonomic call site
-- | _ <- failWith ConnectionLost
-- | ```
-- |
-- | The row constraint still tracks the tag and payload type, so the
-- | inferred error row still carries `(database :: DatabaseError | _)`.
-- | A `catchTag (Proxy :: Proxy "database")` removes it as usual.
-- |
-- | This is purely sugar over `RIO.Aff.Error.fail`. It does not introduce
-- | a new failure channel, so it composes cleanly with `catchTag`,
-- | `catchAll`, `mapError`, and `sandbox`.
module RIO.Aff.Fail
  ( class FailWith
  , failWith
  ) where

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO)
import RIO.Aff.Error (fail)

-- | A domain error type `e` carries an instance that nails it to a
-- | specific row tag. The functional dependency `e -> sym` means
-- | each type has a single tag (no two instances may disagree).
-- |
-- | The class has no methods - the instance is purely a phantom
-- | binding from `e` to `sym`, consumed by `failWith` below.
class FailWith (e :: Type) (sym :: Symbol) | e -> sym

-- | Raise a typed failure for a domain error type with a
-- | pre-declared `FailWith` instance.
-- |
-- | The error row in the inferred type still tracks the tag and
-- | payload, so `catchTag (Proxy :: Proxy "<the tag>")` still
-- | discharges it.
failWith
  :: forall e sym env r r' a
   . FailWith e sym
  => IsSymbol sym
  => Row.Cons sym e r' r
  => e
  -> RIO env r a
failWith = fail (Proxy :: Proxy sym)
