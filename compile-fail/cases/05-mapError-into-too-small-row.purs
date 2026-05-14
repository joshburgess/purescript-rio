-- Case: `mapError` translates a typed failure into a different tag, but
-- the resulting program is then handed to `runRIO'`, which requires an
-- empty error row.
--
-- The inner program raises `parse :: String`; `mapError` rewrites that
-- into `notFound :: Unit`. The residual error row is `(notFound :: Unit)`,
-- not `()`. `runRIO'` (which wants `RIO () () a`) cannot accept it.
module Scratch where

import Prelude

import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, mapError, runRIO')

inner :: RIO () (parse :: String) Int
inner = fail (Proxy :: Proxy "parse") "bad"

translate :: Variant (parse :: String) -> Variant (notFound :: Unit)
translate =
  Variant.case_
    # Variant.on (Proxy :: Proxy "parse")
        (\_ -> Variant.inj (Proxy :: Proxy "notFound") unit)

-- The residual error row is (notFound :: Unit), not (). The handoff to
-- runRIO' must be rejected.
result :: Aff Int
result = runRIO' (mapError translate inner)
