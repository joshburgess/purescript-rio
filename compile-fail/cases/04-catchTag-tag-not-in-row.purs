-- Case: `catchTag` is called with a `Proxy` whose tag is not in the
-- inner program's error row.
--
-- The inner program has error row `(parse :: String)`. We try to
-- catch a `notFound` tag that doesn't exist in that row. The `Cons`
-- constraint cannot be solved: there's no `notFound` label to peel
-- off `(parse :: String)`.
module Scratch where

import Prelude

import Effect.Aff (Aff)
import Data.Either (Either)
import Data.Variant (Variant)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, catchTag, fail, runRIO)

inner :: RIO () (parse :: String) Int
inner = fail (Proxy :: Proxy "parse") "bad"

-- The tag `notFound` is not in the row `(parse :: String)`. The
-- compiler must reject this.
result :: forall e. Aff (Either (Variant e) Int)
result = runRIO (catchTag (Proxy :: Proxy "notFound") (\_ -> pure 0) inner)
