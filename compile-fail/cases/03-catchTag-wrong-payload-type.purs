-- Case: `catchTag` is given a handler whose argument type doesn't
-- match the named tag's payload.
--
-- The tag `parse :: String` has payload type `String`, but the handler
-- expects an `Int`. The Cons constraint pins the payload to String, so
-- a handler of type `Int -> RIO ...` cannot satisfy it.
module Scratch where

import Prelude

import Effect.Aff (Aff)
import Data.Either (Either)
import Data.Variant (Variant)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, catchTag, fail, runRIO)

inner :: RIO () (parse :: String) Int
inner = fail (Proxy :: Proxy "parse") "bad"

-- The handler claims the payload is Int; it isn't.
result :: forall e. Aff (Either (Variant e) Int)
result = runRIO (catchTag (Proxy :: Proxy "parse") (\(n :: Int) -> pure n) inner)
