-- Case: `provideAll` is called with a record that's missing a field
-- the inner program requires.
--
-- The inner program asks for `logger :: { name :: String }` AND
-- `requestId :: String`. The outer call hands `provideAll` a record
-- that only contains `logger`. The compiler must reject the call:
-- the record's row doesn't match the inner program's required row.
module Scratch where

import Prelude

import Data.Either (Either)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask, provideAll, runRIO)

inner
  :: forall e
   . RIO ( logger :: { name :: String }, requestId :: String ) e String
inner = do
  logger <- ask (Proxy :: Proxy "logger")
  rid <- ask (Proxy :: Proxy "requestId")
  pure (logger.name <> ":" <> rid)

-- The record passed to `provideAll` is missing the `requestId` field
-- that the inner program requires. The compiler must reject this.
result :: forall e. Aff (Either (Variant e) String)
result = runRIO (provideAll { logger: { name: "outer" } } inner)
