-- Case: `provide` is called with a value whose type does not match the
-- inner program's required service type.
--
-- The inner program asks for `logger :: { name :: String }`. We try to
-- provide a number instead. The compiler must reject this.
module Scratch where

import Prelude

import Data.Either (Either)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask, provide, runRIO)

inner :: forall e. RIO (logger :: { name :: String }) e String
inner = do
  logger <- ask (Proxy :: Proxy "logger")
  pure logger.name

-- This call should fail: 99 is an Int, not a record with a `name` field.
result :: forall e. Aff (Either (Variant e) String)
result = runRIO (provide (Proxy :: Proxy "logger") 99 inner)
