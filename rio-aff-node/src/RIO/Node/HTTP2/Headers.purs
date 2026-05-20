-- | Pure header helpers from `Node.Http2.Headers`. The whole
-- | upstream surface is already pure, so this module simply
-- | re-exports it under the `RIO.Aff.Node.HTTP2.*` prefix.
module RIO.Aff.Node.HTTP2.Headers
  ( module Exports
  ) where

import Node.Http2.Headers
  ( authority
  , lookup
  , method
  , mkHeaders
  , mkHeadersI
  , path
  , printHeaders
  , printHeaders'
  , scheme
  , status
  , unsafeToObject
  ) as Exports
