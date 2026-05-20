-- | RIO-flavoured wrappers around `Node.HTTP.IncomingMessage`.
-- |
-- | An `IncomingMessage` is a value (a handle on a partially or
-- | fully received request / response), not a capability, so this
-- | module mirrors the upstream surface by lifting each
-- | `Effect`-valued primitive into `RIO` and re-exporting the
-- | pure accessors and event handles unchanged.
module RIO.Aff.Node.HTTP.IncomingMessage
  ( module Exports
  , complete
  , socket
  , trailers
  , trailersDistinct
  ) where

import Data.Array.NonEmpty (NonEmptyArray)
import Data.Maybe (Maybe)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Node.HTTP.IncomingMessage
  ( closeH
  , cookies
  , headers
  , headersDistinct
  , httpVersion
  , method
  , rawHeaders
  , rawTrailers
  , statusCode
  , statusMessage
  , toReadable
  , url
  ) as Exports
import Node.HTTP.IncomingMessage as IM
import Node.HTTP.Types (IncomingMessage)
import Node.Net.Types (Socket, TCP)

import RIO.Aff.Core (RIO)

-- | Whether the message has been fully received.
complete
  :: forall r e mt. IncomingMessage mt -> RIO r e Boolean
complete im = liftEffect (IM.complete im)

-- | The TCP socket carrying the message, if any.
socket
  :: forall r e mt
   . IncomingMessage mt
  -> RIO r e (Maybe (Socket TCP))
socket im = liftEffect (IM.socket im)

-- | The HTTP trailers, if the message has any.
trailers
  :: forall r e mt
   . IncomingMessage mt
  -> RIO r e (Maybe (Object Foreign))
trailers im = liftEffect (IM.trailers im)

-- | The distinct-valued HTTP trailers.
trailersDistinct
  :: forall r e mt
   . IncomingMessage mt
  -> RIO r e (Maybe (Object (NonEmptyArray String)))
trailersDistinct im = liftEffect (IM.trailersDistinct im)
