-- | RIO-flavoured wrappers around `Node.HTTP.OutgoingMessage`.
-- |
-- | An `OutgoingMessage` is a value (a handle on a request /
-- | response being written), not a capability, so this module
-- | mirrors the upstream surface by lifting each `Effect`-valued
-- | primitive into `RIO` and re-exporting the event handles
-- | unchanged.
module RIO.Fiber.Node.HTTP.OutgoingMessage
  ( module Exports
  , addTrailers
  , appendHeader
  , appendHeaders
  , flushHeaders
  , getHeader
  , getHeaderNames
  , getHeaders
  , hasHeader
  , headersSent
  , removeHeader
  , setHeader
  , setHeader'
  , setTimeout
  , socket
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Foreign (Foreign)
import Foreign.Object (Object)
import Node.HTTP.OutgoingMessage
  ( drainH
  , finishH
  , prefinishH
  , toWriteable
  ) as Exports
import Node.HTTP.OutgoingMessage as OM
import Node.HTTP.Types (OutgoingMessage)
import Node.Net.Types (Socket, TCP)

import RIO.Fiber.Core (RIO, liftEffect)

-- | Append a trailer block to the message.
addTrailers
  :: forall r e
   . Object String
  -> OutgoingMessage
  -> RIO r e Unit
addTrailers o msg = liftEffect (OM.addTrailers o msg)

-- | Append a header value.
appendHeader
  :: forall r e
   . String
  -> String
  -> OutgoingMessage
  -> RIO r e Unit
appendHeader name value msg =
  liftEffect (OM.appendHeader name value msg)

-- | Append several values for the same header.
appendHeaders
  :: forall r e
   . String
  -> Array String
  -> OutgoingMessage
  -> RIO r e Unit
appendHeaders name values msg =
  liftEffect (OM.appendHeaders name values msg)

-- | Flush the queued headers without sending a body.
flushHeaders :: forall r e. OutgoingMessage -> RIO r e Unit
flushHeaders msg = liftEffect (OM.flushHeaders msg)

-- | Look up a single header.
getHeader
  :: forall r e
   . String
  -> OutgoingMessage
  -> RIO r e (Maybe String)
getHeader name msg = liftEffect (OM.getHeader name msg)

-- | The names of every header currently set.
getHeaderNames
  :: forall r e
   . String
  -> OutgoingMessage
  -> RIO r e (Array String)
getHeaderNames name msg =
  liftEffect (OM.getHeaderNames name msg)

-- | A copy of every header currently set.
getHeaders
  :: forall r e
   . OutgoingMessage
  -> RIO r e (Object Foreign)
getHeaders msg = liftEffect (OM.getHeaders msg)

-- | Whether a header has been set.
hasHeader
  :: forall r e
   . String
  -> OutgoingMessage
  -> RIO r e Boolean
hasHeader name msg = liftEffect (OM.hasHeader name msg)

-- | Whether the headers have already been flushed.
headersSent :: forall r e. OutgoingMessage -> RIO r e Boolean
headersSent msg = liftEffect (OM.headersSent msg)

-- | Remove a header.
removeHeader
  :: forall r e
   . String
  -> OutgoingMessage
  -> RIO r e Unit
removeHeader name msg = liftEffect (OM.removeHeader name msg)

-- | Set a single-value header.
setHeader
  :: forall r e
   . String
  -> String
  -> OutgoingMessage
  -> RIO r e Unit
setHeader name value msg =
  liftEffect (OM.setHeader name value msg)

-- | Set a multi-value header.
setHeader'
  :: forall r e
   . String
  -> Array String
  -> OutgoingMessage
  -> RIO r e Unit
setHeader' name value msg =
  liftEffect (OM.setHeader' name value msg)

-- | Arm the socket-inactivity timer on this outgoing message.
setTimeout
  :: forall r e
   . Milliseconds
  -> OutgoingMessage
  -> RIO r e Unit
setTimeout ms msg = liftEffect (OM.setTimeout ms msg)

-- | The TCP socket carrying the outgoing message, if any.
socket
  :: forall r e
   . OutgoingMessage
  -> RIO r e (Maybe (Socket TCP))
socket msg = liftEffect (OM.socket msg)
