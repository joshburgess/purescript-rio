-- | RIO-flavoured wrappers around `Node.HTTP.ServerResponse`.
-- |
-- | A `ServerResponse` is a value (a handle on an outgoing HTTP
-- | response), not a capability, so this module mirrors the
-- | upstream surface by lifting each `Effect`-valued primitive
-- | into `RIO` and re-exporting the event handles and pure
-- | accessors unchanged.
module RIO.Fiber.Node.HTTP.ServerResponse
  ( module Exports
  , sendDate
  , setSendDate
  , setStatusCode
  , setStatusMessage
  , setStrictContentLength
  , statusCode
  , statusMessage
  , strictContentLength
  , writeEarlyHints
  , writeEarlyHints'
  , writeHead
  , writeHead'
  , writeHeadHeaders
  , writeHeadMsgHeaders
  , writeProcessing
  ) where

import Prelude

import Effect (Effect)
import Node.HTTP.ServerResponse
  ( closeH
  , finishH
  , req
  , toOutgoingMessage
  ) as Exports
import Node.HTTP.ServerResponse as SR
import Node.HTTP.Types (ServerResponse)

import RIO.Fiber.Core (RIO, liftEffect)

-- | Whether the response will send a `Date` header.
sendDate :: forall r e. ServerResponse -> RIO r e Boolean
sendDate sr = liftEffect (SR.sendDate sr)

-- | Toggle whether the response sends a `Date` header.
setSendDate :: forall r e. Boolean -> ServerResponse -> RIO r e Unit
setSendDate b sr = liftEffect (SR.setSendDate b sr)

-- | The current status code.
statusCode :: forall r e. ServerResponse -> RIO r e Int
statusCode sr = liftEffect (SR.statusCode sr)

-- | Set the status code.
setStatusCode :: forall r e. Int -> ServerResponse -> RIO r e Unit
setStatusCode n sr = liftEffect (SR.setStatusCode n sr)

-- | The current status message.
statusMessage :: forall r e. ServerResponse -> RIO r e String
statusMessage sr = liftEffect (SR.statusMessage sr)

-- | Set the status message.
setStatusMessage
  :: forall r e. String -> ServerResponse -> RIO r e Unit
setStatusMessage m sr = liftEffect (SR.setStatusMessage m sr)

-- | Whether the response enforces a strict `Content-Length`.
strictContentLength
  :: forall r e. ServerResponse -> RIO r e Boolean
strictContentLength sr = liftEffect (SR.strictContentLength sr)

-- | Toggle strict `Content-Length` enforcement.
setStrictContentLength
  :: forall r e. Boolean -> ServerResponse -> RIO r e Unit
setStrictContentLength b sr =
  liftEffect (SR.setStrictContentLength b sr)

-- | Write an HTTP/1.1 103 Early Hints response with the given
-- | hint headers.
writeEarlyHints
  :: forall r e h
   . { | h }
  -> ServerResponse
  -> RIO r e Unit
writeEarlyHints hints sr =
  liftEffect (SR.writeEarlyHints hints sr)

-- | `writeEarlyHints` with a completion callback.
writeEarlyHints'
  :: forall r e h
   . { | h }
  -> Effect Unit
  -> ServerResponse
  -> RIO r e Unit
writeEarlyHints' hints cb sr =
  liftEffect (SR.writeEarlyHints' hints cb sr)

-- | Send the head with just a status code.
writeHead :: forall r e. Int -> ServerResponse -> RIO r e Unit
writeHead n sr = liftEffect (SR.writeHead n sr)

-- | Send the head with a status code and status message.
writeHead'
  :: forall r e
   . Int
  -> String
  -> ServerResponse
  -> RIO r e Unit
writeHead' n m sr = liftEffect (SR.writeHead' n m sr)

-- | Send the head with a status code and explicit header record.
writeHeadHeaders
  :: forall r e h
   . Int
  -> { | h }
  -> ServerResponse
  -> RIO r e Unit
writeHeadHeaders n hdrs sr =
  liftEffect (SR.writeHeadHeaders n hdrs sr)

-- | Send the head with a status code, status message, and
-- | explicit header record.
writeHeadMsgHeaders
  :: forall r e h
   . Int
  -> String
  -> { | h }
  -> ServerResponse
  -> RIO r e Unit
writeHeadMsgHeaders n m hdrs sr =
  liftEffect (SR.writeHeadMsgHeaders n m hdrs sr)

-- | Send an HTTP/1.1 102 Processing response.
writeProcessing :: forall r e. ServerResponse -> RIO r e Unit
writeProcessing sr = liftEffect (SR.writeProcessing sr)
