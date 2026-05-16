-- | RIO-flavoured wrappers around `Node.Http2.Session`.
-- |
-- | An `Http2Session endpoint` is a value (a handle on a live
-- | HTTP/2 connection), not a capability, so every `Effect`
-- | operation is lifted directly into `RIO`. Pure accessors
-- | (`originSet`, `type_`) are re-exported unchanged.
module RIO.Node.HTTP2.Session
  ( module Exports
  , Http2SessionState
  , RequestOptions
  , alpnProtocol
  , altsvcOrigin
  , altsvcStreamId
  , close
  , closed
  , connecting
  , destroy
  , destroyWithCode
  , destroyWithError
  , destroyWithErrorCode
  , destroyed
  , encrypted
  , goAway
  , goAwayCode
  , goAwayCodeLastStreamId
  , goAwayCodeLastStreamIdData
  , localSettings
  , origin
  , pendingSettingsAck
  , ping
  , pingPayload
  , ref
  , remoteSettings
  , request
  , request'
  , setLocalWindowSize
  , setTimeout
  , settings
  , socket
  , state
  , unref
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Buffer (Buffer)
import Node.Http2.ErrorCode (ErrorCode)
import Node.Http2.Session
  ( Http2SessionState
  , RequestOptions
  ) as RawSes
import Node.Http2.Session
  ( altsvcHandle
  , closeH
  , connectH
  , errorH
  , frameErrorH
  , goAwayH
  , localSettingsH
  , originHandle
  , originSet
  , pingH
  , remoteSettingsH
  , streamH
  , timeoutH
  , toEventEmitter
  , type_
  ) as Exports
import Node.Http2.Session as Ses
import Node.Http2.Types
  ( Headers
  , Http2Session
  , Http2Stream
  , Settings
  , StreamId
  )
import Node.Net.Types (Socket, TCP)
import Node.TLS.Types (Client, Server)

import RIO.Core (RIO)

type Http2SessionState = RawSes.Http2SessionState

type RequestOptions = RawSes.RequestOptions

-- | The ALPN protocol negotiated for this session, if any.
alpnProtocol
  :: forall r e ep. Http2Session ep -> RIO r e (Maybe String)
alpnProtocol s = liftEffect (Ses.alpnProtocol s)

-- | Send a graceful close.
close :: forall r e ep. Http2Session ep -> RIO r e Unit
close s = liftEffect (Ses.close s)

-- | Whether the session has been closed.
closed :: forall r e ep. Http2Session ep -> RIO r e Boolean
closed s = liftEffect (Ses.closed s)

-- | Whether the session is still connecting.
connecting :: forall r e ep. Http2Session ep -> RIO r e Boolean
connecting s = liftEffect (Ses.connecting s)

-- | Destroy the session immediately.
destroy :: forall r e ep. Http2Session ep -> RIO r e Unit
destroy s = liftEffect (Ses.destroy s)

-- | Destroy the session with an associated `Error`.
destroyWithError
  :: forall r e ep. Http2Session ep -> Error -> RIO r e Unit
destroyWithError s err = liftEffect (Ses.destroyWithError s err)

-- | Destroy the session with an HTTP/2 error code.
destroyWithCode
  :: forall r e ep. Http2Session ep -> ErrorCode -> RIO r e Unit
destroyWithCode s c = liftEffect (Ses.destroyWithCode s c)

-- | Destroy the session with both an `Error` and an HTTP/2 error
-- | code.
destroyWithErrorCode
  :: forall r e ep
   . Http2Session ep
  -> Error
  -> ErrorCode
  -> RIO r e Unit
destroyWithErrorCode s err c =
  liftEffect (Ses.destroyWithErrorCode s err c)

-- | Whether the session has been destroyed.
destroyed
  :: forall r e ep. Http2Session ep -> RIO r e Boolean
destroyed s = liftEffect (Ses.destroyed s)

-- | Whether the underlying socket is encrypted.
encrypted
  :: forall r e ep. Http2Session ep -> RIO r e (Maybe Boolean)
encrypted s = liftEffect (Ses.encrypted s)

-- | Send a GOAWAY frame with no payload.
goAway :: forall r e ep. Http2Session ep -> RIO r e Unit
goAway s = liftEffect (Ses.goAway s)

-- | Send a GOAWAY frame with an error code.
goAwayCode
  :: forall r e ep. Http2Session ep -> ErrorCode -> RIO r e Unit
goAwayCode s c = liftEffect (Ses.goAwayCode s c)

-- | Send a GOAWAY frame with an error code and last-stream-id.
goAwayCodeLastStreamId
  :: forall r e ep
   . Http2Session ep
  -> ErrorCode
  -> StreamId
  -> RIO r e Unit
goAwayCodeLastStreamId s c lsi =
  liftEffect (Ses.goAwayCodeLastStreamId s c lsi)

-- | Send a GOAWAY frame with an error code, last-stream-id, and
-- | opaque data buffer.
goAwayCodeLastStreamIdData
  :: forall r e ep
   . Http2Session ep
  -> ErrorCode
  -> StreamId
  -> Buffer
  -> RIO r e Unit
goAwayCodeLastStreamIdData s c lsi buf =
  liftEffect (Ses.goAwayCodeLastStreamIdData s c lsi buf)

-- | The local settings sent on this session.
localSettings
  :: forall r e ep. Http2Session ep -> RIO r e Settings
localSettings s = liftEffect (Ses.localSettings s)

-- | Whether a SETTINGS ack is outstanding.
pendingSettingsAck
  :: forall r e ep. Http2Session ep -> RIO r e Boolean
pendingSettingsAck s = liftEffect (Ses.pendingSettingsAck s)

-- | Send a PING frame.
ping
  :: forall r e ep
   . Http2Session ep
  -> (Maybe Error -> Milliseconds -> Buffer -> Effect Unit)
  -> RIO r e Boolean
ping s cb = liftEffect (Ses.ping s cb)

-- | Send a PING frame with an explicit payload.
pingPayload
  :: forall r e ep
   . Http2Session ep
  -> Buffer
  -> (Maybe Error -> Milliseconds -> Buffer -> Effect Unit)
  -> RIO r e Boolean
pingPayload s buf cb = liftEffect (Ses.pingPayload s buf cb)

-- | Re-attach the session's socket to the event loop.
ref :: forall r e ep. Http2Session ep -> RIO r e (Socket TCP)
ref s = liftEffect (Ses.ref s)

-- | The remote settings the peer announced.
remoteSettings
  :: forall r e ep. Http2Session ep -> RIO r e Settings
remoteSettings s = liftEffect (Ses.remoteSettings s)

-- | Adjust the local flow-control window size.
setLocalWindowSize
  :: forall r e ep
   . Http2Session ep
  -> Int
  -> RIO r e Unit
setLocalWindowSize s sz =
  liftEffect (Ses.setLocalWindowSize s sz)

-- | Arm the inactivity timer with a callback.
setTimeout
  :: forall r e ep
   . Http2Session ep
  -> Milliseconds
  -> Effect Unit
  -> RIO r e Unit
setTimeout s ms cb = liftEffect (Ses.setTimeout s ms cb)

-- | Negotiate new settings.
settings
  :: forall r e ep
   . Http2Session ep
  -> Settings
  -> (Maybe Error -> Settings -> Int -> Effect Unit)
  -> RIO r e Unit
settings s set cb = liftEffect (Ses.settings s set cb)

-- | The TCP socket the session is using.
socket :: forall r e ep. Http2Session ep -> RIO r e (Socket TCP)
socket s = liftEffect (Ses.socket s)

-- | Detailed session-state numbers.
state :: forall r e ep. Http2Session ep -> RIO r e Http2SessionState
state s = liftEffect (Ses.state s)

-- | Detach the session's socket from the event loop.
unref :: forall r e ep. Http2Session ep -> RIO r e (Socket TCP)
unref s = liftEffect (Ses.unref s)

-- | Server-side: send an ALTSVC with an explicit stream id.
altsvcStreamId
  :: forall r e
   . Http2Session Server
  -> String
  -> Int
  -> RIO r e Unit
altsvcStreamId s alt sid =
  liftEffect (Ses.altsvcStreamId s alt sid)

-- | Server-side: send an ALTSVC for the given origin.
altsvcOrigin
  :: forall r e
   . Http2Session Server
  -> String
  -> String
  -> RIO r e Unit
altsvcOrigin s alt o = liftEffect (Ses.altsvcOrigin s alt o)

-- | Server-side: send an ORIGIN frame.
origin
  :: forall r e
   . Http2Session Server
  -> Array String
  -> RIO r e Unit
origin s os = liftEffect (Ses.origin s os)

-- | Client-side: open a new request stream.
request
  :: forall r e
   . Http2Session Client
  -> Headers
  -> RIO r e (Http2Stream Client)
request s h = liftEffect (Ses.request s h)

-- | Client-side: open a new request stream with explicit
-- | request options.
request'
  :: forall r e
   . Http2Session Client
  -> Headers
  -> RequestOptions
  -> RIO r e (Http2Stream Client)
request' s h o = liftEffect (Ses.request' s h o)
