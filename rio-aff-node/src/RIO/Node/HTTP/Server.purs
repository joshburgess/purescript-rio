-- | RIO-flavoured wrappers around `Node.HTTP.Server`.
-- |
-- | An `HttpServer' tt` is a value (a handle on an HTTP server),
-- | not a capability, so this module mirrors the upstream surface
-- | by lifting each `Effect`-valued primitive into `RIO`. The
-- | `toTlsServer` conversion is omitted because it would force a
-- | direct dependency on `node-tls`; callers who need it can still
-- | reach for `Node.HTTP.Server.toTlsServer` directly.
module RIO.Aff.Node.HTTP.Server
  ( module Exports
  , clearKeepAliveTimeout
  , clearTimeout
  , closeAllConnections
  , closeIdleConnections
  , headersTimeout
  , keepAliveTimeout
  , maxHeadersCount
  , maxRequestsPerSocket
  , requestTimeout
  , setHeadersTimeout
  , setKeepAliveTimeout
  , setMaxHeadersCount
  , setMaxRequestsPerSocket
  , setRequestTimeout
  , setTimeout
  , setUnlimitedHeadersCount
  , setUnlimitedRequestsPerSocket
  , timeout
  ) where

import Prelude

import Data.Time.Duration (Milliseconds)
import Effect.Class (liftEffect)
import Node.HTTP.Server
  ( ClientErrorException
  , bytesParsed
  , checkContinueH
  , checkExpectationH
  , clientErrorH
  , closeH
  , connectH
  , connectionH
  , dropRequestH
  , rawPacket
  , requestH
  , toError
  , toNetServer
  , upgradeH
  ) as Exports
import Node.HTTP.Server as Srv
import Node.HTTP.Types (HttpServer')

import RIO.Aff.Core (RIO)

-- | Close every connection currently held by the server, even
-- | those still serving requests.
closeAllConnections
  :: forall r e tt. HttpServer' tt -> RIO r e Unit
closeAllConnections hs = liftEffect (Srv.closeAllConnections hs)

-- | Close every idle keep-alive connection.
closeIdleConnections
  :: forall r e tt. HttpServer' tt -> RIO r e Unit
closeIdleConnections hs = liftEffect (Srv.closeIdleConnections hs)

-- | The current `headersTimeout` (in ms).
headersTimeout
  :: forall r e tt. HttpServer' tt -> RIO r e Int
headersTimeout hs = liftEffect (Srv.headersTimeout hs)

-- | Set the `headersTimeout` (in ms).
setHeadersTimeout
  :: forall r e tt
   . Int
  -> HttpServer' tt
  -> RIO r e Unit
setHeadersTimeout n hs = liftEffect (Srv.setHeadersTimeout n hs)

-- | The current `maxHeadersCount`.
maxHeadersCount
  :: forall r e tt. HttpServer' tt -> RIO r e Int
maxHeadersCount hs = liftEffect (Srv.maxHeadersCount hs)

-- | Set `maxHeadersCount`.
setMaxHeadersCount
  :: forall r e tt
   . Int
  -> HttpServer' tt
  -> RIO r e Unit
setMaxHeadersCount n hs = liftEffect (Srv.setMaxHeadersCount n hs)

-- | Remove the cap on the number of headers Node will accept.
setUnlimitedHeadersCount
  :: forall r e tt. HttpServer' tt -> RIO r e Unit
setUnlimitedHeadersCount hs =
  liftEffect (Srv.setUnlimitedHeadersCount hs)

-- | The current `requestTimeout`.
requestTimeout
  :: forall r e tt. HttpServer' tt -> RIO r e Milliseconds
requestTimeout hs = liftEffect (Srv.requestTimeout hs)

-- | Set `requestTimeout`.
setRequestTimeout
  :: forall r e tt
   . Milliseconds
  -> HttpServer' tt
  -> RIO r e Unit
setRequestTimeout ms hs = liftEffect (Srv.setRequestTimeout ms hs)

-- | The current `maxRequestsPerSocket` cap.
maxRequestsPerSocket
  :: forall r e tt. HttpServer' tt -> RIO r e Int
maxRequestsPerSocket hs = liftEffect (Srv.maxRequestsPerSocket hs)

-- | Set `maxRequestsPerSocket`.
setMaxRequestsPerSocket
  :: forall r e tt
   . Int
  -> HttpServer' tt
  -> RIO r e Unit
setMaxRequestsPerSocket n hs =
  liftEffect (Srv.setMaxRequestsPerSocket n hs)

-- | Remove the per-socket request cap.
setUnlimitedRequestsPerSocket
  :: forall r e tt. HttpServer' tt -> RIO r e Unit
setUnlimitedRequestsPerSocket hs =
  liftEffect (Srv.setUnlimitedRequestsPerSocket hs)

-- | The current socket-inactivity timeout.
timeout :: forall r e tt. HttpServer' tt -> RIO r e Milliseconds
timeout hs = liftEffect (Srv.timeout hs)

-- | Arm the socket-inactivity timer.
setTimeout
  :: forall r e tt
   . Milliseconds
  -> HttpServer' tt
  -> RIO r e Unit
setTimeout ms hs = liftEffect (Srv.setTimeout ms hs)

-- | Disable the socket-inactivity timer.
clearTimeout :: forall r e tt. HttpServer' tt -> RIO r e Unit
clearTimeout hs = liftEffect (Srv.clearTimeout hs)

-- | The current keep-alive timeout.
keepAliveTimeout
  :: forall r e tt. HttpServer' tt -> RIO r e Milliseconds
keepAliveTimeout hs = liftEffect (Srv.keepAliveTimeout hs)

-- | Set the keep-alive timeout.
setKeepAliveTimeout
  :: forall r e tt
   . Milliseconds
  -> HttpServer' tt
  -> RIO r e Unit
setKeepAliveTimeout ms hs =
  liftEffect (Srv.setKeepAliveTimeout ms hs)

-- | Disable the keep-alive timeout.
clearKeepAliveTimeout
  :: forall r e tt. HttpServer' tt -> RIO r e Unit
clearKeepAliveTimeout hs =
  liftEffect (Srv.clearKeepAliveTimeout hs)
