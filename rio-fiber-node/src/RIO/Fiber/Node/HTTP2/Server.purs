-- | RIO-flavoured wrappers around `Node.Http2.Server`.
-- |
-- | An `Http2SecureServer` is a value (a handle on the listening
-- | TLS / HTTP/2 server), not a capability, so the `Effect`
-- | primitives are lifted directly into `RIO`. `toTlsServer` and
-- | the `toNetServer` convenience (which composes
-- | `toTlsServer` with `Node.TLS.Server.toTcpServer`) are
-- | re-exported so callers can drive the underlying TLS / TCP
-- | listener (e.g. `listenTcp` / `addressTcp` / `listeningH`)
-- | without reaching for `Node.Http2.Server` or `Node.TLS.Server`
-- | directly.
module RIO.Fiber.Node.HTTP2.Server
  ( module Exports
  , createSecureServer
  , setTimeout
  , timeout
  , updateSettings
  , toNetServer
  ) where

import Prelude

import Data.Time.Duration (Milliseconds)
import Node.Http2.Server
  ( checkContinueH
  , requestH
  , sessionErrorH
  , sessionH
  , streamH
  , timeoutH
  , toTlsServer
  , unknownProtocolH
  ) as Exports
import Node.Http2.Server as Srv
import Node.Http2.Types
  ( Http2CreateSecureServerOptions
  , Http2SecureServer
  , Settings
  )
import Node.Net.Types (NewServerOptions, Server, TCP)
import Node.TLS.Server (toTcpServer) as TlsSrv
import Node.TLS.Types
  ( CreateSecureContextOptions
  , TlsCreateServerOptions
  )
import Node.TLS.Types (Server) as TLS
import Prim.Row as Row

import RIO.Fiber.Core (RIO, liftEffect)

-- | Allocate a fresh HTTP/2 secure server.
createSecureServer
  :: forall r e opts trash
   . Row.Union opts trash
       ( Http2CreateSecureServerOptions
           ( TlsCreateServerOptions TLS.Server
               (CreateSecureContextOptions (NewServerOptions ()))
           )
       )
  => { | opts }
  -> RIO r e Http2SecureServer
createSecureServer opts = liftEffect (Srv.createSecureServer opts)

-- | Arm the server-wide inactivity timer.
setTimeout
  :: forall r e
   . Http2SecureServer
  -> Milliseconds
  -> RIO r e Http2SecureServer
setTimeout s ms = liftEffect (Srv.setTimeout s ms)

-- | The current server-wide inactivity timeout.
timeout
  :: forall r e. Http2SecureServer -> RIO r e Milliseconds
timeout s = liftEffect (Srv.timeout s)

-- | Push new HTTP/2 settings to every active session on this
-- | server.
updateSettings
  :: forall r e
   . Http2SecureServer
  -> Settings
  -> RIO r e Unit
updateSettings s set = liftEffect (Srv.updateSettings s set)

-- | Reach the underlying TCP `Server` so callers can drive its
-- | listening / address / close primitives. Composes
-- | `Node.Http2.Server.toTlsServer` with
-- | `Node.TLS.Server.toTcpServer`.
toNetServer :: Http2SecureServer -> Server TCP
toNetServer = TlsSrv.toTcpServer <<< Srv.toTlsServer
