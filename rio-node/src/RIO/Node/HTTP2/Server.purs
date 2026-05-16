-- | RIO-flavoured wrappers around `Node.Http2.Server`.
-- |
-- | An `Http2SecureServer` is a value (a handle on the listening
-- | TLS / HTTP/2 server), not a capability, so the `Effect`
-- | primitives are lifted directly into `RIO`. `toTlsServer` is
-- | intentionally omitted to avoid forcing a direct dependency
-- | on `node-tls`; the constructor `createSecureServer` is
-- | already typed against the TLS option row, so the dependency
-- | comes in transitively for callers who actually configure a
-- | server.
module RIO.Node.HTTP2.Server
  ( module Exports
  , createSecureServer
  , setTimeout
  , timeout
  , updateSettings
  ) where

import Prelude

import Data.Time.Duration (Milliseconds)
import Effect.Class (liftEffect)
import Node.Http2.Server
  ( checkContinueH
  , requestH
  , sessionErrorH
  , sessionH
  , streamH
  , timeoutH
  , unknownProtocolH
  ) as Exports
import Node.Http2.Server as Srv
import Node.Http2.Types
  ( Http2CreateSecureServerOptions
  , Http2SecureServer
  , Settings
  )
import Node.Net.Types (NewServerOptions)
import Node.TLS.Types
  ( CreateSecureContextOptions
  , Server
  , TlsCreateServerOptions
  )
import Prim.Row as Row

import RIO.Core (RIO)

-- | Allocate a fresh HTTP/2 secure server.
createSecureServer
  :: forall r e opts trash
   . Row.Union opts trash
       ( Http2CreateSecureServerOptions
           ( TlsCreateServerOptions Server
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
