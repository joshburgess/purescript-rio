-- | RIO-flavoured wrappers around `Node.Net.Server`.
-- |
-- | A `Server connectionType` is a value (a handle on a listening
-- | TCP or IPC socket), not a capability, so this module mirrors
-- | the upstream surface by lifting each `Effect` operation into
-- | `RIO` and re-exporting types and event handles unchanged.
module RIO.Aff.Node.Net.Server
  ( module Exports
  , addressIpc
  , addressTcp
  , close
  , createIpcServer
  , createIpcServer'
  , createTcpServer
  , createTcpServer'
  , getConnections
  , listenIpc
  , listenTcp
  , listening
  , maxConnections
  , ref
  , unref
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Node.Net.Server
  ( closeH
  , connectionH
  , dropHandleIpc
  , dropHandleTcp
  , errorH
  , listeningH
  , toEventEmitter
  ) as Exports
import Node.Net.Server as Srv
import Node.Net.Types
  ( IPC
  , IpFamily
  , ListenIpcOptions
  , ListenTcpOptions
  , NewServerOptions
  , Server
  , TCP
  )
import Prim.Row as Row

import RIO.Aff.Core (RIO)

-- | Allocate a fresh, unbound TCP server.
createTcpServer :: forall r e. RIO r e (Server TCP)
createTcpServer = liftEffect Srv.createTcpServer

-- | Allocate a fresh, unbound IPC server.
createIpcServer :: forall r e. RIO r e (Server IPC)
createIpcServer = liftEffect Srv.createIpcServer

-- | `createTcpServer` with `NewServerOptions`.
createTcpServer'
  :: forall r e opts trash
   . Row.Union opts trash (NewServerOptions ())
  => { | opts }
  -> RIO r e (Server TCP)
createTcpServer' opts = liftEffect (Srv.createTcpServer' opts)

-- | `createIpcServer` with `NewServerOptions`.
createIpcServer'
  :: forall r e opts trash
   . Row.Union opts trash (NewServerOptions ())
  => { | opts }
  -> RIO r e (Server IPC)
createIpcServer' opts = liftEffect (Srv.createIpcServer' opts)

-- | The bound address of a TCP server (once `listenTcp` has
-- | succeeded).
addressTcp
  :: forall r e
   . Server TCP
  -> RIO r e (Maybe { port :: Int, family :: IpFamily, address :: String })
addressTcp s = liftEffect (Srv.addressTcp s)

-- | The bound path of an IPC server (once `listenIpc` has
-- | succeeded).
addressIpc :: forall r e. Server IPC -> RIO r e (Maybe String)
addressIpc s = liftEffect (Srv.addressIpc s)

-- | Stop accepting new connections, then close the server once
-- | every active connection has closed.
close :: forall r e c. Server c -> RIO r e Unit
close s = liftEffect (Srv.close s)

-- | Ask the server how many connections are currently active. The
-- | callback fires with `(Error, count)` once Node has computed
-- | the answer.
getConnections
  :: forall r e c
   . Server c
  -> (Error -> Int -> Effect Unit)
  -> RIO r e Unit
getConnections s cb = liftEffect (Srv.getConnections s cb)

-- | Start listening for TCP connections.
listenTcp
  :: forall r e opts trash
   . Row.Union opts trash (ListenTcpOptions ())
  => Server TCP
  -> { | opts }
  -> RIO r e Unit
listenTcp s opts = liftEffect (Srv.listenTcp s opts)

-- | Start listening for IPC connections.
listenIpc
  :: forall r e opts trash
   . Row.Union opts trash (ListenIpcOptions ())
  => Server IPC
  -> { | opts }
  -> RIO r e Unit
listenIpc s opts = liftEffect (Srv.listenIpc s opts)

-- | Whether the server is currently accepting connections.
listening :: forall r e c. Server c -> RIO r e Boolean
listening s = liftEffect (Srv.listening s)

-- | The configured connection cap (a positive value imposes a
-- | hard cap; `0` is unlimited).
maxConnections :: forall r e c. Server c -> RIO r e Int
maxConnections s = liftEffect (Srv.maxConnections s)

-- | Re-attach the server to the event loop.
ref :: forall r e c. Server c -> RIO r e Unit
ref s = liftEffect (Srv.ref s)

-- | Detach the server from the event loop.
unref :: forall r e c. Server c -> RIO r e Unit
unref s = liftEffect (Srv.unref s)
