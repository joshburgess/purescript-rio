-- | RIO-flavoured wrappers around `Node.Net.Socket`.
-- |
-- | A `Socket connectionType` is a value (a handle on a TCP or
-- | IPC connection), not a capability, so this module mirrors the
-- | upstream surface by lifting each `Effect` operation into `RIO`
-- | and re-exporting the types and event handles unchanged.
module RIO.Aff.Node.Net.Socket
  ( module Exports
  , address
  , bytesRead
  , bytesWritten
  , clearTimeout
  , connectIpc
  , connectTcp
  , connecting
  , createConnectionIpc
  , createConnectionTCP
  , destroySoon
  , localAddress
  , localFamily
  , localPort
  , newIpc
  , newTcp
  , pending
  , readyState
  , ref
  , remoteAddress
  , remoteFamily
  , remotePort
  , resetAndDestroy
  , setKeepAlive
  , setKeepAliveAll
  , setKeepAliveBoolean
  , setKeepAliveInitialDelay
  , setNoDelay
  , setNoDelay'
  , setTimeout
  , timeout
  , unref
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Effect.Class (liftEffect)
import Node.Net.Socket
  ( closeH
  , connectH
  , lookupH
  , readyH
  , timeoutH
  , toDuplex
  , toEventEmitter
  ) as Exports
import Node.Net.Socket as Sock
import Node.Net.Types
  ( ConnectIpcOptions
  , ConnectTcpOptions
  , IPC
  , IpFamily
  , NewSocketOptions
  , Socket
  , SocketReadyState
  , TCP
  )
import Prim.Row as Row

import RIO.Aff.Core (RIO)

-- | Allocate a fresh, unconnected TCP socket.
newTcp
  :: forall r e opts trash
   . Row.Union opts trash (NewSocketOptions ())
  => { | opts }
  -> RIO r e (Socket TCP)
newTcp opts = liftEffect (Sock.newTcp opts)

-- | Allocate a fresh, unconnected IPC socket.
newIpc
  :: forall r e opts trash
   . Row.Union opts trash (NewSocketOptions ())
  => { | opts }
  -> RIO r e (Socket IPC)
newIpc opts = liftEffect (Sock.newIpc opts)

-- | Allocate a TCP socket and connect it. At minimum `port` must
-- | be specified.
createConnectionTCP
  :: forall r e opts trash
   . Row.Union opts trash (NewSocketOptions (ConnectTcpOptions ()))
  => { | opts }
  -> RIO r e (Socket TCP)
createConnectionTCP opts = liftEffect (Sock.createConnectionTCP opts)

-- | Allocate an IPC socket and connect it. At minimum `path` must
-- | be specified.
createConnectionIpc
  :: forall r e opts trash
   . Row.Union opts trash (NewSocketOptions (ConnectIpcOptions ()))
  => { | opts }
  -> RIO r e (Socket IPC)
createConnectionIpc opts = liftEffect (Sock.createConnectionIpc opts)

-- | Connect an existing TCP socket. See `ConnectTcpOptions` for
-- | the option row.
connectTcp
  :: forall r e opts trash
   . Row.Union opts trash (ConnectTcpOptions ())
  => Socket TCP
  -> { | opts }
  -> RIO r e (Socket TCP)
connectTcp sock opts = liftEffect (Sock.connectTcp sock opts)

-- | Connect an existing IPC socket to the given path.
connectIpc
  :: forall r e
   . Socket IPC
  -> String
  -> RIO r e (Socket IPC)
connectIpc sock path = liftEffect (Sock.connectIpc sock path)

-- | Whether the socket is in the middle of `connect`ing.
connecting :: forall r e c. Socket c -> RIO r e Boolean
connecting s = liftEffect (Sock.connecting s)

-- | Half-close the writable side once the queued data has been
-- | flushed.
destroySoon :: forall r e c. Socket c -> RIO r e Unit
destroySoon s = liftEffect (Sock.destroySoon s)

-- | The local end of the connection as a `{ port, family, address }`
-- | record.
address
  :: forall r e c
   . Socket c
  -> RIO r e { port :: Int, family :: IpFamily, address :: String }
address s = liftEffect (Sock.address s)

-- | The number of bytes read from the peer.
bytesRead :: forall r e c. Socket c -> RIO r e Int
bytesRead s = liftEffect (Sock.bytesRead s)

-- | The number of bytes written to the peer.
bytesWritten :: forall r e c. Socket c -> RIO r e Int
bytesWritten s = liftEffect (Sock.bytesWritten s)

-- | The local IP address of the connection.
localAddress :: forall r e c. Socket c -> RIO r e String
localAddress s = liftEffect (Sock.localAddress s)

-- | The local IP port of the connection.
localPort :: forall r e c. Socket c -> RIO r e Int
localPort s = liftEffect (Sock.localPort s)

-- | The local IP family of the connection.
localFamily :: forall r e c. Socket c -> RIO r e IpFamily
localFamily s = liftEffect (Sock.localFamily s)

-- | Whether the socket is still pending (i.e. `connect` has not
-- | yet completed).
pending :: forall r e c. Socket c -> RIO r e Boolean
pending s = liftEffect (Sock.pending s)

-- | Re-attach the socket to the event loop.
ref :: forall r e c. Socket c -> RIO r e Unit
ref s = liftEffect (Sock.ref s)

-- | The remote IP address of the connection.
remoteAddress :: forall r e c. Socket c -> RIO r e String
remoteAddress s = liftEffect (Sock.remoteAddress s)

-- | The remote IP port of the connection.
remotePort :: forall r e c. Socket c -> RIO r e Int
remotePort s = liftEffect (Sock.remotePort s)

-- | The remote IP family of the connection.
remoteFamily :: forall r e c. Socket c -> RIO r e IpFamily
remoteFamily s = liftEffect (Sock.remoteFamily s)

-- | Send an RST packet and destroy the socket.
resetAndDestroy :: forall r e. Socket TCP -> RIO r e Unit
resetAndDestroy s = liftEffect (Sock.resetAndDestroy s)

-- | Enable keep-alive with default settings.
setKeepAlive :: forall r e. Socket TCP -> RIO r e Unit
setKeepAlive s = liftEffect (Sock.setKeepAlive s)

-- | Enable / disable keep-alive.
setKeepAliveBoolean
  :: forall r e
   . Socket TCP
  -> Boolean
  -> RIO r e Unit
setKeepAliveBoolean s b = liftEffect (Sock.setKeepAliveBoolean s b)

-- | Set the initial idle delay before the first probe.
setKeepAliveInitialDelay
  :: forall r e
   . Socket TCP
  -> Int
  -> RIO r e Unit
setKeepAliveInitialDelay s ms =
  liftEffect (Sock.setKeepAliveInitialDelay s ms)

-- | Enable / disable keep-alive and set the initial idle delay in
-- | a single call.
setKeepAliveAll
  :: forall r e
   . Socket TCP
  -> Boolean
  -> Int
  -> RIO r e Unit
setKeepAliveAll s b ms = liftEffect (Sock.setKeepAliveAll s b ms)

-- | Disable Nagle's algorithm.
setNoDelay :: forall r e. Socket TCP -> RIO r e Unit
setNoDelay s = liftEffect (Sock.setNoDelay s)

-- | Enable / disable Nagle's algorithm.
setNoDelay' :: forall r e. Socket TCP -> Boolean -> RIO r e Unit
setNoDelay' s b = liftEffect (Sock.setNoDelay' s b)

-- | Arm the inactivity timer. `setTimeout 0` disables it.
setTimeout :: forall r e c. Socket c -> Milliseconds -> RIO r e Unit
setTimeout s ms = liftEffect (Sock.setTimeout s ms)

-- | Disable the inactivity timer.
clearTimeout :: forall r e c. Socket c -> RIO r e Unit
clearTimeout s = liftEffect (Sock.clearTimeout s)

-- | The current inactivity timeout, if one is armed.
timeout :: forall r e c. Socket c -> RIO r e (Maybe Milliseconds)
timeout s = liftEffect (Sock.timeout s)

-- | Detach the socket from the event loop so the process can
-- | exit even while the socket is open.
unref :: forall r e c. Socket c -> RIO r e Unit
unref s = liftEffect (Sock.unref s)

-- | The current ready state of the socket.
readyState :: forall r e c. Socket c -> RIO r e SocketReadyState
readyState s = liftEffect (Sock.readyState s)
