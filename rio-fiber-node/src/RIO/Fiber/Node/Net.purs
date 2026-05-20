-- | RIO-flavoured top-level wrappers around `Node.Net`.
-- |
-- | All four entry points from `Node.Net` are pure (they classify
-- | a `String` as IPv4 / IPv6 / not-an-IP without touching the
-- | network), so this module re-exports them unchanged. The
-- | service-shaped surface lives in the `RIO.Fiber.Node.Net.Socket`,
-- | `RIO.Fiber.Node.Net.Server`, `RIO.Fiber.Node.Net.BlockList`, and
-- | `RIO.Fiber.Node.Net.SocketAddress` submodules.
module RIO.Fiber.Node.Net
  ( module Exports
  ) where

import Node.Net
  ( isIP
  , isIP'
  , isIPv4
  , isIPv6
  ) as Exports
import Node.Net.Types
  ( BlockList
  , ConnectIpcOptions
  , ConnectTcpOptions
  , ConnectionType
  , IPC
  , IPv4
  , IPv6
  , IpFamily(..)
  , ListenIpcOptions
  , ListenTcpOptions
  , NewServerOptions
  , NewSocketOptions
  , Server
  , Socket
  , SocketAddress
  , SocketReadyState(..)
  , TCP
  , familyIpv4
  , familyIpv4And6
  , familyIpv6
  , socketReadyStateToNode
  , toNodeIpFamily
  , unsafeFromNodeIpFamily
  ) as Exports
