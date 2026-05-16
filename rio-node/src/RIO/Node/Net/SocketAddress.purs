-- | RIO-flavoured wrappers around `Node.Net.SocketAddress`.
-- |
-- | A `SocketAddress` is a value (an immutable address record)
-- | rather than a capability, so the constructors are lifted into
-- | `RIO` and the read-only accessors are re-exported as pure
-- | functions.
module RIO.Node.Net.SocketAddress
  ( module Exports
  , newIpv4
  , newIpv6
  ) where

import Effect.Class (liftEffect)
import Node.Net.SocketAddress
  ( Ipv4SocketAddressOptions
  , Ipv6SocketAddressOptions
  , address
  , family
  , flowLabel
  , port
  ) as Exports
import Node.Net.SocketAddress
  ( Ipv4SocketAddressOptions
  , Ipv6SocketAddressOptions
  )
import Node.Net.SocketAddress as SA
import Node.Net.Types (IPv4, IPv6, SocketAddress)

import RIO.Core (RIO)

-- | Allocate a new IPv4 socket address.
newIpv4
  :: forall r e
   . Ipv4SocketAddressOptions
  -> RIO r e (SocketAddress IPv4)
newIpv4 opts = liftEffect (SA.newIpv4 opts)

-- | Allocate a new IPv6 socket address.
newIpv6
  :: forall r e
   . Ipv6SocketAddressOptions
  -> RIO r e (SocketAddress IPv6)
newIpv6 opts = liftEffect (SA.newIpv6 opts)
