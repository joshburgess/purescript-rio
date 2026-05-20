-- | RIO-flavoured wrappers around `Node.Net.BlockList`.
-- |
-- | A `BlockList` is a mutable value (a handle on a list of
-- | address rules), not a capability, so this module simply lifts
-- | each `Effect` operation into `RIO`. The upstream module does
-- | not export a constructor; once one is in hand (typically from
-- | a Node.js API that hands you a fresh `BlockList`), every
-- | mutator and query is surfaced here.
module RIO.Fiber.Node.Net.BlockList
  ( addAddressAddr
  , addAddressStr
  , addRangeAddrAddr
  , addRangeAddrStr
  , addRangeStrAddr
  , addRangeStrStr
  , addSubnetAddr
  , addSubnetStr
  , checkAddr
  , checkStr
  , rules
  ) where

import Prelude

import Node.Net.BlockList as BL
import Node.Net.Types (BlockList, IpFamily, SocketAddress)

import RIO.Fiber.Core (RIO, liftEffect)

-- | Add a `SocketAddress` to the block list.
addAddressAddr
  :: forall r e ipFamily
   . BlockList
  -> SocketAddress ipFamily
  -> IpFamily
  -> RIO r e Unit
addAddressAddr bl a ty = liftEffect (BL.addAddressAddr bl a ty)

-- | Add an address (given as a string) to the block list.
addAddressStr
  :: forall r e
   . BlockList
  -> String
  -> IpFamily
  -> RIO r e Unit
addAddressStr bl a ty = liftEffect (BL.addAddressStr bl a ty)

-- | Add a string / string IP range to the block list.
addRangeStrStr
  :: forall r e
   . BlockList
  -> String
  -> String
  -> IpFamily
  -> RIO r e Unit
addRangeStrStr bl start end ty =
  liftEffect (BL.addRangeStrStr bl start end ty)

-- | Add a string / `SocketAddress` IP range to the block list.
addRangeStrAddr
  :: forall r e ipFamily
   . BlockList
  -> String
  -> SocketAddress ipFamily
  -> IpFamily
  -> RIO r e Unit
addRangeStrAddr bl start end ty =
  liftEffect (BL.addRangeStrAddr bl start end ty)

-- | Add a `SocketAddress` / string IP range to the block list.
addRangeAddrStr
  :: forall r e ipFamily
   . BlockList
  -> String
  -> SocketAddress ipFamily
  -> IpFamily
  -> RIO r e Unit
addRangeAddrStr bl start end ty =
  liftEffect (BL.addRangeAddrStr bl start end ty)

-- | Add a `SocketAddress` / `SocketAddress` IP range to the
-- | block list.
addRangeAddrAddr
  :: forall r e ipFamilyStart ipFamilyEnd
   . BlockList
  -> SocketAddress ipFamilyStart
  -> SocketAddress ipFamilyEnd
  -> IpFamily
  -> RIO r e Unit
addRangeAddrAddr bl start end ty =
  liftEffect (BL.addRangeAddrAddr bl start end ty)

-- | Add a subnet (string form) to the block list.
addSubnetStr
  :: forall r e
   . BlockList
  -> String
  -> Int
  -> IpFamily
  -> RIO r e Unit
addSubnetStr bl net prefix ty =
  liftEffect (BL.addSubnetStr bl net prefix ty)

-- | Add a subnet (`SocketAddress` form) to the block list.
addSubnetAddr
  :: forall r e ipFamily
   . BlockList
  -> SocketAddress ipFamily
  -> Int
  -> IpFamily
  -> RIO r e Unit
addSubnetAddr bl net prefix ty =
  liftEffect (BL.addSubnetAddr bl net prefix ty)

-- | Check whether a string address is blocked.
checkStr
  :: forall r e
   . BlockList
  -> String
  -> IpFamily
  -> RIO r e Boolean
checkStr bl a ty = liftEffect (BL.checkStr bl a ty)

-- | Check whether a `SocketAddress` is blocked.
checkAddr
  :: forall r e ipFamily
   . BlockList
  -> SocketAddress ipFamily
  -> IpFamily
  -> RIO r e Boolean
checkAddr bl a ty = liftEffect (BL.checkAddr bl a ty)

-- | The current rule strings the block list is enforcing.
rules :: forall r e. BlockList -> RIO r e (Array String)
rules bl = liftEffect (BL.rules bl)
