-- | RIO-flavoured wrappers around `Node.HTTP.ClientRequest`.
-- |
-- | A `ClientRequest` is a value (a handle on an in-flight HTTP
-- | request), not a capability, so this module mirrors the
-- | upstream surface by lifting each `Effect`-valued primitive
-- | into `RIO` and re-exporting the event handles and pure
-- | accessors unchanged.
module RIO.Aff.Node.HTTP.ClientRequest
  ( module Exports
  , setNoDelay
  , setSocketKeepAlive
  , setTimeout
  ) where

import Prelude

import Data.Time.Duration (Milliseconds)
import Effect.Class (liftEffect)
import Node.HTTP.ClientRequest
  ( closeH
  , connectH
  , continueH
  , finishH
  , host
  , informationH
  , method
  , path
  , protocol
  , responseH
  , reusedSocket
  , socketH
  , timeoutH
  , toOutgoingMessage
  , upgradeH
  ) as Exports
import Node.HTTP.ClientRequest as CR
import Node.HTTP.Types (ClientRequest)

import RIO.Aff.Core (RIO)

-- | Enable / disable Nagle's algorithm on the underlying socket.
setNoDelay :: forall r e. Boolean -> ClientRequest -> RIO r e Unit
setNoDelay b cr = liftEffect (CR.setNoDelay b cr)

-- | Enable / disable keep-alive on the underlying socket and set
-- | the initial delay.
setSocketKeepAlive
  :: forall r e
   . Boolean
  -> Milliseconds
  -> ClientRequest
  -> RIO r e Unit
setSocketKeepAlive b ms cr =
  liftEffect (CR.setSocketKeepAlive b ms cr)

-- | Arm the inactivity timer.
setTimeout
  :: forall r e
   . Milliseconds
  -> ClientRequest
  -> RIO r e Unit
setTimeout ms cr = liftEffect (CR.setTimeout ms cr)
