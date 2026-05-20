-- | RIO-flavoured wrappers around `Node.Http2.Client`.
module RIO.Aff.Node.HTTP2.Client
  ( connect
  , connect'
  ) where

import Effect.Class (liftEffect)
import Node.Http2.Client as C
import Node.Http2.Types (Http2ClientConnectOptions, Http2Session)
import Node.Net.Types (ConnectTcpOptions)
import Node.TLS.Types
  ( Client
  , ConnectTlsSocketOptions
  , CreateSecureContextOptions
  )
import Prim.Row as Row

import RIO.Aff.Core (RIO)

-- | Connect to the given authority (e.g. "https://example.com").
connect :: forall r e. String -> RIO r e (Http2Session Client)
connect a = liftEffect (C.connect a)

-- | Connect with explicit options.
connect'
  :: forall r e opts trash
   . Row.Union opts trash
       ( Http2ClientConnectOptions
           ( ConnectTlsSocketOptions Client
               (CreateSecureContextOptions (ConnectTcpOptions ()))
           )
       )
  => String
  -> { | opts }
  -> RIO r e (Http2Session Client)
connect' a o = liftEffect (C.connect' a o)
