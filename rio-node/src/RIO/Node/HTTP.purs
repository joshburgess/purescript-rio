-- | RIO-flavoured wrappers around `Node.HTTP`.
-- |
-- | An `HttpServer` is a value (a handle on an HTTP server),
-- | not a capability, so this module mirrors the upstream
-- | surface by lifting each `Effect` operation into `RIO` and
-- | re-exporting the types unchanged.
module RIO.Node.HTTP
  ( module Exports
  , createServer
  , createServer'
  , get
  , getOpts
  , getUrl
  , get'
  , getUrl'
  , request
  , requestOpts
  , requestUrl
  , request'
  , requestURL'
  , setMaxIdleHttpParsers
  ) where

import Prelude

import Effect.Class (liftEffect)
import Node.HTTP (CreateServerOptions, RequestOptions, maxHeaderSize) as Exports
import Node.HTTP (CreateServerOptions, RequestOptions)
import Node.HTTP as HTTP
import Node.HTTP.Types
  ( ClientRequest
  , Encrypted
  , HttpServer
  , HttpServer'
  , HttpsServer
  , IMClientRequest
  , IMServer
  , IncomingMessage
  , IncomingMessageType
  , OutgoingMessage
  , PlainText
  , ServerResponse
  , TransmissionType
  ) as Exports
import Node.HTTP.Types (ClientRequest, HttpServer)
import Node.URL (URL)
import Prim.Row as Row

import RIO.Core (RIO)

-- | Allocate a fresh HTTP server.
createServer :: forall r e. RIO r e HttpServer
createServer = liftEffect HTTP.createServer

-- | `createServer` with `CreateServerOptions`.
createServer'
  :: forall r e opts trash
   . Row.Union opts trash CreateServerOptions
  => { | opts }
  -> RIO r e HttpServer
createServer' opts = liftEffect (HTTP.createServer' opts)

-- | Issue an HTTP request to the given URL string.
request :: forall r e. String -> RIO r e ClientRequest
request url = liftEffect (HTTP.request url)

-- | Issue an HTTP request to the given `URL`.
requestUrl :: forall r e. URL -> RIO r e ClientRequest
requestUrl url = liftEffect (HTTP.requestUrl url)

-- | Issue an HTTP request with explicit `RequestOptions`.
request'
  :: forall r e opts trash
   . Row.Union opts trash (RequestOptions ())
  => String
  -> { | opts }
  -> RIO r e ClientRequest
request' url opts = liftEffect (HTTP.request' url opts)

-- | Issue an HTTP request to the given `URL` with explicit
-- | `RequestOptions`.
requestURL'
  :: forall r e opts trash
   . Row.Union opts trash (RequestOptions ())
  => URL
  -> { | opts }
  -> RIO r e ClientRequest
requestURL' url opts = liftEffect (HTTP.requestURL' url opts)

-- | Issue an HTTP request from a `RequestOptions`-shaped record
-- | only (no URL).
requestOpts
  :: forall r e opts trash
   . Row.Union opts trash (RequestOptions ())
  => { | opts }
  -> RIO r e ClientRequest
requestOpts opts = liftEffect (HTTP.requestOpts opts)

-- | Issue an HTTP GET to the given URL string.
get :: forall r e. String -> RIO r e ClientRequest
get url = liftEffect (HTTP.get url)

-- | Issue an HTTP GET to the given `URL`.
getUrl :: forall r e. URL -> RIO r e ClientRequest
getUrl url = liftEffect (HTTP.getUrl url)

-- | Issue an HTTP GET with explicit options. `method` is forbidden.
get'
  :: forall r e opts trash
   . Row.Union opts trash (RequestOptions ())
  => Row.Lacks "method" opts
  => String
  -> { | opts }
  -> RIO r e ClientRequest
get' url opts = liftEffect (HTTP.get' url opts)

-- | Issue an HTTP GET to the given `URL` with explicit options.
-- | `method` is forbidden.
getUrl'
  :: forall r e opts trash
   . Row.Union opts trash (RequestOptions ())
  => Row.Lacks "method" opts
  => URL
  -> { | opts }
  -> RIO r e ClientRequest
getUrl' url opts = liftEffect (HTTP.getUrl' url opts)

-- | Issue an HTTP GET from a `RequestOptions`-shaped record only.
getOpts
  :: forall r e opts trash
   . Row.Union opts trash (RequestOptions ())
  => { | opts }
  -> RIO r e ClientRequest
getOpts opts = liftEffect (HTTP.getOpts opts)

-- | Cap the number of cached HTTP parsers Node keeps idle.
setMaxIdleHttpParsers :: forall r e. Int -> RIO r e Unit
setMaxIdleHttpParsers n = liftEffect (HTTP.setMaxIdleHttpParsers n)
