-- | RIO-flavoured wrappers around `Node.URL`.
-- |
-- | A `Node.URL.URL` is a mutable handle whose accessors are
-- | `Effect`-valued (the underlying WHATWG-URL implementation
-- | maintains its own internal state). Rather than fronting that
-- | with a service row (the URL is a value, not a capability), we
-- | simply re-expose every operation lifted from `Effect` into
-- | `RIO`.
-- |
-- | The two operations that are genuinely pure (`canParse` and
-- | `origin`) stay pure here; everything else picks up a
-- | `forall r e. RIO r e ...` shape.
module RIO.Aff.Node.URL
  ( module Exports
  , canParse
  , domainToAscii
  , domainToUnicode
  , fileURLToPath
  , fileURLToPath'
  , format
  , format'
  , hash
  , host
  , hostname
  , href
  , new
  , new'
  , origin
  , password
  , pathToFileURL
  , pathname
  , port
  , protocol
  , search
  , searchParams
  , setHash
  , setHost
  , setHostname
  , setHref
  , setPassword
  , setPathname
  , setPort
  , setProtocol
  , setSearch
  , setUsername
  , urlToHTTPOptions
  , username
  ) where

import Prelude

import Effect.Class (liftEffect)
import Node.URL (HttpOptions, URL, UrlFormatOptions) as Exports
import Node.URL (URL, HttpOptions)
import Node.URL as NU
import Node.URL.URLSearchParams (URLSearchParams)
import Prim.Row as Row

import RIO.Aff.Core (RIO)

-- | Construct a new URL by parsing the string.
new :: forall r e. String -> RIO r e URL
new s = liftEffect (NU.new s)

-- | Construct a new URL by parsing `input` relative to `base`.
new' :: forall r e. String -> String -> RIO r e URL
new' input base = liftEffect (NU.new' input base)

-- | Convert a filesystem path to a `file://` URL.
pathToFileURL :: forall r e. String -> RIO r e URL
pathToFileURL p = liftEffect (NU.pathToFileURL p)

hash :: forall r e. URL -> RIO r e String
hash u = liftEffect (NU.hash u)

setHash :: forall r e. String -> URL -> RIO r e Unit
setHash v u = liftEffect (NU.setHash v u)

host :: forall r e. URL -> RIO r e String
host u = liftEffect (NU.host u)

setHost :: forall r e. String -> URL -> RIO r e Unit
setHost v u = liftEffect (NU.setHost v u)

hostname :: forall r e. URL -> RIO r e String
hostname u = liftEffect (NU.hostname u)

setHostname :: forall r e. String -> URL -> RIO r e Unit
setHostname v u = liftEffect (NU.setHostname v u)

href :: forall r e. URL -> RIO r e String
href u = liftEffect (NU.href u)

setHref :: forall r e. String -> URL -> RIO r e Unit
setHref v u = liftEffect (NU.setHref v u)

-- | Pure URL origin accessor (the underlying Node implementation
-- | exposes `origin` as a value, not an effectful getter).
origin :: URL -> String
origin = NU.origin

password :: forall r e. URL -> RIO r e String
password u = liftEffect (NU.password u)

setPassword :: forall r e. String -> URL -> RIO r e Unit
setPassword v u = liftEffect (NU.setPassword v u)

pathname :: forall r e. URL -> RIO r e String
pathname u = liftEffect (NU.pathname u)

setPathname :: forall r e. String -> URL -> RIO r e Unit
setPathname v u = liftEffect (NU.setPathname v u)

port :: forall r e. URL -> RIO r e String
port u = liftEffect (NU.port u)

setPort :: forall r e. String -> URL -> RIO r e Unit
setPort v u = liftEffect (NU.setPort v u)

protocol :: forall r e. URL -> RIO r e String
protocol u = liftEffect (NU.protocol u)

setProtocol :: forall r e. String -> URL -> RIO r e Unit
setProtocol v u = liftEffect (NU.setProtocol v u)

search :: forall r e. URL -> RIO r e String
search u = liftEffect (NU.search u)

setSearch :: forall r e. String -> URL -> RIO r e Unit
setSearch v u = liftEffect (NU.setSearch v u)

searchParams :: forall r e. URL -> RIO r e URLSearchParams
searchParams u = liftEffect (NU.searchParams u)

username :: forall r e. URL -> RIO r e String
username u = liftEffect (NU.username u)

setUsername :: forall r e. String -> URL -> RIO r e Unit
setUsername v u = liftEffect (NU.setUsername v u)

-- | Pure parse-probe. Returns whether `input` (resolved against
-- | `base`) is a valid URL without throwing.
canParse :: String -> String -> Boolean
canParse = NU.canParse

domainToAscii :: forall r e. String -> RIO r e String
domainToAscii d = liftEffect (NU.domainToAscii d)

domainToUnicode :: forall r e. String -> RIO r e String
domainToUnicode d = liftEffect (NU.domainToUnicode d)

-- | Convert a `file://` URL string to a path.
fileURLToPath :: forall r e. String -> RIO r e String
fileURLToPath s = liftEffect (NU.fileURLToPath s)

-- | Convert a `file://` URL value to a path.
fileURLToPath' :: forall r e. URL -> RIO r e String
fileURLToPath' u = liftEffect (NU.fileURLToPath' u)

format :: forall r e. URL -> RIO r e String
format u = liftEffect (NU.format u)

format'
  :: forall r e options trash
   . Row.Union options trash NU.UrlFormatOptions
  => URL
  -> { | options }
  -> RIO r e String
format' u opts = liftEffect (NU.format' u opts)

urlToHTTPOptions :: forall r e. URL -> RIO r e HttpOptions
urlToHTTPOptions u = liftEffect (NU.urlToHTTPOptions u)
