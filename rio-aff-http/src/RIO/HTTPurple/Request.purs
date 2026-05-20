-- | A snapshot of HTTP-shaped values captured from an HTTPurple
-- | `Request` before crossing into `RIO`. Carries the method,
-- | path, headers, and a per-request id chosen by
-- | `mkRequestContext` (either honouring an inbound `X-Request-Id`
-- | header or assigning a monotonic counter value).
-- |
-- | This module exists because HTTPurple's `Request` is
-- | parameterised over the route type and lives in `Aff` /
-- | `Effect`. Capturing the pieces middleware actually needs into
-- | a flat record keeps the `RIO`-side middleware free of the
-- | route type variable.
module RIO.Aff.HTTPurple.Request
  ( RequestContext
  , defaultRequestIdHeader
  , mkRequestContext
  , newRequestCounter
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import HTTPurple (Method, Request, RequestHeaders, lookup)

-- | A flat snapshot of the HTTP-shaped values needed by
-- | `RIO.Aff.HTTPurple.Middleware.withRequestContext` and
-- | `RIO.Aff.HTTPurple.Auth.requireAuth`.
type RequestContext =
  { method :: Method
  , path :: String
  , requestId :: String
  , headers :: RequestHeaders
  }

-- | The default header name consulted for an inbound request id.
defaultRequestIdHeader :: String
defaultRequestIdHeader = "X-Request-Id"

-- | Allocate a fresh per-process counter. The counter is read /
-- | incremented every time `mkRequestContext` falls back to
-- | generating a request id because the inbound header was
-- | absent. Call this once at startup and pass the resulting
-- | `Ref` into every `mkRequestContext` invocation.
newRequestCounter :: Effect (Ref Int)
newRequestCounter = Ref.new 0

-- | Build a `RequestContext` from an inbound `Request`. If the
-- | request carries `opts.headerName`, that value becomes the
-- | request id; otherwise the next monotonic value from
-- | `opts.counter` is used, formatted as `req-N`.
-- |
-- | Pass `defaultRequestIdHeader` for `opts.headerName` unless
-- | you need a different convention.
mkRequestContext
  :: forall route
   . { headerName :: String, counter :: Ref Int }
  -> Request route
  -> Effect RequestContext
mkRequestContext opts req = do
  rid <- case lookup req.headers opts.headerName of
    Just s -> pure s
    Nothing -> do
      n <- Ref.modify (_ + 1) opts.counter
      pure ("req-" <> show n)
  pure
    { method: req.method
    , path: req.url
    , requestId: rid
    , headers: req.headers
    }
