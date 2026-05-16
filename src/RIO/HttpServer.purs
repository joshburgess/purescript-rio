-- | A shape-only HTTP server framework. Defines the surface
-- | (`HttpServer` service record, `ServerRequest`, `ServerResponse`,
-- | `Handler`, routing combinators, response builders) so
-- | application code can be written against one API regardless
-- | of which driver is wired in. A reusable `mockHttpServer`
-- | runs handlers in-process for tests.
-- |
-- | The application defines a `Handler :: ServerRequest -> RIO
-- | ... ServerResponse`. A driver (`rio-node`'s Node http
-- | wrapper, an edge-runtime adapter, a Cloudflare Worker shim)
-- | is responsible for translating between its native request
-- | type and `ServerRequest`, calling the handler, and rendering
-- | the `ServerResponse` back. The handler itself stays portable.
-- |
-- | Routing is intentionally minimal: `route` takes a method,
-- | a path pattern (literal segments plus `:name` captures), and
-- | a handler. `router` chains a list of routes and produces a
-- | fallback `404` for misses. Middleware is plain handler
-- | composition (`Handler -> Handler`), so `Tracer.withSpan`,
-- | `Logger.withFields`, and so on layer in by wrapping the
-- | inner handler.
-- |
-- | Bodies are typed (`NoBody`, `TextBody`, `JsonBody`); a
-- | streaming `RequestStream` arm is reserved for the
-- | streaming-bodies follow-up.
module RIO.HttpServer
  ( HttpServer
  , ServerRequest
  , ServerResponse
  , Handler
  , Route
  , ResponseBody(..)
  , Middleware
  , mockHttpServer
  , runHandler
  , route
  , router
  , notFound
  , ok
  , textResponse
  , jsonResponse
  , status
  , withHeader
  , withHeaders
  , captureParam
  , withMiddleware
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array as Array
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)

import RIO.Core (RIO)
import RIO.HttpClient (Method, RequestBody)

-- | The HTTP server service. A backend wraps a handler and
-- | exposes two operations: `listen` (start accepting requests
-- | on a host / port) and `shutdown` (stop accepting). The
-- | service does not return any per-request handle because
-- | requests flow through the `Handler` the driver was
-- | constructed with, not through the service record.
type HttpServer =
  { listen :: { host :: String, port :: Int } -> Aff Unit
  , shutdown :: Aff Unit
  }

-- | A request the handler receives. `path` is the URL path with
-- | the leading `/` preserved; `query` is the parsed query
-- | string (drivers are responsible for the parse); `headers`
-- | are case-preserved as the driver received them.
type ServerRequest =
  { method :: Method
  , path :: String
  , query :: Array (Tuple String String)
  , headers :: Array (Tuple String String)
  , body :: RequestBody
  , params :: Array (Tuple String String)
  }

-- | The response a handler returns.
type ServerResponse =
  { status :: Int
  , headers :: Array (Tuple String String)
  , body :: ResponseBody
  }

-- | A response body. `TextBody` and `JsonBody` are the common
-- | cases; `NoResponseBody` covers `204` / `304` / `HEAD`
-- | responses. A streaming arm is reserved for the streaming-
-- | bodies follow-up.
data ResponseBody
  = NoResponseBody
  | TextResponseBody String
  | JsonResponseBody Json

derive instance eqResponseBody :: Eq ResponseBody

instance showResponseBody :: Show ResponseBody where
  show = case _ of
    NoResponseBody -> "NoResponseBody"
    TextResponseBody s -> "(TextResponseBody " <> show s <> ")"
    JsonResponseBody _ -> "(JsonResponseBody _)"

-- | A handler is an `Aff` from request to response. The handler
-- | is run by the driver per inbound request; failures inside
-- | the handler are the driver's responsibility (typically
-- | translating to a `500` and logging).
type Handler = ServerRequest -> Aff ServerResponse

-- | Middleware is a `Handler -> Handler` transformer.
type Middleware = Handler -> Handler

-- | A single route declaration: a method, a path pattern (with
-- | `:name` captures), and the handler to invoke on a match.
type Route =
  { method :: Method
  , pattern :: String
  , handler :: Handler
  }

-- | Build a route declaration.
route :: Method -> String -> Handler -> Route
route method pattern handler = { method, pattern, handler }

-- | Compose a list of routes into a single `Handler`. The first
-- | route whose method and pattern match wins; if none match,
-- | the result is a `404` with an empty body.
router :: Array Route -> Handler
router routes req = case Array.findMap (tryMatch req) routes of
  Just (Tuple matched params) ->
    matched.handler (req { params = req.params <> params })
  Nothing -> pure notFound

-- | Try to match a request against a single route. Returns the
-- | route together with the captured parameters if both method
-- | and pattern match.
tryMatch
  :: ServerRequest
  -> Route
  -> Maybe (Tuple Route (Array (Tuple String String)))
tryMatch req r
  | r.method /= req.method = Nothing
  | otherwise = case matchPattern r.pattern req.path of
      Nothing -> Nothing
      Just params -> Just (Tuple r params)

-- | Match a path pattern against a concrete path. Returns the
-- | captured parameters on success.
matchPattern :: String -> String -> Maybe (Array (Tuple String String))
matchPattern pattern path =
  let
    patSegs = String.split (Pattern "/") pattern
    pathSegs = String.split (Pattern "/") path
  in
    if Array.length patSegs /= Array.length pathSegs then Nothing
    else
      foldr step (Just []) (Array.zip patSegs pathSegs)
  where
  step (Tuple p s) acc = case acc of
    Nothing -> Nothing
    Just params -> case String.stripPrefix (Pattern ":") p of
      Just name -> Just (params <> [ Tuple name s ])
      Nothing -> if p == s then Just params else Nothing

-- | The canonical `404 Not Found` response.
notFound :: ServerResponse
notFound = { status: 404, headers: [], body: NoResponseBody }

-- | A `200 OK` with an empty body.
ok :: ServerResponse
ok = { status: 200, headers: [], body: NoResponseBody }

-- | A `200 OK` with a text body and `Content-Type: text/plain`.
textResponse :: String -> ServerResponse
textResponse t =
  { status: 200
  , headers: [ Tuple "Content-Type" "text/plain; charset=utf-8" ]
  , body: TextResponseBody t
  }

-- | A `200 OK` with a JSON body and `Content-Type: application/json`.
jsonResponse :: Json -> ServerResponse
jsonResponse j =
  { status: 200
  , headers: [ Tuple "Content-Type" "application/json" ]
  , body: JsonResponseBody j
  }

-- | Override the status code on an existing response.
status :: Int -> ServerResponse -> ServerResponse
status s r = r { status = s }

-- | Append a single header.
withHeader :: String -> String -> ServerResponse -> ServerResponse
withHeader k v r = r { headers = r.headers <> [ Tuple k v ] }

-- | Append a list of headers in order.
withHeaders
  :: Array (Tuple String String) -> ServerResponse -> ServerResponse
withHeaders hs r = r { headers = r.headers <> hs }

-- | Look up a captured path parameter (e.g. `:id` -> `"id"`).
captureParam :: String -> ServerRequest -> Maybe String
captureParam name req = lookupFirst name req.params

lookupFirst :: forall v. String -> Array (Tuple String v) -> Maybe v
lookupFirst k =
  Array.findMap
    (\(Tuple k' v) -> if k == k' then Just v else Nothing)

-- | Wrap a handler with a middleware. Equivalent to applying
-- | the middleware directly; this name is provided for
-- | readability at the call site.
withMiddleware :: Middleware -> Handler -> Handler
withMiddleware mw h = mw h

-- | Build an in-process mock server. The `listen` and `shutdown`
-- | operations are no-ops; tests drive the handler directly
-- | through `runHandler`.
mockHttpServer :: HttpServer
mockHttpServer =
  { listen: \_ -> pure unit
  , shutdown: pure unit
  }

-- | Run a handler against a request directly. Useful for tests
-- | that want to exercise routing and middleware without
-- | spinning up a real server. Lives in `RIO` so callers can
-- | compose with the env row, even though the handler itself
-- | runs in `Aff`.
runHandler
  :: forall r e
   . Handler
  -> ServerRequest
  -> RIO r e ServerResponse
runHandler h req = liftAff (h req)
