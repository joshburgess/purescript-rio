-- | A pluggable HTTP client service.
-- |
-- | `RIO.Aff.HttpClient` defines the *shape* of an HTTP transport so
-- | application code can be written against a single, opinionated
-- | API regardless of which backend ends up wired in (Node's
-- | native `https`, `fetch`, an in-memory test double). The
-- | service record is small:
-- |
-- | ```purescript
-- | type HttpClient = { sendRequest :: Request -> Aff (Either HttpError Response) }
-- | ```
-- |
-- | A `Request` is a plain record (method, url, headers, body,
-- | optional timeout); a `Response` is similarly a plain record.
-- | `HttpError` covers transport failure, timeout, and Schema
-- | decode failure as a closed sum; non-2xx responses are *not*
-- | automatically errors (the caller decides via `ensureStatus`).
-- |
-- | The high-level entry points (`get`, `post`, `put`, `patch`,
-- | `delete`, `head_`, `options`) return raw `Response` values
-- | and project `HttpError` onto the `httpError` row tag, so the
-- | error row of every call is
-- | `(httpError :: HttpError | e)`. Combine with
-- | `RIO.Aff.Schedule.retry` for retry policies, with `RIO.Aff.Tracer`
-- | for span instrumentation, or with `RIO.Aff.Logger` for structured
-- | logs - the client deliberately does not bake any of those in,
-- | so the same call site works whether the program has those
-- | services or not.
-- |
-- | ```purescript
-- | example
-- |   :: forall r e
-- |    . RIO
-- |        (httpClient :: HttpClient | r)
-- |        (httpError :: HttpError | e)
-- |        User
-- | example = do
-- |   resp <- get "https://api.example.com/users/42"
-- |   ensureStatus resp
-- |   decodeBody userSchema resp
-- | ```
-- |
-- | A concrete backend (e.g. Node `https`) lives in `rio-aff-node`
-- | once added; this module ships only the shape, smart
-- | constructors, and `mockHttpClient` for tests.
module RIO.Aff.HttpClient
  ( HttpClient
  , Method(..)
  , RequestBody(..)
  , Request
  , Response
  , HttpError(..)
  , StatusClass(..)
  , mockHttpClient
  -- Request building
  , newRequest
  , withMethod
  , withHeader
  , withHeaders
  , withBody
  , withJsonBody
  , withTimeout
  -- Sending
  , send
  , get
  , post
  , put
  , patch
  , delete
  , head_
  , options
  -- Body decoding
  , decodeBody
  -- Status helpers
  , statusClass
  , isSuccess
  , ensureStatus
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Array (uncons) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (toLower) as String
import Data.Time.Duration (Milliseconds)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, fail)
import RIO.Aff.HttpStream (BodyStream)
import RIO.Aff.Schema (DecodeError, Schema)
import RIO.Aff.Schema as Schema

-- | HTTP request methods. The `Show` instance produces the
-- | uppercase wire name (`"GET"`, `"POST"`, ...) so it can be
-- | spliced directly into a request line.
data Method
  = GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD
  | OPTIONS

derive instance eqMethod :: Eq Method
derive instance ordMethod :: Ord Method

instance showMethod :: Show Method where
  show = case _ of
    GET -> "GET"
    POST -> "POST"
    PUT -> "PUT"
    PATCH -> "PATCH"
    DELETE -> "DELETE"
    HEAD -> "HEAD"
    OPTIONS -> "OPTIONS"

-- | Request body alternatives.
-- |
-- |   * `NoBody` sends no entity body at all.
-- |   * `TextBody` sends the literal string; the caller is
-- |     responsible for `Content-Type`.
-- |   * `JsonBody` carries a `Json` value. The high-level
-- |     `withJsonBody` helper sets `Content-Type: application/json`
-- |     automatically.
-- |   * `StreamBody` carries a pull-based chunk stream. The
-- |     backend pulls chunks one at a time and writes each to
-- |     the wire; `Nothing` ends the request body. Use this for
-- |     uploads that should not be buffered in memory (file
-- |     uploads, large multipart payloads). The caller is still
-- |     responsible for `Content-Type` and any framing
-- |     (chunked-transfer-encoding is the backend's call).
data RequestBody
  = NoBody
  | TextBody String
  | JsonBody Json
  | StreamBody BodyStream

-- | The shape sent to the backend. The fields are intentionally
-- | plain (no smart constructors required) so unfamiliar
-- | backends can serialise them however they like.
type Request =
  { method :: Method
  , url :: String
  , headers :: Array (Tuple String String)
  , body :: RequestBody
  , timeout :: Maybe Milliseconds
  }

-- | The shape returned from the backend. The body is always
-- | surfaced as a `String`; use `decodeBody` or `Schema.parseJson`
-- | to decode it.
type Response =
  { status :: Int
  , statusText :: String
  , headers :: Array (Tuple String String)
  , body :: String
  }

-- | Errors that can surface during a request.
-- |
-- |   * `HttpTransport` covers network and protocol-level errors
-- |     (DNS failure, connection refused, malformed framing,
-- |     ...).
-- |   * `HttpTimeout` is the timeout signal. Backends should map
-- |     their host-level timeout exception to this tag so
-- |     `ensureStatus` and Schedule policies can treat timeouts
-- |     uniformly.
-- |   * `HttpUnexpectedStatus` is raised by `ensureStatus` for
-- |     non-2xx responses. Carries the response so callers can
-- |     still inspect body / headers.
-- |   * `HttpDecode` is raised by `decodeBody` when a Schema
-- |     decode fails. Backends never raise this directly.
data HttpError
  = HttpTransport String
  | HttpTimeout
  | HttpUnexpectedStatus Response
  | HttpDecode DecodeError

derive instance eqHttpError :: Eq HttpError

instance showHttpError :: Show HttpError where
  show = case _ of
    HttpTransport msg -> "(HttpTransport " <> show msg <> ")"
    HttpTimeout -> "HttpTimeout"
    HttpUnexpectedStatus r ->
      "(HttpUnexpectedStatus { status: " <> show r.status <> " })"
    HttpDecode e -> "(HttpDecode " <> show e <> ")"

-- | The five canonical HTTP status classes.
data StatusClass
  = Informational
  | Success
  | Redirection
  | ClientError
  | ServerError
  | Unknown

derive instance eqStatusClass :: Eq StatusClass

instance showStatusClass :: Show StatusClass where
  show = case _ of
    Informational -> "Informational"
    Success -> "Success"
    Redirection -> "Redirection"
    ClientError -> "ClientError"
    ServerError -> "ServerError"
    Unknown -> "Unknown"

-- | The HTTP client service. A backend is "just" an `Aff`
-- | computation that takes a `Request` and returns either an
-- | `HttpError` or a `Response`; `Aff` already covers cancellation
-- | and concurrency, so the service surface stays this small.
type HttpClient =
  { sendRequest :: Request -> Aff (Either HttpError Response)
  }

-- | Build an `HttpClient` from a hand-rolled handler. The
-- | intended use is testing: thread a `Ref` of canned responses
-- | (or an `Object` keyed by URL) through the handler and assert
-- | on requests and responses. Production backends should be
-- | constructed by a dedicated module (e.g. `rio-aff-node`).
mockHttpClient :: (Request -> Aff (Either HttpError Response)) -> HttpClient
mockHttpClient handler = { sendRequest: handler }

-- | The starting point for a request builder. Method defaults to
-- | `GET`, headers and body are empty, no timeout. Combine with
-- | the `with*` helpers:
-- |
-- | ```purescript
-- | newRequest "https://api.example.com/users"
-- |   # withMethod POST
-- |   # withHeader "Authorization" ("Bearer " <> token)
-- |   # withJsonBody userJson
-- | ```
newRequest :: String -> Request
newRequest url =
  { method: GET
  , url
  , headers: []
  , body: NoBody
  , timeout: Nothing
  }

-- | Set the method.
withMethod :: Method -> Request -> Request
withMethod m r = r { method = m }

-- | Append a single header. Duplicate names are preserved (some
-- | servers care about repetition; let the backend collapse them
-- | if it wants).
withHeader :: String -> String -> Request -> Request
withHeader k v r = r { headers = r.headers <> [ Tuple k v ] }

-- | Append a list of headers in order.
withHeaders :: Array (Tuple String String) -> Request -> Request
withHeaders hs r = r { headers = r.headers <> hs }

-- | Set the body directly.
withBody :: RequestBody -> Request -> Request
withBody b r = r { body = b }

-- | Set a JSON body and add `Content-Type: application/json` if
-- | the request does not already carry one.
withJsonBody :: Json -> Request -> Request
withJsonBody j r =
  let
    hasCt = case lookupCi "content-type" r.headers of
      Just _ -> true
      Nothing -> false
    withCt =
      if hasCt then r.headers
      else r.headers <> [ Tuple "Content-Type" "application/json" ]
  in
    r { body = JsonBody j, headers = withCt }

-- | Set a per-request timeout. The backend is responsible for
-- | enforcement; not every backend supports timeouts on every
-- | platform.
withTimeout :: Milliseconds -> Request -> Request
withTimeout t r = r { timeout = Just t }

lookupCi :: forall a. String -> Array (Tuple String a) -> Maybe a
lookupCi key = go
  where
  target = toLowerAscii key
  go arr = case arr of
    [] -> Nothing
    _ -> case Array.uncons arr of
      Just { head: Tuple k v, tail }
        | toLowerAscii k == target -> Just v
        | otherwise -> go tail
      Nothing -> Nothing

toLowerAscii :: String -> String
toLowerAscii = String.toLower

-- | Send a `Request` through the wired-in `HttpClient`. Surfaces
-- | transport / timeout failures on the `httpError` row tag.
send
  :: forall r e
   . Request
  -> RIO (httpClient :: HttpClient | r) (httpError :: HttpError | e) Response
send req = do
  client <- ask (Proxy :: Proxy "httpClient")
  result <- liftAff (client.sendRequest req)
  case result of
    Left err -> fail (Proxy :: Proxy "httpError") err
    Right resp -> pure resp

-- | Shorthand for a no-body `GET`.
get
  :: forall r e
   . String
  -> RIO (httpClient :: HttpClient | r) (httpError :: HttpError | e) Response
get url = send (newRequest url)

-- | Shorthand for a `POST` carrying the supplied body.
post
  :: forall r e
   . String
  -> RequestBody
  -> RIO (httpClient :: HttpClient | r) (httpError :: HttpError | e) Response
post url b = send (withBody b (withMethod POST (newRequest url)))

-- | Shorthand for a `PUT` carrying the supplied body.
put
  :: forall r e
   . String
  -> RequestBody
  -> RIO (httpClient :: HttpClient | r) (httpError :: HttpError | e) Response
put url b = send (withBody b (withMethod PUT (newRequest url)))

-- | Shorthand for a `PATCH` carrying the supplied body.
patch
  :: forall r e
   . String
  -> RequestBody
  -> RIO (httpClient :: HttpClient | r) (httpError :: HttpError | e) Response
patch url b = send (withBody b (withMethod PATCH (newRequest url)))

-- | Shorthand for a no-body `DELETE`.
delete
  :: forall r e
   . String
  -> RIO (httpClient :: HttpClient | r) (httpError :: HttpError | e) Response
delete url = send (withMethod DELETE (newRequest url))

-- | Shorthand for a `HEAD`. The trailing underscore avoids the
-- | clash with `Data.Array.head` in callers.
head_
  :: forall r e
   . String
  -> RIO (httpClient :: HttpClient | r) (httpError :: HttpError | e) Response
head_ url = send (withMethod HEAD (newRequest url))

-- | Shorthand for an `OPTIONS` request.
options
  :: forall r e
   . String
  -> RIO (httpClient :: HttpClient | r) (httpError :: HttpError | e) Response
options url = send (withMethod OPTIONS (newRequest url))

-- | Decode a `Response` body through a `Schema`. Surfaces decode
-- | failures on the `httpError` row tag as `HttpDecode`.
decodeBody
  :: forall r e a
   . Schema a
  -> Response
  -> RIO r (httpError :: HttpError | e) a
decodeBody schema resp = case Schema.parseJson schema resp.body of
  Left e -> fail (Proxy :: Proxy "httpError") (HttpDecode e)
  Right a -> pure a

-- | Classify an HTTP status code into one of the five canonical
-- | classes. Codes outside `[100, 599]` map to `Unknown`.
statusClass :: Int -> StatusClass
statusClass code
  | code >= 100 && code < 200 = Informational
  | code >= 200 && code < 300 = Success
  | code >= 300 && code < 400 = Redirection
  | code >= 400 && code < 500 = ClientError
  | code >= 500 && code < 600 = ServerError
  | otherwise = Unknown

-- | `true` iff the response carries a 2xx status.
isSuccess :: Response -> Boolean
isSuccess r = statusClass r.status == Success

-- | Fail with `HttpUnexpectedStatus` if the response is not 2xx.
-- | The original response is preserved on the error so callers
-- | can still inspect the body or headers.
ensureStatus
  :: forall r e
   . Response
  -> RIO r (httpError :: HttpError | e) Unit
ensureStatus resp =
  if isSuccess resp then pure unit
  else fail (Proxy :: Proxy "httpError") (HttpUnexpectedStatus resp)

