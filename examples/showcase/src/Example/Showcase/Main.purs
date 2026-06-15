-- | A worked example that wires every major RIO service
-- | introduced by the strategic-gap work into a single
-- | in-process HTTP application and drives a handful of
-- | requests through the resulting handler.
-- |
-- | The app is intentionally small. It exposes five routes
-- | (`/health`, `/schema`, `/widgets` POST, `/widgets` GET,
-- | `/events`) backed by a `mockSql` store, validates request
-- | bodies with a `Schema` (including a `brand`ed identifier
-- | rendered into the JSON Schema fragment as a `title`),
-- | structures log output with `Logger.withFields`, opens a
-- | `Tracer` span per request (recording into an in-memory
-- | tracer that the OTel exporter then serialises to OTLP/JSON),
-- | and serves a Server-Sent Events feed through
-- | `HttpStream.fromChunks`.
-- |
-- | No real network is touched: requests are constructed by
-- | hand and dispatched through the `Handler` the `router`
-- | returns. The point is to show how the pieces compose, not
-- | to spin up a server.
-- |
-- | Run with:
-- |
-- |   npx spago run -p rio-example-showcase
module Example.Showcase.Main
  ( main
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as Json
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import Foreign.Object as Object
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO)
import RIO.Aff.Env (provideAll)
import RIO.Aff.HttpClient (Method(..), RequestBody(..))
import RIO.Aff.HttpServer
  ( Handler
  , ResponseBody(..)
  , ServerRequest
  , ServerResponse
  , eventStreamResponse
  , jsonResponse
  , route
  , router
  , status
  )
import RIO.Aff.HttpStream as Stream
import RIO.Aff.Internal (unRIO)
import RIO.Aff.Logger (Logger, consoleLogger, logInfo, withFields)
import RIO.Aff.Schema (Branded, Schema)
import RIO.Aff.Schema as Schema
import RIO.Aff.Sql (Sql, SqlError, SqlResult, SqlRow, SqlValue(..), mockSql)
import RIO.Aff.Sql as Sql
import RIO.Aff.Test.Tracer (newRecordingTracer)
import RIO.Aff.Tracer (Span, Tracer, addAttribute, withSpan)
import RIO.Aff.Tracer.OTel (exportSpans, renderOTLP)
import RIO.Aff.Tracer.Propagation as Propagation

-- | The application environment row exposed to every handler.
type AppEnv =
  ( logger :: Logger
  , tracer :: Tracer
  , sql :: Sql
  )

-- | The application's typed error row. `Sql` failures are the
-- | only tagged failure a handler raises; everything else is
-- | mapped to a `500` by the request bracket.
type AppErr = (sqlError :: SqlError)

-- | A `WidgetId` brand: at the type level the id is distinct
-- | from a plain `Int`, and `Schema.brand` surfaces the tag in
-- | the JSON Schema fragment as a `"title": "WidgetId"`. The
-- | example only uses the brand on the JSON Schema export path;
-- | application code stays with raw `Int`s internally to avoid
-- | round-tripping through `parseJson` just to mint a wrapper.
type WidgetId = Branded "WidgetId" Int

widgetIdSchema :: Schema WidgetId
widgetIdSchema = Schema.brand (Proxy :: Proxy "WidgetId") Schema.int

-- | The shape of a `POST /widgets` request body: just the
-- | fields the client supplies. The server assigns the id.
type WidgetCreate =
  { name :: String
  , quantity :: Int
  }

widgetCreateSchema :: Schema WidgetCreate
widgetCreateSchema = Schema.recordOf
  ( { name: _, quantity: _ }
      <$> Schema.field "name" _.name Schema.string
      <*> Schema.field "quantity" _.quantity Schema.int
  )

-- | An in-process store of widgets together with the next-id
-- | counter the mock SQL insert reads from.
type Store =
  { rows ::
      Ref.Ref (Array { id :: Int, name :: String, quantity :: Int })
  , nextId :: Ref.Ref Int
  }

newStore :: Effect Store
newStore = do
  rows <- Ref.new []
  nextId <- Ref.new 1
  pure { rows, nextId }

rowToSqlRow
  :: { id :: Int, name :: String, quantity :: Int } -> SqlRow
rowToSqlRow w = Object.fromFoldable
  [ Tuple "id" (SqlInt w.id)
  , Tuple "name" (SqlString w.name)
  , Tuple "quantity" (SqlInt w.quantity)
  ]

-- | Build the mock `Sql` service. `execute` recognises the
-- | three transaction statements `withTransaction` issues
-- | (`BEGIN` / `COMMIT` / `ROLLBACK`) plus a parameterised
-- | insert that pushes a row into the in-memory store. `query`
-- | ignores the statement text and returns every stored widget.
mkSql :: Store -> Sql
mkSql store = mockSql
  { execute: \stmt -> case stmt.text of
      "BEGIN" -> pure (Right okRes)
      "COMMIT" -> pure (Right okRes)
      "ROLLBACK" -> pure (Right okRes)
      _ -> case stmt.params of
        [ SqlString name, SqlInt qty ] -> do
          newId <- liftEffect (Ref.modify (_ + 1) store.nextId)
          liftEffect
            ( Ref.modify_
                (\xs -> Array.snoc xs { id: newId, name, quantity: qty })
                store.rows
            )
          pure (Right { rowsAffected: 1 })
        _ -> pure (Right okRes)
  , query: \_ -> do
      widgets <- liftEffect (Ref.read store.rows)
      pure (Right (map rowToSqlRow widgets))
  }
  where
  okRes :: SqlResult
  okRes = { rowsAffected: 0 }

-- | Adapt a `RIO` program into a `Handler`. Runs the program
-- | against the wired env; tagged `Sql` failures map to a `500`.
adaptHandler
  :: Record AppEnv
  -> (ServerRequest -> RIO AppEnv AppErr ServerResponse)
  -> Handler
adaptHandler env program req = do
  result <- unRIO (provideAll env (program req)) {}
  case result of
    Right resp -> pure resp
    Left _ -> pure
      (status 500 { status: 500, headers: [], body: NoResponseBody })

-- | The per-request bracket. Opens a `Tracer` span around the
-- | handler body, parses the inbound `traceparent` header (W3C
-- | trace context) and attaches the upstream trace and span ids
-- | as span attributes, structures the log line with
-- | `Logger.withFields`, and emits an info entry on entry.
withRequest
  :: forall a
   . ServerRequest
  -> RIO AppEnv AppErr a
  -> RIO AppEnv AppErr a
withRequest req body =
  withFields
    [ Tuple "request.path" req.path
    , Tuple "request.method" (show req.method)
    ]
    ( withSpan "http.request" do
        addAttribute "http.method" (show req.method)
        addAttribute "http.path" req.path
        case traceparentHeader req.headers of
          Nothing -> pure unit
          Just hv -> case Propagation.parseTraceparent hv of
            Just tc -> do
              addAttribute "http.trace.id" tc.traceId
              addAttribute "http.parent.span.id" tc.spanId
            Nothing -> pure unit
        logInfo "handling request"
        body
    )

traceparentHeader
  :: Array (Tuple String String) -> Maybe String
traceparentHeader =
  Array.findMap \(Tuple k v) ->
    if String.toLower k == "traceparent" then Just v else Nothing

-- | The route table. `router` collapses the route list into a
-- | single `Handler` with `:capture` matching and a `404`
-- | fallback; a real driver would call that `Handler` per
-- | inbound request after building the `ServerRequest` from
-- | the wire.
routes
  :: Record AppEnv
  -> Array { handler :: Handler, method :: Method, pattern :: String }
routes env =
  [ route GET "/health"
      (adaptHandler env (\req -> withRequest req healthHandler))
  , route GET "/schema"
      (adaptHandler env (\req -> withRequest req schemaHandler))
  , route POST "/widgets"
      (adaptHandler env (\req -> withRequest req (createHandler req)))
  , route GET "/widgets"
      (adaptHandler env (\req -> withRequest req listHandler))
  , route GET "/events"
      (adaptHandler env (\req -> withRequest req eventsHandler))
  ]

healthHandler :: RIO AppEnv AppErr ServerResponse
healthHandler = pure
  { status: 200
  , headers: []
  , body: TextResponseBody "ok"
  }

-- | Return the JSON Schema for the brand-tagged `WidgetId`,
-- | demonstrating the `title` field `Schema.brand` adds.
schemaHandler :: RIO AppEnv AppErr ServerResponse
schemaHandler = pure
  (jsonResponse (Schema.toJsonSchema widgetIdSchema))

-- | Validate the request body with `widgetCreateSchema` and
-- | insert it inside a `withTransaction` bracket. `Sql.withTransaction`
-- | issues `BEGIN` / `COMMIT` through the mock `execute` handler
-- | and rolls back on typed failure.
createHandler
  :: ServerRequest -> RIO AppEnv AppErr ServerResponse
createHandler req = case req.body of
  JsonBody j -> case Schema.decode widgetCreateSchema j of
    Left err -> pure
      ( status 400
          { status: 400
          , headers: []
          , body: TextResponseBody (Schema.renderError err)
          }
      )
    Right widget -> Sql.withTransaction do
      _ <- Sql.execute
        ( Sql.statement
            "INSERT INTO widgets (name, quantity) VALUES ($1, $2)"
            [ SqlString widget.name, SqlInt widget.quantity ]
        )
      logInfo "inserted widget"
      pure
        ( jsonResponse
            ( Json.fromObject
                ( Object.fromFoldable
                    [ Tuple "name" (Json.fromString widget.name)
                    , Tuple "quantity"
                        ( Json.fromNumber
                            (Int.toNumber widget.quantity)
                        )
                    ]
                )
            )
        )
  _ -> pure
    ( status 415
        { status: 415
        , headers: []
        , body: TextResponseBody "expected JSON body"
        }
    )

listHandler :: RIO AppEnv AppErr ServerResponse
listHandler = do
  rows <- Sql.query (Sql.statement "SELECT * FROM widgets" [])
  pure (jsonResponse (Json.fromArray (map Sql.rowToJson rows)))

-- | Streaming SSE: hand `eventStreamResponse` a `BodyStream`
-- | built from three pre-formatted SSE frames. A real handler
-- | would build the stream lazily with a custom `BodyStream`
-- | (`Aff (Maybe String)`) that pulls from a `Channel` or a
-- | polled `Queue`. (`fromChunks` here just wraps a fixed array.)
eventsHandler :: RIO AppEnv AppErr ServerResponse
eventsHandler = do
  stream <- liftAff
    ( Stream.fromChunks
        [ "data: hello\n\n"
        , "data: world\n\n"
        , "data: bye\n\n"
        ]
    )
  pure (eventStreamResponse stream)

-- | Dispatch one canned request through the router and print a
-- | summary line plus the body content. Streaming bodies are
-- | drained for display.
dispatch :: Record AppEnv -> ServerRequest -> Aff Unit
dispatch env req = do
  resp <- router (routes env) req
  liftEffect
    ( log
        ( "  "
            <> show req.method
            <> " "
            <> req.path
            <> "  -> "
            <> show resp.status
        )
    )
  case resp.body of
    TextResponseBody t -> liftEffect (log ("    body: " <> t))
    JsonResponseBody j ->
      liftEffect (log ("    body: " <> Json.stringify j))
    StreamResponseBody s -> do
      body <- Stream.drain s
      liftEffect (log ("    body (streamed): " <> body))
    NoResponseBody -> pure unit

main :: Effect Unit
main = launchAff_ do
  store <- liftEffect newStore
  recorder <- newRecordingTracer
  logger <- liftEffect consoleLogger
  let
    env :: Record AppEnv
    env =
      { logger
      , tracer: recorder.tracer
      , sql: mkSql store
      }
  liftEffect (log "")
  liftEffect (log "showcase: dispatching example requests")
  liftEffect (log "--------------------------------------")
  dispatch env
    { method: GET
    , path: "/health"
    , query: []
    , headers:
        [ Tuple "traceparent"
            "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
        ]
    , body: NoBody
    , params: []
    }
  dispatch env
    { method: GET
    , path: "/schema"
    , query: []
    , headers: []
    , body: NoBody
    , params: []
    }
  dispatch env
    { method: POST
    , path: "/widgets"
    , query: []
    , headers: [ Tuple "Content-Type" "application/json" ]
    , body: JsonBody
        ( Schema.encode widgetCreateSchema
            { name: "widget-1", quantity: 42 }
        )
    , params: []
    }
  dispatch env
    { method: GET
    , path: "/widgets"
    , query: []
    , headers: []
    , body: NoBody
    , params: []
    }
  dispatch env
    { method: GET
    , path: "/events"
    , query: []
    , headers: []
    , body: NoBody
    , params: []
    }
  spans <- liftEffect recorder.snapshot
  liftEffect do
    log ""
    log "OTLP/JSON export of the captured spans:"
    log "---------------------------------------"
    log (renderOTLP (otlpDoc spans))

otlpDoc :: Array Span -> Json
otlpDoc spans =
  exportSpans
    { resource: { serviceName: "rio-showcase", serviceVersion: "0.0.1" }
    , scope: { name: "rio-example-showcase", version: "0.0.1" }
    , traceId: "0af7651916cd43dd8448eb211c80319c"
    }
    (idMap spans)
    spans
  where
  idMap ss = Map.fromFoldable
    (Array.mapWithIndex (\i s -> Tuple s.id (pad16 (show (i + 1)))) ss)
  pad16 s =
    let
      need = 16 - CU.length s
    in
      if need <= 0 then s
      else CU.fromCharArray (Array.replicate need '0') <> s
