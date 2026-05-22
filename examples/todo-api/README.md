# todo-api

A small HTTP service built with [HTTPurple](https://pursuit.purescript.org/packages/purescript-httpurple/4.0.0)
and `rio`, backed by a real Postgres instance via `rio-aff-postgres`.
Listens on `localhost:8080`.

The example covers the production-relevant slice of the library
in a single shape you can run:

- **Services** (`src/Example/TodoApi/Services.purs`): re-exports
  the library services the example reads against: `Logger`
  (`RIO.Aff.Logger`), `Clock` (`RIO.Aff.Clock`), the `Local String`
  request id (`RIO.Aff.Local`), and the `Postgres` token + `PgError`
  type (`RIO.Aff.Postgres`).
- **Layers** (`src/Example/TodoApi/Layers.purs`): `appLayer`
  horizontally combines `RIO.Aff.Logger.consoleLogger` with a
  `postgresLayer` that owns the `node-postgres` pool's lifetime
  via a scope finalizer. `migrate` defers to
  `RIO.Aff.Postgres.Migrate.migrate`, which takes a Postgres
  advisory lock, records applied versions in
  `__rio_migrations`, and applies any pending step. Adding a
  new column or index later is a new entry in the `Map Int
  Migration`, not a one-off DDL re-run.
- **Middleware** (`src/Example/TodoApi/Middleware.purs`):
  `withRequestContext` wraps every handler with a per-request
  log-annotation block and times the body. `requireAuth` is a
  bearer-token check that raises an `unauthorized` typed
  failure.
- **Handlers** (`src/Example/TodoApi/Handlers.purs`): each
  endpoint is an `RIO` action that calls `query` / `queryParams`
  / `execParams` from `RIO.Aff.Postgres` directly. Three typed
  failures (`notFound`, `unauthorized`, `db`) flow up through
  the error row; the `db` tag carries any driver error.
- **JSON codecs** (`src/Example/TodoApi/Codecs.purs`): argonaut
  encoders/decoders wrapped in HTTPurple's `JsonEncoder` /
  `JsonDecoder` newtypes.
- **Routes** (`src/Example/TodoApi/Routes.purs`): a
  `Routing.Duplex` route definition for `/todos` and
  `/todos/:id`.
- **Wiring** (`src/Example/TodoApi/Main.purs`): reads
  `DATABASE_URL`, runs the whole program under
  `provideLayer (appLayer connStr)` so the pool's scope spans
  the server's lifetime, runs `migrate`, installs the HTTPurple
  router, then parks forever.

## Running

The example needs a Postgres reachable at the URL in
`DATABASE_URL`. The repo's `docker-compose.yml` exposes a
`postgres:16-alpine` container on host port `55432`:

```
docker compose up -d postgres
export DATABASE_URL="postgres://rio:rio@localhost:55432/rio_test"
npx spago run -p rio-example-todo-api
```

The server prints `todo-api: listening on http://localhost:8080`
once ready (and HTTPurple prints its own banner). Smoke test:

```
# reads are public
curl -s localhost:8080/todos

# mutations require Authorization: Bearer example-token
curl -s -X POST -H 'Authorization: Bearer example-token' \
     -d '{"title":"buy milk"}' localhost:8080/todos
curl -s -X POST -H 'Authorization: Bearer example-token' \
     -d '{"title":"read book"}' localhost:8080/todos

curl -s localhost:8080/todos
curl -s localhost:8080/todos/1
curl -s -X DELETE -H 'Authorization: Bearer example-token' \
     localhost:8080/todos/1

# unauthorized: 401
curl -s -X POST -d '{"title":"x"}' localhost:8080/todos

# 404 with the request id echoed in the body
curl -s localhost:8080/todos/999

# 400 on bad JSON
curl -s -X POST -H 'Authorization: Bearer example-token' \
     -d 'not-json' localhost:8080/todos

# 405 on the wrong method
curl -s -X PUT localhost:8080/todos

# correlate logs by passing an inbound request id
curl -s -H 'X-Request-Id: trace-abc-123' localhost:8080/todos
```

Every request prints two log lines (`request received`,
`request completed` / `request failed`) plus any domain lines
the handler emits. Every line carries `request.id`,
`request.method`, and `request.path` so logs from a single
request can be grouped without touching the handler code:

```
[INFO] request received  request.id=req-4, request.method=Post, request.path=/todos
[INFO] create todo       request.id=req-4, request.method=Post, request.path=/todos, todo.title=milk
[INFO] request completed request.id=req-4, request.method=Post, request.path=/todos, duration_ms=0.0, result=ok
```

If the inbound request includes an `X-Request-Id` header its
value becomes the `request.id` field; otherwise the server
assigns a monotonic `req-N`.

## What the example shows

### Handlers as `RIO` programs talking to Postgres

Each handler returns an `RIO Env ApiError a` and calls
`rio-aff-postgres` smart constructors directly. Cross-cutting
concerns like logging correlation are scoped by
`RIO.Aff.Logger.withFields`, not threaded through every call:

```purescript
getHandler :: Int -> RIO Env ApiError Todo
getHandler tid = withFields [ Tuple "todo.id" (show tid) ] do
  logInfo "fetch todo"
  row <- queryParams dbTag
    "select id, title, done, created_at_ms from rio_todos where id = $1"
    tid
  case (row :: Maybe TodoRow) of
    Nothing -> fail (Proxy :: Proxy "notFound") { id: tid }
    Just r -> pure (rowToTodo r)
```

A handler never opens a connection by hand, never reads from
`Effect.Now` directly, and never produces a `[INFO]` prefix.
All of that lives in the layer or the middleware.
`queryParams` binds `$1` safely via the driver, so user-
controlled values (path segments, JSON fields) flow into SQL
without string concatenation.

### Middleware as `RIO` combinators

`withRequestContext` is the pattern this example exists to
demonstrate. It wraps any `RIO Env ApiError a` so the wrapped
action runs inside a `withFields` block stamping the request
id / method / path, with a `locally` write into the `Local
String` so domain code can read the current request id, and
with a "received" / "completed" pair around the body that
times the action and records its success / failure verdict:

```purescript
withRequestContext ctx action = withFields
  [ Tuple "request.id" ctx.requestId
  , Tuple "request.method" (show ctx.method)
  , Tuple "request.path" ctx.path
  ]
  do
    reqIdLocal <- ask (Proxy :: Proxy "requestId")
    locally reqIdLocal ctx.requestId (timed action)
```

`requireAuth` is the same shape, just shorter: it inspects the
captured headers and either falls through or raises the
`unauthorized` typed failure on the error row.

### One layer for the process, one pool for every request

`main` runs the entire program under
`provideLayer (appLayer connStr)`. The Postgres pool allocated
by `postgresLayer` lives for the lifetime of the surrounding
scope; the request loop runs inside that scope, so every
request borrows a connection from the same pool and returns it
when the handler finishes. The pool's `end()` is registered as
the layer's finalizer.

The HTTP listener is registered on Node's event loop, then
`boot` calls `parkForever` to hold the layer's scope open for
as long as the process lives. On `SIGINT`/`SIGTERM` the OS
reaps the process; for an interactive long-running server that
is the natural shutdown shape.

### Typed failures hit one place

`ApiError` is
`(notFound :: { id :: Int }, unauthorized :: Unit, db :: PgError)`.
`runRIO` returns an `Either (Variant ApiError) a`, and
`renderApiError` matches the variant exactly once to choose an
HTTP status:

```purescript
renderApiError reqId = Variant.match
  { notFound: \{ id } ->
      response Status.notFound
        ("todo " <> show id <> " not found (request " <> reqId <> ")")
  , unauthorized: \_ ->
      response Status.unauthorized
        ("missing or invalid Authorization header (request " <> reqId <> ")")
  , db: \pgErr ->
      response Status.internalServerError
        ("database error (request " <> reqId <> "): " <> pgErrorMessage pgErr)
  }
```

Adding a new typed failure (`rateLimited`, `validationFailed`,
...) means adding a tag to `ApiError` and a case here; the
compiler errors on every handler call site until the new tag
is handled.

## Where to go from here

Swap the in-memory `requestId` counter for a uuid generator,
add structured-log forwarding via `RIO.Aff.Tracer`, or drop in a
prepared-statement variant of the handlers, all without
touching the routes, the codecs, or `renderApiError`.
