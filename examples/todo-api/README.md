# todo-api

A small HTTP service built with [HTTPurple](https://pursuit.purescript.org/packages/purescript-httpurple/4.0.0)
and `rio`. Listens on `localhost:8080`, persists todos in memory.

The example covers the production-relevant slice of the library
in a single shape you can run:

- **Services** (`src/Example/TodoApi/Services.purs`): a domain
  `TodoStore` plus the library services `Logger` (`RIO.Logger`),
  `Clock` (`RIO.Clock`), and a `Local String` request id
  (`RIO.Local`), all re-exported under one namespace.
- **Layers** (`src/Example/TodoApi/Layers.purs`): `appLayer`
  horizontally combines `RIO.Logger.consoleLogger` with a
  resourceful `inMemoryStore` built from `Ref`s.
- **Middleware** (`src/Example/TodoApi/Middleware.purs`):
  `withRequestContext` wraps every handler with a per-request
  log-annotation block and times the body. `requireAuth` is a
  bearer-token check that raises an `unauthorized` typed
  failure.
- **Handlers** (`src/Example/TodoApi/Handlers.purs`): each
  endpoint is an `RIO` action over the shared service row, with
  two typed failures (`notFound`, `unauthorized`) in the error
  row. Handlers stay domain-focused; logging, auth, and request
  IDs are layered on by the middleware.
- **JSON codecs** (`src/Example/TodoApi/Codecs.purs`): argonaut
  encoders/decoders wrapped in HTTPurple's `JsonEncoder` /
  `JsonDecoder` newtypes.
- **Routes** (`src/Example/TodoApi/Routes.purs`): a
  `Routing.Duplex` route definition for `/todos` and
  `/todos/:id`.
- **Wiring** (`src/Example/TodoApi/Main.purs`): builds the
  layer once at startup, allocates the per-process request-id
  `Local`, installs an HTTPurple router, captures HTTP-shaped
  values into a `RequestContext`, runs each request through the
  middleware wrapper.

## Running

From the workspace root:

```
npx spago run -p rio-example-todo-api
```

The server prints `todo-api: listening on http://localhost:8080`
once ready. Smoke test:

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

### Handlers as `RIO` programs

Each handler reads services it needs via `ask` and returns an
`RIO Env ApiError a`. Cross-cutting concerns like logging
correlation are scoped by `RIO.Logger.withFields`, not
threaded through every call:

```purescript
getHandler :: Int -> RIO Env ApiError Todo
getHandler tid = withFields [ Tuple "todo.id" (show tid) ] do
  store <- ask (Proxy :: Proxy "todoStore")
  logInfo "fetch todo"
  row <- liftAff (store.get tid)
  case row of
    Nothing -> fail (Proxy :: Proxy "notFound") { id: tid }
    Just todo -> pure todo
```

A handler never opens a database connection, never reads from
`Effect.Now` directly, and never produces a `[INFO]` prefix by
hand. All of that lives in the layer or the middleware.

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

### One layer build, many requests

`main` calls `buildLayer appLayer` once and captures the result
in a closure for every request. The `inMemoryStore` allocated
inside the layer survives for the process's lifetime; the same
`Ref` backs every request; the same `Local String` carries the
per-request id (overridden via `locally` per request).

### Typed failures hit one place

`ApiError` is `(notFound :: { id :: Int }, unauthorized :: Unit)`.
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
  }
```

Adding a new typed failure (`rateLimited`, `validationFailed`,
...) means adding a tag to `ApiError` and a case here; the
compiler errors on every handler call site until the new tag
is handled.

## Where to go from here

The example deliberately keeps persistence in-memory so it has
zero external dependencies. To swap to a real database without
touching the handlers, replace `inMemoryStore` with a layer
that returns the same `TodoStore` interface backed by your
driver of choice. The handlers, the JSON codecs, the routes,
the middleware, and `renderApiError` all stay the same.
