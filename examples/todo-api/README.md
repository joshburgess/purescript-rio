# todo-api

A small HTTP service built with [HTTPurple](https://pursuit.purescript.org/packages/purescript-httpurple/4.0.0)
and `rio`. Listens on `localhost:8080`, persists todos in memory.

The example demonstrates the full v0.1 RIO surface in a real
shape:

- **Services** (`src/Example/TodoApi/Services.purs`): `Logger`,
  `TodoStore`, and `Clock` (the production `RIO.Clock` service
  re-exported for one-stop import).
- **Layers** (`src/Example/TodoApi/Layers.purs`): `appLayer`
  horizontally combines a static `consoleLogger` with a
  resourceful `inMemoryStore` built from `Ref`s.
- **Handlers** (`src/Example/TodoApi/Handlers.purs`): each
  endpoint is an `RIO` action over the shared service row, with
  a single typed failure `notFound` in the error row.
- **JSON codecs** (`src/Example/TodoApi/Codecs.purs`): argonaut
  encoders/decoders wrapped in HTTPurple's `JsonEncoder` /
  `JsonDecoder` newtypes.
- **Routes** (`src/Example/TodoApi/Routes.purs`): a
  `Routing.Duplex` route definition for the two URL shapes
  (`/todos` and `/todos/:id`).
- **Wiring** (`src/Example/TodoApi/Main.purs`): builds the layer
  once at startup, installs an HTTPurple router, runs each
  request as an `RIO` action and renders the result.

## Running

From the workspace root:

```
npx spago run -p rio-example-todo-api
```

The server prints `todo-api: listening on http://localhost:8080`
once ready. Smoke test:

```
curl -s localhost:8080/todos
curl -s -X POST -d '{"title":"buy milk"}' localhost:8080/todos
curl -s -X POST -d '{"title":"read book"}' localhost:8080/todos
curl -s localhost:8080/todos
curl -s localhost:8080/todos/1
curl -s -X DELETE localhost:8080/todos/1
curl -s localhost:8080/todos/1      # 404
curl -s -X POST -d 'not-json' localhost:8080/todos   # 400
curl -s -X PUT localhost:8080/todos                  # 405
```

Each request that hits a handler logs a single line to stdout
through the `Logger` service.

## What the example shows

### Handlers as `RIO` programs

Each handler reads services it needs via `ask` and returns an
`RIO Env ApiError a`:

```purescript
getHandler :: Int -> RIO Env ApiError Todo
getHandler tid = do
  logger <- ask (Proxy :: Proxy "logger")
  store <- ask (Proxy :: Proxy "todoStore")
  liftAff (logger.log ("GET /todos/" <> show tid))
  row <- liftAff (store.get tid)
  case row of
    Nothing -> fail (Proxy :: Proxy "notFound") { id: tid }
    Just todo -> pure todo
```

A handler never builds its own services, never opens a database
connection, never reads a clock from `Effect.Now` directly. All
that is the layer's job.

### One layer build, many requests

`main` calls `buildLayer appLayer` once, then captures the
resulting record in a closure for every request:

```purescript
built <- buildLayer appLayer
case built of
  Left _ -> log "todo-api: layer build failed"
  Right base -> do
    let env = { logger: base.logger, todoStore: base.todoStore, clock: liveClock }
    _ <- serve { port: 8080 } { route, router: mkRouter env }
    ...
```

The `inMemoryStore` allocated inside the layer survives for the
process's lifetime; the same `Ref` backs every request.

### Typed failures hit one place

The `notFound` tag flows up through every handler that may
raise it; `runRIO` returns an `Either (Variant ApiError) a`,
and `renderApiError` matches the variant exactly once to choose
an HTTP status:

```purescript
renderApiError :: Variant ApiError -> ResponseM
renderApiError = Variant.match
  { notFound: \{ id } -> response Status.notFound ("todo " <> show id <> " not found")
  }
```

Adding a new typed failure (`unauthorized`, `rateLimited`, ...)
means adding a tag to `ApiError` and a case to `renderApiError`;
the compiler errors on the handler call site until the new tag
is handled.

## Where to go from here

The example deliberately keeps persistence in-memory so it has
zero external dependencies. To swap to a real database without
touching the handlers, replace `inMemoryStore` with a layer
that returns the same `TodoStore` interface backed by your
driver of choice. The handlers, the JSON codecs, the routes,
and `renderApiError` all stay the same.
