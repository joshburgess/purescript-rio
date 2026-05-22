## Structured logging

> **Naming convention.** This guide describes the **rio-aff**
> `RIO.Aff.Logger` surface in detail. The premier **rio-fiber**
> package ships `RIO.Fiber.Logger` with a parallel intent but a
> different module surface: `LogLevel` constructors are
> `Trace`/`Debug`/`Info`/`Warn`/`Error` (no `Log` prefix),
> annotations are scoped via `annotateLogs` rather than
> `withField`/`withFields`, and the active logger lives in a
> module-level `FiberRef` rather than the env row. See
> `rio-fiber/src/RIO/Fiber/Logger.purs` for that surface.

`RIO.Aff.Logger` is the service for emitting structured log
lines. A `Logger` value sits in the environment row at the
`logger` field; the smart constructors (`logTrace`, `logDebug`,
`logInfo`, `logWarn`, `logError`) pull it out, snapshot the
ambient annotation set, and forward to the backend's `log`
operation.

The annotation set is the "fields" portion of a structured log
record. It is populated by `withField` / `withFields`, which
scope a batch of `(key, value)` pairs to a block of code. Every
emission inside the block carries those fields; once the block
exits the previous annotation set is restored.

```purescript
import RIO.Aff.Logger
  ( Logger
  , logInfo
  , withField
  , withFields
  )

handleRequest req = withFields
  [ Tuple "request.id" req.id
  , Tuple "request.path" req.path
  ] do
    logInfo "received"
    resp <- processRequest req
    logInfo ("responded with " <> show resp.status)
    pure resp
```

Both `logInfo` lines automatically carry `request.id` and
`request.path` in their fields. After `withFields` exits the
previous annotation set (typically empty, or whatever an outer
`withFields` had set) is restored.

This is the same pattern as ZIO `ZLogger.withAnnotations` and
Effect `Effect.logAnnotations`: snapshot a few correlation
values at the top of a request, log freely throughout, and let
the ambient fields flow into every emitted line.

## Levels

```purescript
data LogLevel
  = LogTrace
  | LogDebug
  | LogInfo
  | LogWarn
  | LogError
```

Five levels, mirroring OTel's `SeverityNumber` family with one
entry per band:

- `LogTrace`: noisy diagnostic detail, usually filtered out in
  production.
- `LogDebug`: development-time signal, useful when actively
  troubleshooting but too noisy for normal operation.
- `LogInfo`: the default "something notable happened" level.
- `LogWarn`: recoverable anomalies the operator should know
  about: retries that succeeded, deprecated paths, near-quota
  warnings.
- `LogError`: failures that callers will see.

There is intentionally no `LogFatal`. Unrecoverable failures
belong on the defect channel (`die` in `RIO.Aff.Core`, rio-fiber:
`RIO.Fiber.Core`), not as a log level.

## API

```purescript
data LogLevel = LogTrace | LogDebug | LogInfo | LogWarn | LogError

type Logger =
  { log :: LogLevel -> String -> Array (Tuple String String) -> Effect Unit
  , getAnnotations :: Effect (Array (Tuple String String))
  , setAnnotations :: Array (Tuple String String) -> Effect Unit
  }

noopLogger :: Effect Logger
consoleLogger :: Effect Logger

logTrace, logDebug, logInfo, logWarn, logError
  :: forall r e. String -> RIO (logger :: Logger | r) e Unit

withField
  :: forall r e a
   . String
  -> String
  -> RIO (logger :: Logger | r) e a
  -> RIO (logger :: Logger | r) e a

withFields
  :: forall r e a
   . Array (Tuple String String)
  -> RIO (logger :: Logger | r) e a
  -> RIO (logger :: Logger | r) e a
```

The `Logger` record is the service. Backends provide it; user
code reaches it through the smart constructors and never opens
the record directly. `getAnnotations` / `setAnnotations` are
exposed on the record so `withFields` can implement the
snapshot / restore dance without locking the service shape.

## Backends

- `noopLogger`: discards every emission. The annotation Ref is
  still maintained so `withFields` keeps its scoping behaviour.
  Use this when you want a logger-in-the-row but no output
  (typically: tests that don't care about log content, or a
  CLI tool with a `--quiet` flag).
- `consoleLogger`: writes one line per emission to
  `Effect.Console.log`. Format is
  `"[LEVEL] message  key1=value1, key2=value2"` with the
  trailing field block omitted when there are no fields.
  Suitable for local development; in production reach for a
  JSON or structured backend.
- `RIO.Aff.Test.Logger.newRecordingLogger`: captures every emission
  with its merged annotation set into an in-memory buffer.
  Returns a `{ logger, snapshot }` pair; `snapshot` reads the
  buffer at any time. Use in tests that assert on log output.

```purescript
import RIO.Aff.Test.Logger (newRecordingLogger)

it "logs what we expect" do
  rec <- liftAff newRecordingLogger
  let
    program :: RIO (logger :: Logger) () Unit
    program = withField "request.id" "abc" (logInfo "ok")
  _ <- runRIO (provideAll { logger: rec.logger } program)
  records <- liftEffect rec.snapshot
  case records of
    [ r ] -> do
      r.level `shouldEqual` LogInfo
      r.fields `shouldEqual` [ Tuple "request.id" "abc" ]
    _ -> 1 `shouldEqual` Array.length records
```

## Annotation merging

When `withFields` is called inside an enclosing `withFields`
block, the two field sets are merged: any key present in the
inner batch shadows the corresponding outer entry; outer keys
that are not in the inner batch are preserved; new inner keys
are appended. Order is preserved so backends can render fields
in attach order.

```purescript
withFields [ Tuple "request.id" "outer", Tuple "tenant" "acme" ] do
  -- fields here: [ ("request.id", "outer"), ("tenant", "acme") ]
  withField "request.id" "inner" do
    -- fields here: [ ("tenant", "acme"), ("request.id", "inner") ]
    -- (outer "request.id" dropped, inner appended at the end)
    pure unit
  -- back to: [ ("request.id", "outer"), ("tenant", "acme") ]
```

## Restoration and termination

`withFields` restores the previous annotation set with
`Aff.finally`, so the restore runs on every termination path:
success, typed failure, defect, and fiber interruption
mid-block. This is the same guarantee `RIO.Aff.Local` and
`RIO.Aff.Tracer` make. (rio-fiber: `annotateLogs` runs through
the fiber runtime's `ensuring` finalizer rather than
`Aff.finally`, with the same observable semantics.)

## Concurrency and fork inheritance

The annotation set is stored in a `Ref` inside the `Logger`
record. A forked fiber that emits a log line reads whatever
annotations are current at emission time; writes from any
fiber are visible to every fiber. This is the implicit-context
model `RIO.Aff.Tracer` and `RIO.Aff.Local` use, and the trade-off
is the same:

- works correctly for the common pattern: snapshot annotations
  at the top of a request with `withFields`, fork child fibers
  inside the block, and `join` them before the block exits;
- does not give per-fiber isolation. A sibling fiber that
  installs its own annotations via `withFields` will overwrite
  the shared cell for the duration of its block; if a parent
  emits in that window the parent will see the sibling's
  fields.

For genuinely independent log contexts across concurrent
fibers, capture the relevant values explicitly at the fork
point and pass them as arguments. See `docs/11-fiber-local.md`
for the same discussion in the `RIO.Aff.Local` setting.

## Comparison to ZIO and Effect

| Concept                | RIO                                 | ZIO                                  | Effect                          |
| ---------------------- | ----------------------------------- | ------------------------------------ | ---------------------------------- |
| Emit at a level        | `logInfo "msg"`                     | `ZIO.logInfo("msg")`                 | `Effect.logInfo("msg")`            |
| Scoped fields          | `withFields [ ... ] action`         | `ZIO.logAnnotate("k", "v") *> ...`   | `Effect.annotateLogs("k", "v")`    |
| Backend                | a `Logger` value in the env row     | a `ZLogger` registered on the runtime| a `Logger` layer                   |
| In-memory capture      | `newRecordingLogger`                | `ZTestLogger`                        | `Logger.test`                      |
| Per-fiber isolation    | *no* (shared `Ref` of annotations)  | yes                                  | yes                                |

The single behavioural difference is the snapshot-vs-shared
fork semantics for the annotation Ref. For the everyday
"snapshot at the top, read everywhere below" pattern this is
irrelevant; for fully isolated per-fiber log contexts it
matters, and the workaround is the same as for `RIO.Aff.Local`.
(rio-fiber's `RIO.Fiber.Logger` lives in a `FiberRef` and
inherits per-fiber snapshot semantics for free; the rio-aff
workaround is unnecessary on the fiber side.)

## Pointers

- `rio-aff/src/RIO/Aff/Logger.purs` and
  `rio-fiber/src/RIO/Fiber/Logger.purs`: the module.
- `rio-aff/src/RIO/Aff/Test/Logger.purs` and
  `rio-fiber/src/RIO/Fiber/Test/Logger.purs`: the recording
  backend used in tests.
- `rio-aff/test/Test/RIO/Aff/LoggerSpec.purs`: tests for every
  level, for
  `withField` / `withFields` propagation, for restoration on
  success and on typed failure, for nested annotation
  shadowing, and for input-order preservation.
