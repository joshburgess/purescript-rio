-- | A structured-logging service.
-- |
-- | A `Logger` is a small record of operations: a `log` that
-- | takes a level, a message, and a flat array of key / value
-- | fields; plus `getAnnotations` / `setAnnotations` for the
-- | ambient annotations that `withFields` scopes to a block.
-- |
-- | The intended use is the same as ZIO `ZLogger` /
-- | `Effect.logAnnotations`: snapshot a few correlation values
-- | at the top of a request (request ID, tenant ID, user ID),
-- | log freely throughout, and let the ambient annotations
-- | flow through to every emitted line.
-- |
-- | ```purescript
-- | handleRequest req = withFields
-- |   [ Tuple "request.id" req.id
-- |   , Tuple "request.path" req.path
-- |   ] do
-- |     logInfo "received"
-- |     resp <- processRequest req
-- |     logInfo ("responded with " <> show resp.status)
-- |     pure resp
-- | ```
-- |
-- | Both `logInfo` lines automatically carry `request.id` and
-- | `request.path`. After `withFields` exits, the previous
-- | annotation set (typically empty, or whatever an outer
-- | `withFields` had set) is restored.
-- |
-- | ## Concurrency and fork inheritance
-- |
-- | Annotations are stored in an `Effect.Ref` inside the
-- | `Logger` record. A forked fiber that emits a log line reads
-- | whatever annotations are current at emission time; writes
-- | from any fiber are visible to every fiber. This is the
-- | same trade-off `RIO.Tracer` and `RIO.Local` document: it
-- | works correctly for the common pattern (snapshot at the
-- | top, await children before `withFields` exits) and is not
-- | the per-fiber isolation that ZIO's runtime provides. See
-- | `docs/11-fiber-local.md` for the longer discussion.
-- |
-- | ## Backends
-- |
-- |   - `noopLogger`: discards every emission. Annotations are
-- |     still snapshotted and restored so `withFields` behaves
-- |     uniformly; the call to `log` is the only no-op.
-- |   - `consoleLogger`: writes one line per emission to
-- |     `process.stdout`. Format is
-- |     `"[LEVEL] message  key1=value1, key2=value2"`. Suitable
-- |     for local dev; in production reach for a JSON or
-- |     structured backend.
-- |   - `RIO.Test.Logger.newRecordingLogger`: captures every
-- |     emission with its merged annotation set. Use in tests
-- |     that assert on log output.
module RIO.Logger
  ( LogLevel(..)
  , Logger
  , combineLoggers
  , consoleLogger
  , filterLevel
  , formatJsonLine
  , jsonLogger
  , logDebug
  , logError
  , logInfo
  , logTrace
  , logWarn
  , noopLogger
  , withField
  , withFields
  ) where

import Prelude

import Data.Array (filter, intercalate) as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Ref as Ref
import Record (get) as Record
import Type.Proxy (Proxy(..))

import RIO.Internal (RIO(..), unRIO)

-- | The five levels mirror OTel's `SeverityNumber` family with
-- | one entry per band. `LogTrace` is the noisiest; `LogError`
-- | is the loudest non-defect signal. Unrecoverable failures
-- | belong on the defect channel (`die`), not as `LogFatal`,
-- | which is why there is no fatal entry here.
data LogLevel
  = LogTrace
  | LogDebug
  | LogInfo
  | LogWarn
  | LogError

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

instance showLogLevel :: Show LogLevel where
  show = case _ of
    LogTrace -> "LogTrace"
    LogDebug -> "LogDebug"
    LogInfo -> "LogInfo"
    LogWarn -> "LogWarn"
    LogError -> "LogError"

-- | Render a level as the short uppercase tag used by the
-- | console backend's prefix.
renderLevelTag :: LogLevel -> String
renderLevelTag = case _ of
  LogTrace -> "TRACE"
  LogDebug -> "DEBUG"
  LogInfo -> "INFO"
  LogWarn -> "WARN"
  LogError -> "ERROR"

-- | The service record. Smart constructors below read the
-- | logger out of the environment and forward to these
-- | operations.
type Logger =
  { log :: LogLevel -> String -> Array (Tuple String String) -> Effect Unit
  , getAnnotations :: Effect (Array (Tuple String String))
  , setAnnotations :: Array (Tuple String String) -> Effect Unit
  }

-- | A logger that discards every emission. Annotations are
-- | still snapshotted / restored via internal state, so
-- | `withFields` retains the right scoping behaviour and
-- | nested annotations work as expected.
noopLogger :: Effect Logger
noopLogger = do
  ref <- Ref.new []
  pure
    { log: \_ _ _ -> pure unit
    , getAnnotations: Ref.read ref
    , setAnnotations: \as -> Ref.write as ref
    }

-- | A logger that writes one line per emission to
-- | `Effect.Console.log`. Format is
-- | `"[LEVEL] message  key1=value1, key2=value2"` with the
-- | trailing field block omitted when there are no fields.
consoleLogger :: Effect Logger
consoleLogger = do
  ref <- Ref.new []
  let
    emit level msg fields = do
      let
        prefix = "[" <> renderLevelTag level <> "] " <> msg
        rendered = case fields of
          [] -> prefix
          _ -> prefix <> "  " <> renderFields fields
      Console.log rendered
  pure
    { log: emit
    , getAnnotations: Ref.read ref
    , setAnnotations: \as -> Ref.write as ref
    }

renderFields :: Array (Tuple String String) -> String
renderFields fields =
  Array.intercalate ", " (map renderOne fields)
  where
  renderOne :: Tuple String String -> String
  renderOne (Tuple k v) = k <> "=" <> v

-- | Tee every emission into both wrapped loggers. Annotations
-- | are read from the first logger; `setAnnotations` writes to
-- | both so that direct use of either child stays in sync with
-- | the combined view.
-- |
-- | Useful for fan-out backends, e.g. mirroring `consoleLogger`
-- | with a JSON file backend, or wiring a `RecordingLogger`
-- | alongside `consoleLogger` in integration tests.
-- |
-- | ```purescript
-- | logger <- liftEffect do
-- |   c <- consoleLogger
-- |   j <- jsonLogger
-- |   pure (combineLoggers c j)
-- | ```
combineLoggers :: Logger -> Logger -> Logger
combineLoggers a b =
  { log: \level msg fields -> do
      a.log level msg fields
      b.log level msg fields
  , getAnnotations: a.getAnnotations
  , setAnnotations: \as -> do
      a.setAnnotations as
      b.setAnnotations as
  }

-- | Drop emissions whose level is below `minLevel`. Annotation
-- | reads and writes pass through unchanged, so `withFields`
-- | still scopes correctly even when every emission inside is
-- | filtered away.
-- |
-- | The level ordering follows the `Ord LogLevel` instance:
-- | `LogTrace < LogDebug < LogInfo < LogWarn < LogError`.
-- |
-- | ```purescript
-- | infoOnly = filterLevel LogInfo consoleLogger
-- | ```
filterLevel :: LogLevel -> Logger -> Logger
filterLevel minLevel logger =
  { log: \level msg fields ->
      if level >= minLevel then logger.log level msg fields
      else pure unit
  , getAnnotations: logger.getAnnotations
  , setAnnotations: logger.setAnnotations
  }

-- | Render a single emission as a one-line JSON object. The
-- | shape is
-- | `{"level":"INFO","message":"...","fields":{"k":"v",...}}`.
-- | Field order matches the input array.
-- |
-- | Pure, no side effects, so it is the building block both for
-- | `jsonLogger` and for any custom JSON backend that wants to
-- | route lines somewhere other than the console.
formatJsonLine
  :: LogLevel
  -> String
  -> Array (Tuple String String)
  -> String
formatJsonLine level msg fields =
  "{\"level\":"
    <> show (renderLevelTag level)
    <> ",\"message\":"
    <> show msg
    <> ",\"fields\":"
    <> renderJsonFields fields
    <> "}"

renderJsonFields :: Array (Tuple String String) -> String
renderJsonFields fields =
  "{" <> Array.intercalate "," (map renderPair fields) <> "}"
  where
  renderPair :: Tuple String String -> String
  renderPair (Tuple k v) = show k <> ":" <> show v

-- | A logger that writes one JSON object per line to
-- | `Effect.Console.log`. The format is the one produced by
-- | `formatJsonLine`, which makes it suitable for piping into
-- | log aggregators that expect newline-delimited JSON.
jsonLogger :: Effect Logger
jsonLogger = do
  ref <- Ref.new []
  let
    emit level msg fields =
      Console.log (formatJsonLine level msg fields)
  pure
    { log: emit
    , getAnnotations: Ref.read ref
    , setAnnotations: \as -> Ref.write as ref
    }

-- | Emit at `LogTrace`. The level is the noisiest in the
-- | hierarchy; production deployments typically filter it out
-- | at the backend.
logTrace
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
logTrace = emitAt LogTrace

-- | Emit at `LogDebug`. Useful for development-time signal
-- | that is too noisy for normal operation but useful when
-- | actively troubleshooting.
logDebug
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
logDebug = emitAt LogDebug

-- | Emit at `LogInfo`. The default "something notable
-- | happened" level: request received, response sent, batch
-- | started, batch finished.
logInfo
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
logInfo = emitAt LogInfo

-- | Emit at `LogWarn`. Use for recoverable anomalies the
-- | operator should know about but that did not cause the
-- | request to fail: a retry that eventually succeeded, a
-- | deprecated code path being hit, a near-quota warning.
logWarn
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
logWarn = emitAt LogWarn

-- | Emit at `LogError`. Use for failures that callers will
-- | see, distinct from unrecoverable defects (which belong on
-- | the defect channel via `die`).
logError
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
logError = emitAt LogError

emitAt
  :: forall r e
   . LogLevel
  -> String
  -> RIO (logger :: Logger | r) e Unit
emitAt level msg = RIO \r -> do
  let logger = Record.get (Proxy :: Proxy "logger") r
  annotations <- liftEffect logger.getAnnotations
  liftEffect (logger.log level msg annotations)
  pure (Right unit)

-- | Run `action` with a single ambient field temporarily
-- | attached to every log emission. The annotation cell is
-- | scoped by the dynamic extent of `action`: forked fibers
-- | inside `action` keep the annotation in their private view
-- | regardless of when the parent's `withField` exits.
withField
  :: forall r e a
   . String
  -> String
  -> RIO (logger :: Logger | r) e a
  -> RIO (logger :: Logger | r) e a
withField key value = withFields [ Tuple key value ]

-- | Run `action` with a batch of ambient fields attached.
-- |
-- | When fields share a key with an outer `withFields` block,
-- | the inner value shadows the outer for the dynamic extent
-- | of `action`; the outer annotation set is restored on exit.
-- | Field order is preserved so the rendering backend can
-- | print them in the order they were attached.
-- |
-- | A fiber forked inside `withFields` inherits a *private*
-- | annotation cell initialised to the merged set: subsequent
-- | `withFields` activity in the parent (or the parent's exit
-- | from the surrounding block) does not bleed into the child,
-- | and the child's own annotation updates do not bleed back
-- | into the parent. This works because each `withFields` call
-- | swaps in a fresh logger record (sharing the original
-- | backend) whose `getAnnotations` / `setAnnotations` point at
-- | a per-block `Ref`, and that swapped record is what every
-- | fork inside the block captures in its environment.
withFields
  :: forall r e a
   . Array (Tuple String String)
  -> RIO (logger :: Logger | r) e a
  -> RIO (logger :: Logger | r) e a
withFields fields action = RIO \r -> do
  let logger = Record.get (Proxy :: Proxy "logger") r
  previous <- liftEffect logger.getAnnotations
  let next = mergeAnnotations previous fields
  privateRef <- liftEffect (Ref.new next)
  let
    scopedLogger = logger
      { getAnnotations = Ref.read privateRef
      , setAnnotations = \as -> Ref.write as privateRef
      }
  unRIO action (r { logger = scopedLogger })

-- | Merge a new field batch into an existing annotation list.
-- | Any keys present in `incoming` replace their counterparts
-- | in `existing`; remaining `existing` entries keep their
-- | original order; new entries from `incoming` are appended
-- | in their input order.
mergeAnnotations
  :: Array (Tuple String String)
  -> Array (Tuple String String)
  -> Array (Tuple String String)
mergeAnnotations existing incoming =
  let
    incomingKeys :: Array String
    incomingKeys = map fst incoming

    keepExisting :: Tuple String String -> Boolean
    keepExisting (Tuple k _) = not (member k incomingKeys)

    survivors :: Array (Tuple String String)
    survivors = Array.filter keepExisting existing
  in
    survivors <> incoming

member :: String -> Array String -> Boolean
member k = foldl (\acc x -> acc || x == k) false
