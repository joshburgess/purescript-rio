-- | A swappable logging service.
-- |
-- | Production code emits log messages via `log` / `logAt` rather
-- | than calling `Effect.Console` directly. Tests can swap the
-- | implementation with `withLogger` to capture messages in a `Ref`
-- | and make assertions on what was logged.
-- |
-- | The default implementation writes a single line per message to
-- | the JS `console` at the appropriate severity. The current
-- | implementation lives in a module-level `FiberRef`, so a
-- | `withLogger` block scopes the override to the wrapped action
-- | and any child fibers forked from inside it.
module RIO.Fiber.Logger
  ( Logger(..)
  , LogLevel(..)
  , annotateLogs
  , defaultLogger
  , log
  , logAt
  , trace
  , logTrace
  , debug
  , info
  , warn
  , error
  , withLogger
  , getLogger
  , setLogger
  ) where

import Prelude

import Data.Array as Array
import Data.String (joinWith) as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log) as Console
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Internal (FiberRef)
import RIO.Fiber.Ref (getFiberRef, locally, newFiberRef, setFiberRef)

-- | Standard log levels, ordered from least to most severe.
-- | `Trace` is the noisiest band, useful for fine-grained
-- | per-step instrumentation; production deployments typically
-- | filter it out.
data LogLevel = Trace | Debug | Info | Warn | Error

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

instance showLogLevel :: Show LogLevel where
  show Trace = "TRACE"
  show Debug = "DEBUG"
  show Info = "INFO"
  show Warn = "WARN"
  show Error = "ERROR"

-- | A logger implementation. The single primitive is `emit`; the
-- | wrappers in this module fix the level.
newtype Logger = Logger
  { emit :: LogLevel -> String -> Effect Unit
  }

-- | The default logger: writes one console line per message at the
-- | level's severity (`debug`, `info`, `warn`, or `error`).
defaultLogger :: Logger
defaultLogger = Logger
  { emit: \level msg -> Console.log ("[" <> show level <> "] " <> msg)
  }

loggerRef :: FiberRef Logger
loggerRef = unsafePerformEffect (newFiberRef defaultLogger)

-- | Emit a message at the given log level via the active logger.
logAt :: forall r e. LogLevel -> String -> RIO r e Unit
logAt level msg = do
  Logger l <- getFiberRef loggerRef
  liftEffect (l.emit level msg)

-- | Emit a message at `Info` level.
log :: forall r e. String -> RIO r e Unit
log = logAt Info

-- | Emit a message at `Trace` level. The noisiest band; intended
-- | for fine-grained instrumentation that is normally filtered
-- | out in production.
trace :: forall r e. String -> RIO r e Unit
trace = logAt Trace

-- | Alias for `trace`, matching rio-aff's `logTrace` name.
logTrace :: forall r e. String -> RIO r e Unit
logTrace = trace

debug :: forall r e. String -> RIO r e Unit
debug = logAt Debug

info :: forall r e. String -> RIO r e Unit
info = logAt Info

warn :: forall r e. String -> RIO r e Unit
warn = logAt Warn

error :: forall r e. String -> RIO r e Unit
error = logAt Error

-- | Read the active logger implementation.
getLogger :: forall r e. RIO r e Logger
getLogger = getFiberRef loggerRef

-- | Replace the active logger for the current fiber and its
-- | descendants.
setLogger :: forall r e. Logger -> RIO r e Unit
setLogger = setFiberRef loggerRef

-- | Run `body` with `logger` as the active logger, restoring the
-- | previous implementation on exit.
withLogger :: forall r e a. Logger -> RIO r e a -> RIO r e a
withLogger logger body = locally loggerRef logger body

-- | Tag every log message emitted by `body` with the given
-- | annotations. The annotations are rendered as
-- | `key=value key=value ` and prepended to the message string the
-- | active logger receives.
-- |
-- | Annotations nest: an inner `annotateLogs` extends (rather than
-- | replaces) the surrounding annotation set, and the outer
-- | annotations are restored when the inner block exits.
-- |
-- | The decoration happens by swapping the active logger via
-- | `withLogger`, so the body's nested `withLogger` calls win:
-- | replacing the logger inside an `annotateLogs` block drops the
-- | annotations for the duration of that inner block. Pass the
-- | annotations to your replacement logger explicitly if you need
-- | both.
annotateLogs
  :: forall r e a
   . Array (Tuple String String)
  -> RIO r e a
  -> RIO r e a
annotateLogs annotations body
  | Array.null annotations = body
  | otherwise = do
      Logger l <- getLogger
      let
        rendered =
          String.joinWith " "
            (map (\(Tuple k v) -> k <> "=" <> v) annotations)
        wrapped = Logger
          { emit: \level msg -> l.emit level (rendered <> " " <> msg)
          }
      withLogger wrapped body
