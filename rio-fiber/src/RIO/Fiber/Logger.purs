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
  , defaultLogger
  , log
  , logAt
  , debug
  , info
  , warn
  , error
  , withLogger
  , getLogger
  , setLogger
  ) where

import Prelude

import Effect (Effect)
import Effect.Console (log) as Console
import Effect.Unsafe (unsafePerformEffect)
import RIO.Fiber.Core (RIO, ensuring, liftEffect)
import RIO.Fiber.Internal (FiberRef)
import RIO.Fiber.Ref (getFiberRef, newFiberRef, setFiberRef)

-- | Standard log levels, ordered from least to most severe.
data LogLevel = Debug | Info | Warn | Error

derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

instance showLogLevel :: Show LogLevel where
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
withLogger logger body = do
  prev <- getLogger
  setLogger logger
  ensuring (setLogger prev) body
