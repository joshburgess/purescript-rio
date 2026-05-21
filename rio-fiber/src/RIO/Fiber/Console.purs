-- | A thin `RIO` wrapper over `Effect.Console`.
-- |
-- | This module exists so that small programs and examples can do
-- | basic stdout I/O without having to drop down to `liftEffect`
-- | for every call. For structured, level-tagged logging with
-- | field annotations, reach for `RIO.Fiber.Logger` instead.
-- |
-- | The error row is left polymorphic on every function: console
-- | writes can fail at the JS level but the failures surface as
-- | defects (uncaught exceptions), not typed errors.
-- |
-- | ```purescript
-- | example :: forall r e. RIO r e Unit
-- | example = do
-- |   log "starting"
-- |   logShow [ 1, 2, 3 ]
-- |   warn "slow path"
-- | ```
module RIO.Fiber.Console
  ( log
  , logShow
  , warn
  , warnShow
  , error
  , errorShow
  , info
  , infoShow
  , debug
  , debugShow
  ) where

import Prelude

import Effect.Console as Console

import RIO.Fiber.Core (RIO, liftEffect)

-- | Write a line at the default level.
log :: forall r e. String -> RIO r e Unit
log s = liftEffect (Console.log s)

-- | `log` via `Show`.
logShow :: forall r e a. Show a => a -> RIO r e Unit
logShow a = liftEffect (Console.logShow a)

-- | Write a warning.
warn :: forall r e. String -> RIO r e Unit
warn s = liftEffect (Console.warn s)

-- | `warn` via `Show`.
warnShow :: forall r e a. Show a => a -> RIO r e Unit
warnShow a = liftEffect (Console.warnShow a)

-- | Write an error message (does not raise a typed failure or
-- | defect; this is purely about stderr output).
error :: forall r e. String -> RIO r e Unit
error s = liftEffect (Console.error s)

-- | `error` via `Show`.
errorShow :: forall r e a. Show a => a -> RIO r e Unit
errorShow a = liftEffect (Console.errorShow a)

-- | Write an informational message.
info :: forall r e. String -> RIO r e Unit
info s = liftEffect (Console.info s)

-- | `info` via `Show`.
infoShow :: forall r e a. Show a => a -> RIO r e Unit
infoShow a = liftEffect (Console.infoShow a)

-- | Write a debug-level message.
debug :: forall r e. String -> RIO r e Unit
debug s = liftEffect (Console.debug s)

-- | `debug` via `Show`.
debugShow :: forall r e a. Show a => a -> RIO r e Unit
debugShow a = liftEffect (Console.debugShow a)
