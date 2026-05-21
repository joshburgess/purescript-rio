-- | A service for the bits of the host process environment
-- | application code reads at runtime: environment variables,
-- | command-line arguments, and the current working directory.
-- |
-- | The live implementation delegates to `Node.Process`; the
-- | test implementation in `RIO.Fiber.Test.System` is a mutable
-- | snapshot so tests can simulate `lookupEnv` returning a value,
-- | then later returning something else, without touching the
-- | host process.
-- |
-- | `RIO.Fiber.Config` covers the parse-the-environment-once-at-startup
-- | path: snapshot the env into a `Source`, decode the
-- | application's config record, and never read the env again.
-- | `RIO.Fiber.System` is for the (rarer) cases where a handler needs
-- | to consult an env variable mid-request, where a CLI needs
-- | `argv`, or where a script needs `cwd`.
-- |
-- | ```purescript
-- | handler :: forall r e. RIO (system :: System | r) e Response
-- | handler = do
-- |   home <- System.lookupEnv "HOME"
-- |   ...
-- | ```
module RIO.Fiber.System
  ( System
  , lookupEnv
  , getArgs
  , getCwd
  , liveSystem
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Node.Process as Node
import Type.Proxy (Proxy(..))

import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Env (askAt)

-- | The service record. Three operations cover the common
-- | runtime-host queries.
-- |
-- |   * `lookupEnv` returns the value of an environment variable
-- |     or `Nothing` if it is unset.
-- |   * `getArgs` returns the process argument vector exactly as
-- |     the host gave it (typically `[node, script.js, ...]`).
-- |   * `getCwd` returns the current working directory.
type System =
  { lookupEnv :: String -> Effect (Maybe String)
  , getArgs :: Effect (Array String)
  , getCwd :: Effect String
  }

-- | Look up an environment variable.
lookupEnv
  :: forall r e
   . String
  -> RIO (system :: System | r) e (Maybe String)
lookupEnv k = do
  s <- askAt (Proxy :: Proxy "system")
  liftEffect (s.lookupEnv k)

-- | The process argument vector.
getArgs
  :: forall r e
   . RIO (system :: System | r) e (Array String)
getArgs = do
  s <- askAt (Proxy :: Proxy "system")
  liftEffect s.getArgs

-- | The current working directory.
getCwd
  :: forall r e
   . RIO (system :: System | r) e String
getCwd = do
  s <- askAt (Proxy :: Proxy "system")
  liftEffect s.getCwd

-- | The live implementation, backed by `Node.Process`. Provide it
-- | via `provideAt` / `provideAll` or a `Layer`.
liveSystem :: System
liveSystem =
  { lookupEnv: Node.lookupEnv
  , getArgs: Node.argv
  , getCwd: Node.cwd
  }
