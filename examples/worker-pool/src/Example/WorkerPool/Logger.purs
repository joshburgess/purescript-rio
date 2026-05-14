-- | A minimal logger service for the worker-pool example. Same
-- | shape as `examples/logger`, kept local so this example has no
-- | inter-example dependency.
module Example.WorkerPool.Logger
  ( Logger
  , consoleLogger
  , info
  , warn
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask)

type Logger =
  { log :: String -> Aff Unit
  }

consoleLogger :: Logger
consoleLogger = { log: Console.log }

info :: forall r e. String -> RIO (logger :: Logger | r) e Unit
info msg = do
  l <- ask (Proxy :: Proxy "logger")
  liftAff (l.log msg)

warn :: forall r e. String -> RIO (logger :: Logger | r) e Unit
warn msg = info ("[WARN] " <> msg)
