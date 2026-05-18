-- | A minimal logger service example.
-- |
-- | The service is a record of `Aff`-valued operations. Each smart
-- | constructor (`info` / `warn` / `err`) reads the record from the
-- | environment and `liftAff`s the chosen operation into `RIO`. This is
-- | the idiomatic shape for an `RIO` service and is what
-- | `docs/02-services.md` walks through.
-- |
-- | A common temptation is to make the operation polymorphic over the
-- | calling monad (`forall m. MonadAff m => ... -> m Unit`). Don't.
-- | PureScript's row machinery struggles to project a rank-N field out
-- | of a record at a concrete type, so inference falls over at the call
-- | site. Keep service operations at concrete `Aff` (or `Effect`) and
-- | lift them in the smart constructors.
module Example.Logger
  ( Logger
  , LogLevel(..)
  , consoleLogger
  , info
  , warn
  , err
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, ask)

-- | A log message's severity. Used by the example `consoleLogger`'s
-- | prefix; richer implementations might filter or route by level.
data LogLevel = Info | Warn | Error

derive instance eqLogLevel :: Eq LogLevel

-- | The service record. Operations are `Aff`-valued so the field is
-- | monomorphic; smart constructors lift them into `RIO`.
type Logger =
  { log :: LogLevel -> String -> Aff Unit
  }

-- | A console-backed implementation. Each call prints to stdout with a
-- | level prefix. Production code would pass this through a real logger
-- | (timestamped, JSON-shaped, level-filtered) but the shape is the
-- | same: build the record, hand it to `provide` or `provideAll`.
consoleLogger :: Logger
consoleLogger =
  { log: \lvl msg -> Console.log (prefix lvl <> msg)
  }
  where
  prefix Info = "[info]  "
  prefix Warn = "[warn]  "
  prefix Error = "[error] "

-- | Smart constructors that already lift into `RIO`. Users write
-- | `info "hello"` rather than `do l <- ask _; liftAff (l.log Info "hello")`.
info
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
info = withLevel Info

warn
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
warn = withLevel Warn

err
  :: forall r e
   . String
  -> RIO (logger :: Logger | r) e Unit
err = withLevel Error

withLevel
  :: forall r e
   . LogLevel
  -> String
  -> RIO (logger :: Logger | r) e Unit
withLevel lvl msg = do
  logger <- ask (Proxy :: Proxy "logger")
  liftAff (logger.log lvl msg)
