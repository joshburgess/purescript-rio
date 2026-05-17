-- Case: horizontal layer composition (`<+>`, `combine`) is asked to
-- merge two layers whose output rows share a label.
--
-- Both `consoleLogger` and `fileLogger` produce a `logger` field. The
-- combined output row would have two `logger` entries, which is
-- ill-formed. The compiler must reject the call.
module Scratch where

import Prelude

import RIO.Layer (Layer, fromRecord, (<+>))

type Logger = { log :: String -> String }

consoleLogger :: forall rIn e. Layer rIn e (logger :: Logger)
consoleLogger = fromRecord { logger: { log: \s -> "console:" <> s } }

fileLogger :: forall rIn e. Layer rIn e (logger :: Logger)
fileLogger = fromRecord { logger: { log: \s -> "file:" <> s } }

-- The combined row has `logger` twice. `combine`'s
-- `Row.Union r1Out r2Out rOut` constraint cannot unify against the
-- caller-annotated singleton output row, so the compiler rejects.
result :: forall e. Layer () e (logger :: Logger)
result = consoleLogger <+> fileLogger
