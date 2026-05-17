-- Case: vertical layer composition (`>>>`, `andThen`) is asked to
-- chain two layers whose rOut / rIn rows don't match.
--
-- `configLayer` produces `(config :: Config)` but `dbLayer` requires
-- `(dsn :: String)` as input. The compiler must reject the chain:
-- the intermediate row of `(>>>)` cannot unify.
module Scratch where

import Prelude hiding ((>>>))

import RIO.Layer (Layer, fromRecord, (>>>))

type Config = { host :: String }
type Database = { query :: String }

configLayer :: forall rIn e. Layer rIn e (config :: Config)
configLayer = fromRecord { config: { host: "localhost" } }

dbLayer :: forall e. Layer (dsn :: String) e (db :: Database)
dbLayer = fromRecord { db: { query: "sql" } }

-- `>>>` needs configLayer's rOut to equal dbLayer's rIn. They don't:
-- `(config :: Config)` vs `(dsn :: String)`. The compiler must reject.
result :: forall e. Layer () e (db :: Database)
result = configLayer >>> dbLayer
