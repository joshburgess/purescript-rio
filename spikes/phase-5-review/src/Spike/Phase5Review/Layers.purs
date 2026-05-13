-- | Phase 5 review: layered application.
-- |
-- | Six services across three layers:
-- |
-- |   * `platformLayer :: Layer () e (config, logger, clock)`
-- |     Built from `()`, combines a `Config` (`fromRecord`), a
-- |     `Logger` that records every line into an external `Ref`,
-- |     and a `Clock` whose `now` counts monotonically from a
-- |     separate `Ref`. Demonstrates horizontal composition with
-- |     `<+>` and `fromRecord` / `fromRIO`.
-- |
-- |   * `dataLayer :: Layer (config, logger, clock) (dbConnect :: String)
-- |                          (cache, database, logger, clock)`
-- |     Reads `config`, opens a `Cache` and a `Database`. Each
-- |     resource registers an `Aff` finalizer with the layer's
-- |     scope so its release is deterministic. The layer fails
-- |     with `dbConnect` when `databaseUrl` is empty, exercising
-- |     the failing-layer story. It also *re-emits* the upstream
-- |     `logger` and `clock` services so downstream layers can
-- |     still see them (we have no passthrough operator yet; see
-- |     FINDINGS.md, DX-1).
-- |
-- |   * `userServiceLayer :: Layer (cache, database, logger, clock) e
-- |                                (userService :: UserService)`
-- |     Composes cache + database + logger into the user-facing
-- |     `greet` operation.
-- |
-- | `appLayer`:
-- |
-- |   platformLayer events cfg >>> dataLayer events >>> userServiceLayer
-- |     :: Layer () (dbConnect :: String) (userService :: UserService)
module Spike.Phase5Review.Layers
  ( Events
  , appLayer
  , clockLayer
  , configLayer
  , dataLayer
  , loggerLayer
  , platformLayer
  , push
  , userServiceLayer
  ) where

import Prelude hiding ((>>>))

import Data.Array (find, snoc)
import Data.Maybe (Maybe(..))
import Data.String (length) as String
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Core
  ( Layer
  , addFinalizer
  , ask
  , fail
  , fromRIO
  , fromRecord
  )
import RIO.Layer ((<+>), (>>>))

import Spike.Phase5Review.Services
  ( Cache
  , Clock
  , Config
  , Database
  , Logger
  , UserService
  )

-- | The events ref the harness reads after each scenario. Layers
-- | push tagged strings onto it on every observable transition so the
-- | Main harness can assert exact event sequences.
type Events = Ref (Array String)

push :: forall m. MonadEffect m => Events -> String -> m Unit
push events s =
  liftEffect (Ref.modify_ (\xs -> snoc xs s) events)

configLayer :: forall e. Config -> Layer () e (config :: Config)
configLayer cfg = fromRecord { config: cfg }

loggerLayer :: forall e. Events -> Layer () e (logger :: Logger)
loggerLayer events = fromRecord
  { logger:
      { log: \s -> push events ("log:" <> s)
      }
  }

-- The clock sub-layer: an in-memory monotonic counter built from a
-- fresh `Ref` allocated inside `fromRIO`. The counter is internal
-- to the layer; we surface only the `now` operation.
clockLayer :: forall e. Events -> Layer () e (clock :: Clock)
clockLayer events = fromRIO do
  counter <- liftAff (liftEffect (Ref.new 0))
  push events "clock:init"
  pure
    { clock:
        { now: liftEffect (Ref.modify (\k -> k + 1) counter)
        }
    }

-- | Phase 5.2 horizontal composition: three independent sub-layers
-- | combined into one output row via `<+>`.
platformLayer
  :: forall e
   . Events
  -> Config
  -> Layer () e (config :: Config, logger :: Logger, clock :: Clock)
platformLayer events cfg =
  configLayer cfg <+> loggerLayer events <+> clockLayer events

-- The middle layer: reads `config` to decide whether to connect,
-- opens a cache and a database (each with its own finalizer), and
-- passes the upstream logger / clock through.
dataLayer
  :: Events
  -> Layer
       (config :: Config, logger :: Logger, clock :: Clock)
       (dbConnect :: String)
       ( cache :: Cache
       , database :: Database
       , logger :: Logger
       , clock :: Clock
       )
dataLayer events = fromRIO do
  cfg <- ask (Proxy :: Proxy "config")
  logger <- ask (Proxy :: Proxy "logger")
  clock <- ask (Proxy :: Proxy "clock")
  scope <- ask (Proxy :: Proxy "scope")

  -- Failing-layer story: refuse to open a database if the URL is
  -- empty. The error tag joins the layer's row.
  when (String.length cfg.databaseUrl == 0)
    (fail (Proxy :: Proxy "dbConnect") "empty database url")

  cacheRef <- liftAff (liftEffect (Ref.new ([] :: Array { k :: String, v :: String })))
  push events ("cache-open cap=" <> show cfg.cacheCap)
  _ <- addFinalizer scope (push events "cache-flush")

  let
    cache :: Cache
    cache =
      { get: \k -> liftEffect do
          entries <- Ref.read cacheRef
          pure (map _.v (find (\r -> r.k == k) entries))
      , put: \k v -> liftEffect
          (Ref.modify_ (\xs -> snoc xs { k, v }) cacheRef)
      }

    dbSeed =
      [ { id: 1, name: "alice" }
      , { id: 2, name: "bob" }
      , { id: 7, name: "grace" }
      ]

  dbRef <- liftAff (liftEffect (Ref.new dbSeed))
  push events ("db-open url=" <> cfg.databaseUrl)
  _ <- addFinalizer scope (push events "db-close")

  let
    database :: Database
    database =
      { fetchUser: \uid -> liftEffect do
          rows <- Ref.read dbRef
          pure (map _.name (find (\r -> r.id == uid) rows))
      }

  pure
    { cache
    , database
    , logger
    , clock
    }

-- The business-logic layer: produces a `userService` from cache +
-- database + logger + clock.
userServiceLayer
  :: forall e
   . Layer
       ( cache :: Cache
       , database :: Database
       , logger :: Logger
       , clock :: Clock
       )
       e
       (userService :: UserService)
userServiceLayer = fromRIO do
  cache <- ask (Proxy :: Proxy "cache")
  database <- ask (Proxy :: Proxy "database")
  logger <- ask (Proxy :: Proxy "logger")
  clock <- ask (Proxy :: Proxy "clock")
  pure
    { userService:
        { greet: \uid -> do
            t <- clock.now
            logger.log ("greet@" <> show t <> " uid=" <> show uid)
            cached <- cache.get (show uid)
            case cached of
              Just name -> pure ("hello again, " <> name)
              Nothing -> do
                row <- database.fetchUser uid
                case row of
                  Nothing -> pure ("hello, stranger #" <> show uid)
                  Just name -> do
                    cache.put (show uid) name
                    pure ("hello, " <> name)
        }
    }

-- | The complete composition: from no services to a `userService`,
-- | with the `dbConnect` failure mode surfaced. Built once and used
-- | from `Main.purs` under multiple scenarios.
appLayer
  :: Events
  -> Config
  -> Layer () (dbConnect :: String) (userService :: UserService)
appLayer events cfg =
  platformLayer events cfg >>> dataLayer events >>> userServiceLayer
