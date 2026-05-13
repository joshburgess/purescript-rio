-- | Phase 7 review: layered application.
-- |
-- | Three layers, mirroring the Phase 5 review shape so the
-- | comparison is apples-to-apples:
-- |
-- |   * `loggerLayer` reads `clock` (passes it through) and
-- |     produces a `Logger` whose `log` operation pushes through
-- |     a caller-supplied recorder (typically the `record` field
-- |     of `RIO.Test.recording`).
-- |
-- |   * `dataLayer` reads `logger` + `clock` and produces a
-- |     `Database`, also re-emitting both upstream services. It
-- |     fails with `dbConnect` when the supplied URL is empty,
-- |     exercising the failing-layer story.
-- |
-- |   * `userServiceLayer` reads `database`, `logger`, and
-- |     `clock`, and produces a `UserService` whose `greet`
-- |     logs through `logger.log` and looks up via
-- |     `database.fetchUser`, and whose `greetAfter` first
-- |     sleeps through `clock.sleep`.
-- |
-- | The top-level `appLayer dbUrl record` strings them together
-- | so a program needs only `userService` in its environment
-- | row; `clock` is supplied at the test boundary (live or
-- | virtual).
-- |
-- | The two passthroughs (`clock` through `loggerLayer`,
-- | `logger` and `clock` through `dataLayer`) are the Phase 5
-- | DX-1 pattern: there is no implicit passthrough operator, so
-- | a layer that wants to expose upstream services to its
-- | downstream must list them in its output record.
module Spike.Phase7Review.Layers
  ( appLayer
  , dataLayer
  , loggerLayer
  , userServiceLayer
  ) where

import Prelude hiding ((>>>))

import Data.Maybe (Maybe(..))
import Data.String (length) as String
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Type.Proxy (Proxy(..))

import RIO.Core (Layer, ask, fail, fromRIO)
import RIO.Layer ((>>>))
import RIO.Clock (Clock)

import Spike.Phase7Review.Services (Database, Logger, UserService)

loggerLayer
  :: forall e
   . (String -> Aff Unit)
  -> Layer (clock :: Clock) e (logger :: Logger, clock :: Clock)
loggerLayer record = fromRIO do
  clock <- ask (Proxy :: Proxy "clock")
  pure
    { logger: { log: record }
    , clock
    }

-- | Failing-layer story: refuse to build the database when the
-- | URL is empty. Also seeds an in-memory database with three
-- | rows so `greet` has something to look up.
dataLayer
  :: String
  -> Layer
       (logger :: Logger, clock :: Clock)
       (dbConnect :: String)
       ( database :: Database
       , logger :: Logger
       , clock :: Clock
       )
dataLayer dbUrl = fromRIO do
  logger <- ask (Proxy :: Proxy "logger")
  clock <- ask (Proxy :: Proxy "clock")
  when (String.length dbUrl == 0)
    (fail (Proxy :: Proxy "dbConnect") "empty database url")
  liftAff (logger.log ("db-open url=" <> dbUrl))
  let
    database :: Database
    database =
      { fetchUser: \uid -> pure case uid of
          1 -> Just "alice"
          2 -> Just "bob"
          7 -> Just "grace"
          _ -> Nothing
      }
  pure { database, logger, clock }

userServiceLayer
  :: forall e
   . Layer
       ( database :: Database
       , logger :: Logger
       , clock :: Clock
       )
       e
       (userService :: UserService)
userServiceLayer = fromRIO do
  logger <- ask (Proxy :: Proxy "logger")
  database <- ask (Proxy :: Proxy "database")
  clock <- ask (Proxy :: Proxy "clock")
  pure
    { userService:
        { greet: \uid -> do
            logger.log ("greet uid=" <> show uid)
            row <- database.fetchUser uid
            case row of
              Nothing -> pure ("hello, stranger #" <> show uid)
              Just name -> pure ("hello, " <> name)
        , greetAfter: \ms uid -> do
            clock.sleep ms
            logger.log ("greet uid=" <> show uid)
            row <- database.fetchUser uid
            case row of
              Nothing -> pure ("hello, stranger #" <> show uid)
              Just name -> pure ("hello, " <> name)
        }
    }

-- | The complete composition. Starts from `(clock :: Clock)`
-- | rather than `()` because Clock is supplied at the test
-- | boundary (live or virtual); the layer machinery threads it
-- | through to the user service.
appLayer
  :: String
  -> (String -> Aff Unit)
  -> Layer
       (clock :: Clock)
       (dbConnect :: String)
       (userService :: UserService)
appLayer dbUrl record =
  loggerLayer record >>> dataLayer dbUrl >>> userServiceLayer
