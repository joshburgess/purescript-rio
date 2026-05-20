-- | Layer wiring for the `Notify` service.
-- |
-- | `notifyLayer` allocates a dedicated `node-postgres` client
-- | from a config record, connects it, attaches a global
-- | notification dispatcher to that client, and registers the
-- | client's shutdown plus listener removal as finalizers on the
-- | surrounding scope.
-- |
-- | The dispatcher reads the `Notify`'s subscribers table on every
-- | incoming notification and invokes every callback registered
-- | for `notification.channel`. Subscribers are added and removed
-- | by `RIO.Aff.Postgres.Notify.withListen`.
-- |
-- | The error row is left polymorphic: connect failures are
-- | surfaced as defects (Aff exceptions) rather than typed
-- | failures, to keep the layer's input row pinned to `()` and
-- | composable horizontally with `postgresLayer`. A bad
-- | connection string is a programmer error at startup, not a
-- | runtime condition to recover from on the typed-failure row.
module RIO.Aff.Postgres.Notify.Layer
  ( notifyLayer
  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.EventEmitter (on) as EE
import Prim.Row (class Union)

import Effect.Aff.Postgres.Client (connect, end, notificationE) as PG.Client
import Effect.Postgres.Client (Config, make) as PG.Client

import RIO.Aff.Core (RIO)
import RIO.Aff.Env (ask)
import RIO.Aff.Layer (Layer, fromRIO)
import RIO.Aff.Resource (Scope, addFinalizer)
import Type.Proxy (Proxy(..))

import RIO.Aff.Postgres.Notify (Notify(..))

-- | Build a `Notify` service from a Postgres client config. The
-- | client is created and connected eagerly so the subscriber is
-- | ready by the time `withListen` runs; connection failure is a
-- | defect.
-- |
-- | ```purescript
-- | let
-- |   layer = postgresLayer { connectionString }
-- |     <+> notifyLayer { connectionString }
-- | runRIO' (provideLayer layer program)
-- | ```
notifyLayer
  :: forall @config @missing @trash rIn
   . Union config missing (PG.Client.Config trash)
  => Record config
  -> Layer rIn () (notify :: Notify)
notifyLayer cfg = fromRIO acquire
  where
  acquire :: RIO (scope :: Scope | rIn) () { notify :: Notify }
  acquire = do
    client <- liftEffect (PG.Client.make @config @missing @trash cfg)
    liftAff (PG.Client.connect client)
    subscribers <- liftEffect (Ref.new Map.empty)
    nextId <- liftEffect (Ref.new 0)
    removeListener <- liftEffect
      ( EE.on PG.Client.notificationE
          ( \notification -> do
              table <- Ref.read subscribers
              case Map.lookup notification.channel table of
                Nothing -> pure unit
                Just inner -> for_ (Map.values inner)
                  (\h -> h notification)
          )
          client
      )
    scope <- ask (Proxy :: Proxy "scope")
    addFinalizer scope
      ( do
          liftEffect removeListener
          _ <- liftAff (runExceptT (PG.Client.end client))
          pure unit
      )
    pure
      { notify: Notify { client, subscribers, nextId }
      }
