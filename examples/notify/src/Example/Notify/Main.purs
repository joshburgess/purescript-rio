-- | A tiny program that exercises `RIO.Aff.Postgres.Notify` end to end:
-- |
-- |   1. builds a `postgresLayer + notifyLayer` over the URL in
-- |      `DATABASE_URL`,
-- |   2. subscribes a handler to `rio_notify_example` via
-- |      `withListen` (LISTEN fires on entry),
-- |   3. fires three NOTIFY payloads via `notify` on the pool,
-- |   4. waits briefly for delivery,
-- |   5. exits, letting the scope finalizer drain both clients.
-- |
-- | Run with the repo's docker-compose Postgres:
-- |
-- | ```
-- | docker compose up -d postgres
-- | export DATABASE_URL="postgres://rio:rio@localhost:55432/rio_test"
-- | npx spago run -p rio-example-notify
-- | ```
module Example.Notify.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.Process (lookupEnv)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, provideLayer, runRIO)
import RIO.Aff.Layer ((<+>))
import RIO.Aff.Postgres (PgError, Postgres, pgErrorMessage)
import RIO.Aff.Postgres.Layer (postgresLayer)
import RIO.Aff.Postgres.Notify (Notify, notify, withListen)
import RIO.Aff.Postgres.Notify.Layer (notifyLayer)

type AppRow = (postgres :: Postgres, notify :: Notify)

type AppErr = (db :: PgError)

dbTag :: Proxy "db"
dbTag = Proxy

channel :: String
channel = "rio_notify_example"

program :: RIO AppRow AppErr Unit
program = withListen dbTag channel
  ( \n -> Console.log
      ( "  received on " <> n.channel
          <> ": "
          <> fromMaybe "<no payload>" n.payload
      )
  )
  do
    liftEffect (Console.log ("listening on " <> channel))
    notify dbTag channel "hello"
    notify dbTag channel "world"
    notify dbTag channel "(empty payload follows)"
    notify dbTag channel ""
    -- give the subscriber client a moment to receive the four
    -- notifications before the surrounding scope tears it down.
    liftAff (delay (Milliseconds 500.0))
    liftEffect (Console.log "done")

main :: Effect Unit
main = do
  mConn <- lookupEnv "DATABASE_URL"
  case mConn of
    Nothing -> Console.log
      "notify-example: set DATABASE_URL (e.g. postgres://rio:rio@localhost:55432/rio_test)"
    Just conn -> launchAff_ do
      result <- runRIO
        ( provideLayer
            ( postgresLayer { connectionString: conn }
                <+> notifyLayer { connectionString: conn }
            )
            program
        )
      liftEffect case result of
        Right _ -> Console.log "notify-example: ok"
        Left v -> Console.log
          ( "notify-example: failed: "
              <> (Variant.case_ # Variant.on dbTag pgErrorMessage) v
          )
