-- | LISTEN / NOTIFY adapter for `rio-fiber-postgres`.
-- |
-- | Postgres lets a client subscribe to a named channel and
-- | receive asynchronous payloads any time another connection runs
-- | `NOTIFY channel, 'payload'`. The subscriber must hold a
-- | dedicated long-lived connection because the LISTEN state is
-- | per-connection: a pooled client returning to the pool would
-- | lose its subscriptions.
-- |
-- | This module wraps that pattern as `Notify`, a service token
-- | produced by `notifyLayer` (in `RIO.Fiber.Postgres.Notify.Layer`).
-- | The layer owns one dedicated `node-postgres` client; finalizers
-- | drain the client and tear down the dispatch hookup when the
-- | surrounding scope exits.
-- |
-- | Two combinators live here:
-- |
-- |   * `withListen` runs an inner program with a callback
-- |     registered against a channel. LISTEN fires on entry,
-- |     UNLISTEN on exit, no matter how the inner action returned
-- |     (success, typed failure, defect, or external kill).
-- |
-- |   * `notify` sends a payload through the regular pool by way
-- |     of `select pg_notify($1, $2)`, so values from user input
-- |     are parameter-bound rather than concatenated into SQL.
-- |
-- | Multiple `withListen` blocks can target the same channel; each
-- | callback fires for every incoming notification, and `UNLISTEN`
-- | only runs when the final subscriber leaves.
module RIO.Fiber.Postgres.Notify
  ( Notify(..)
  , Notification
  , withListen
  , notify
  , notifyUsing
  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll) as String
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as EC
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy)

import Effect.Aff.Postgres.Client (Client, Notification, exec) as PG
import Effect.Postgres.Error.Except (Except) as PG

import RIO.Fiber.Aff (fromAff)
import RIO.Fiber.Core (RIO, ask, liftEffect)
import RIO.Fiber.Scope (acquireRelease, scoped)

import RIO.Fiber.Postgres (PgError, Postgres, execParams, execParamsUsing)
import RIO.Fiber.Postgres as PG.RIO

-- | The Notify service token. Holds the dedicated subscriber
-- | client plus a dispatch table indexed by channel name and an
-- | id counter for per-subscription removal.
-- |
-- | Build one with `RIO.Fiber.Postgres.Notify.Layer.notifyLayer`.
newtype Notify = Notify
  { client :: PG.Client
  , subscribers :: Ref (Map String (Map Int (Notification -> Effect Unit)))
  , nextId :: Ref Int
  }

-- | A notification raised by `NOTIFY`. Re-exports the upstream
-- | record shape verbatim:
-- |
-- | ```purescript
-- | type Notification =
-- |   { processId :: Number
-- |   , channel :: String
-- |   , payload :: Maybe String
-- |   }
-- | ```
type Notification = PG.Notification

-- | Run `program` with `handler` subscribed to `channel`. Issues
-- | `LISTEN "channel"` on the subscriber client when the first
-- | handler for a channel joins, runs `program`, then unsubscribes
-- | the handler on exit. If the last handler for a channel leaves,
-- | the corresponding `UNLISTEN "channel"` runs too.
-- |
-- | The handler runs in `Effect`. If the handler needs to perform
-- | `Aff` or `RIO` work, capture the env in the closure and
-- | dispatch via `launchAff_` / `runRIOCallback`.
-- |
-- | Cleanup is bracketed and runs on every exit path. Typed
-- | failures from `program` re-raise on the caller's tag after
-- | cleanup.
withListen
  :: forall sym r e e' a
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> String
  -> (Notification -> Effect Unit)
  -> RIO (notify :: Notify | r) e a
  -> RIO (notify :: Notify | r) e a
withListen sym channel handler program = do
  { notify: Notify rec } <- ask
  scoped \scope -> do
    _ <- acquireRelease scope
      (registerSubscriber sym rec channel handler)
      (\sid -> fromAff (removeSubscriber rec channel sid))
    program

-- | Send a Postgres notification on the regular pool. The channel
-- | and payload are bound as parameters to `pg_notify`, so values
-- | from user input are safe to pass through verbatim.
-- |
-- | Subscribers attached via `withListen` (on this database, on
-- | any connection) receive the payload through their handler.
notify
  :: forall sym r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> String
  -> String
  -> RIO (postgres :: Postgres | r) e Unit
notify sym channel payload = do
  _ <- execParams sym
    "select pg_notify($1, $2)"
    (channel /\ payload)
  pure unit

-- | Variant of `notify` that runs `pg_notify` on a supplied client
-- | rather than borrowing one from the pool. Use inside
-- | `withTransaction` to make a notification part of the same
-- | transaction's commit: the NOTIFY is held until the transaction
-- | commits, and rolled back with the rest of the transaction if
-- | the body fails. The pool-borrowing `notify` instead sends on a
-- | fresh connection, which races the surrounding transaction's
-- | commit and is visible to subscribers immediately.
notifyUsing
  :: forall sym r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> String
  -> String
  -> PG.Client
  -> RIO r e Unit
notifyUsing sym channel payload client = do
  _ <- execParamsUsing sym
    "select pg_notify($1, $2)"
    (channel /\ payload)
    client
  pure unit

-- | Add `handler` to the dispatch table under `channel`. If this
-- | is the first handler for the channel, issue `LISTEN "channel"`
-- | on the subscriber client. Returns the freshly minted
-- | subscriber id so cleanup can target this exact handler.
registerSubscriber
  :: forall sym r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> { client :: PG.Client
     , subscribers :: Ref (Map String (Map Int (Notification -> Effect Unit)))
     , nextId :: Ref Int
     }
  -> String
  -> (Notification -> Effect Unit)
  -> RIO r e Int
registerSubscriber sym rec channel handler = do
  sid <- liftEffect (Ref.modify (_ + 1) rec.nextId)
  isFirst <- liftEffect do
    table <- Ref.read rec.subscribers
    let existing = fromMaybe Map.empty (Map.lookup channel table)
    let next = Map.insert sid handler existing
    Ref.write (Map.insert channel next table) rec.subscribers
    pure (Map.isEmpty existing)
  when isFirst (PG.RIO.execUsing sym (("listen " <> quoteIdent channel) :: String) rec.client >>= \_ -> pure unit)
  pure sid

-- | Remove the handler keyed by `sid` from the dispatch table. If
-- | that drained the channel's subscriber set, issue `UNLISTEN`.
-- | Errors from UNLISTEN are swallowed: the cleanup path is
-- | already best-effort.
removeSubscriber
  :: { client :: PG.Client
     , subscribers :: Ref (Map String (Map Int (Notification -> Effect Unit)))
     , nextId :: Ref Int
     }
  -> String
  -> Int
  -> Aff Unit
removeSubscriber rec channel sid = do
  drained <- EC.liftEffect do
    table <- Ref.read rec.subscribers
    case Map.lookup channel table of
      Nothing -> pure false
      Just inner -> do
        let next = Map.delete sid inner
        if Map.isEmpty next then do
          Ref.write (Map.delete channel table) rec.subscribers
          pure true
        else do
          Ref.write (Map.insert channel next table) rec.subscribers
          pure false
  when drained do
    _ <- runExceptT
      ( PG.exec (("unlisten " <> quoteIdent channel) :: String) rec.client
          :: PG.Except Aff Int
      )
    pure unit

-- | Quote a Postgres identifier (channel name) for safe inclusion
-- | in `LISTEN` / `UNLISTEN`. Doubles embedded quotes per the
-- | standard identifier-quoting rule.
quoteIdent :: String -> String
quoteIdent s = "\"" <> escape s <> "\""
  where
  escape = String.replaceAll
    (String.Pattern "\"")
    (String.Replacement "\"\"")
