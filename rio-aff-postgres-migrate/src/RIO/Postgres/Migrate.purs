-- | A tiny schema-migration runner.
-- |
-- | The user supplies a `Map Int Migration` indexed by an
-- | application-chosen version number. `migrate` ensures a
-- | bookkeeping table exists, opens a transaction, takes a
-- | transaction-scoped Postgres advisory lock (so two processes
-- | racing to migrate can't apply the same step twice), reads the
-- | versions that are already recorded, and runs each missing
-- | migration in version order. Each applied migration adds its
-- | version to the bookkeeping table. If any step typed-fails the
-- | whole transaction rolls back, leaving the database and the
-- | bookkeeping table consistent.
-- |
-- | The bookkeeping table is `__rio_migrations`:
-- |
-- | ```sql
-- | create table __rio_migrations
-- |   ( version integer primary key
-- |   , applied_at timestamptz not null default now()
-- |   )
-- | ```
-- |
-- | A migration is a `PG.Client -> RIO (postgres :: Postgres | r) e Unit`
-- | so each step runs on the same locked client; use
-- | `RIO.Aff.Postgres.execUsing` / `queryUsing` / `execParamsUsing`
-- | inside the callback.
-- | Convenience helper `sqlMigration` builds a migration from a
-- | single SQL string.
-- |
-- | ```purescript
-- | migrate dbTag $ Map.fromFoldable
-- |   [ 1 /\ sqlMigration dbTag
-- |       "create table users \
-- |       \( id serial primary key \
-- |       \, email text not null unique \
-- |       \)"
-- |   , 2 /\ sqlMigration dbTag
-- |       "create index users_email_idx on users (email)"
-- |   ]
-- | ```
-- |
-- | Since the whole run sits in one transaction, DDL statements
-- | that can't run inside a transaction (`CREATE INDEX
-- | CONCURRENTLY`, `VACUUM`, ...) aren't supported here. For those,
-- | run them outside `migrate`.
module RIO.Aff.Postgres.Migrate
  ( Migration
  , migrate
  , sqlMigration
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Set as Set
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy)

import Effect.Aff.Postgres.Client (Client) as PG
import RIO.Aff.Core (RIO)
import RIO.Aff.Postgres
  ( PgError
  , Postgres
  , exec
  , execParamsUsing
  , execUsing
  , queryUsing
  , withTransaction
  )

-- | A single migration step. The supplied `Client` is the
-- | transaction's client; run all SQL for this step against it via
-- | `RIO.Aff.Postgres.execUsing` / `queryUsing` / `execParamsUsing`.
-- |
-- | The migration runs inside the surrounding `migrate`
-- | transaction, so the env row already carries `postgres`.
type Migration r e = PG.Client -> RIO (postgres :: Postgres | r) e Unit

-- | A namespace constant for the advisory lock. Two processes
-- | running `migrate` against the same database will serialize on
-- | this lock; the lock is per-database, so different databases on
-- | the same cluster don't collide.
advisoryLockKey :: String
advisoryLockKey = "7235189431234567"

bookkeepingDdl :: String
bookkeepingDdl =
  "create table if not exists __rio_migrations \
  \( version integer primary key \
  \, applied_at timestamptz not null default now() \
  \)"

-- | Build a migration from a single SQL string. The SQL is sent
-- | verbatim with `execUsing`; the return value (affected-row
-- | count) is discarded.
sqlMigration
  :: forall sym r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> String
  -> Migration r e
sqlMigration sym sql client = do
  _ <- execUsing sym sql client
  pure unit

-- | Apply every migration in `migrations` whose version isn't
-- | already recorded in `__rio_migrations`. Steps run in ascending
-- | version order, inside a single transaction holding a
-- | Postgres advisory lock. Already-applied versions are skipped.
-- |
-- | The bookkeeping table is created on first run.
migrate
  :: forall sym r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> Map Int (Migration r e)
  -> RIO (postgres :: Postgres | r) e Unit
migrate sym migrations = do
  _ <- exec sym bookkeepingDdl
  withTransaction sym \client -> do
    _ <- execUsing sym
      ( ("select pg_advisory_xact_lock(" <> advisoryLockKey <> ")") :: String
      )
      client
    applied <- queryUsing sym
      ("select version from __rio_migrations" :: String)
      client
    let
      appliedSet = Set.fromFoldable (applied :: Array Int)
      pending =
        Map.toUnfoldable migrations :: Array (Int /\ Migration r e)
    for_ pending \(version /\ run) ->
      when (not (Set.member version appliedSet)) do
        run client
        _ <- execParamsUsing sym
          "insert into __rio_migrations (version) values ($1)"
          version
          client
        pure unit
