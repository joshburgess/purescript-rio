-- | App-facing bindings for `purescript-postgresql` (the
-- | `cakekindel/purescript-postgresql` driver, which wraps
-- | `node-postgres` / `pg`) expressed as `RIO` combinators.
-- |
-- | The service is a single token, `Postgres`, held in the
-- | environment row under the conventional label `postgres`. It
-- | wraps a connection pool. Smart constructors below acquire a
-- | client from the pool for the duration of a query and return
-- | it to the pool on exit (success, typed failure, defect, or
-- | external kill).
-- |
-- | Errors from the underlying driver are surfaced as a single
-- | newtype, `PgError`, on a caller-chosen typed-failure tag. The
-- | caller passes the tag as a `Proxy` so this module never
-- | commits to a particular row layout; downstream wrappers (the
-- | application's own `Postgres` module) typically pin the tag
-- | once and re-export combinators without it.
-- |
-- | Layer wiring (acquire the pool, release on scope exit) lives
-- | in `RIO.Aff.Postgres.Layer`.
module RIO.Aff.Postgres
  ( Postgres(..)
  , PgError(..)
  , pgErrorMessage
  , withClient
  , withClientUsing
  , query
  , queryUsing
  , queryParams
  , queryParamsUsing
  , queryPrepared
  , queryPreparedUsing
  , exec
  , execUsing
  , execParams
  , execParamsUsing
  , execPrepared
  , execPreparedUsing
  , withTransaction
  , module Reexports
  ) where

import Prelude

import Control.Monad.Except.Trans (runExceptT)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy(..))

import Data.Maybe (Maybe(..))
import Data.Postgres.Query (class AsQuery, class AsQueryParams, Query(..), asQueryParams) as PG
import Data.Postgres.Query (class AsQuery, class AsQueryParams) as Reexports
import Data.Postgres.Result (class FromRows) as Reexports
import Data.Postgres.Result (class FromRows) as PG
import Effect.Aff.Postgres.Client (Client) as Reexports
import Effect.Aff.Postgres.Client (Client) as PG
import Effect.Aff.Postgres.Client (query, exec) as PG.Client
import Effect.Aff.Postgres.Pool (Pool, connect) as PG.Pool
import Effect.Postgres.Error (Error) as Reexports
import Effect.Postgres.Error (Error) as PG
import Effect.Postgres.Error.Common (toString) as PG.Err
import Effect.Postgres.Error.Except (Except) as PG
import Effect.Postgres.Pool (release) as PG.PoolE

import RIO.Aff.Env (ask)
import RIO.Aff.Error (catchAll, rethrow)
import RIO.Aff.Internal (RIO, mkRIO, rioFail)
import RIO.Aff.Resource (acquireRelease)

-- | The Postgres service token. Holds a connection pool. Place it
-- | in the environment under the label `postgres`; smart
-- | constructors below `ask` for it implicitly.
-- |
-- | Build one with `RIO.Aff.Postgres.Layer.postgresLayer`.
newtype Postgres = Postgres { pool :: PG.Pool.Pool }

-- | An error raised by the underlying `node-postgres` driver. The
-- | upstream library surfaces a non-empty array because a single
-- | call can produce multiple failures (e.g. a serialization
-- | failure followed by a connection drop); we preserve that
-- | shape verbatim so callers that want to introspect can.
-- |
-- | Use `pgErrorMessage` to render this for logging.
newtype PgError = PgError (NonEmptyArray PG.Error)

-- | Render a `PgError` as a (multi-line) human-readable string,
-- | preserving the upstream library's formatting.
pgErrorMessage :: PgError -> String
pgErrorMessage (PgError es) = show (map PG.Err.toString es)

-- | Lift an `Except Aff a` (= `ExceptT (NonEmptyArray Error) Aff a`)
-- | into RIO, mapping any driver failure to a caller-chosen typed
-- | tag carrying `PgError`.
fromExcept
  :: forall sym r e e' a
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> PG.Except Aff a
  -> RIO r e a
fromExcept sym action = mkRIO \_ -> do
  res <- runExceptT action
  case res of
    Right a -> pure a
    Left es -> rioFail (Variant.inj sym (PgError es) :: Variant e)

-- | Acquire a client from the pool, run `use`, and return the
-- | client to the pool on exit (success, typed failure, defect,
-- | or external kill). The release path swallows any
-- | `Disconnecting` error the driver may produce while returning
-- | the client; cleanup failures are not surfaced as typed
-- | failures.
-- |
-- | The error row must mention the caller's `PgError` tag because
-- | connection acquisition itself can fail.
withClient
  :: forall sym r e e' a
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> (PG.Client -> RIO (postgres :: Postgres | r) e a)
  -> RIO (postgres :: Postgres | r) e a
withClient sym use = do
  Postgres { pool } <- ask (Proxy :: Proxy "postgres")
  withClientUsing sym pool use

-- | Variant of `withClient` for the rare case where the caller
-- | already holds a `Pool` (e.g. test harnesses that build the
-- | pool inline). Production code should prefer `withClient`,
-- | which threads the pool through the environment row.
withClientUsing
  :: forall sym r e e' a
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> PG.Pool.Pool
  -> (PG.Client -> RIO r e a)
  -> RIO r e a
withClientUsing sym pool use =
  acquireRelease
    (fromExcept sym (PG.Pool.connect pool))
    ( \client -> mkRIO \_ -> do
        _ <- liftEffect (runExceptT (PG.PoolE.release pool client))
        pure unit
    )
    use

-- | Acquire a client, run a query that returns rows, release the
-- | client. The result type is whatever `FromRows` instance the
-- | call site picks: typically `Array (Int /\ String /\ ...)` for
-- | tuple decoders, or any user-defined `FromRows` instance.
query
  :: forall sym q out r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQuery q
  => PG.FromRows out
  => Proxy sym
  -> q
  -> RIO (postgres :: Postgres | r) e out
query sym q = withClient sym (queryUsing sym q)

-- | Variant of `query` that runs on a supplied client. Use inside
-- | `withClient` / `withTransaction` when you want to thread the
-- | same client through several operations.
queryUsing
  :: forall sym q out r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQuery q
  => PG.FromRows out
  => Proxy sym
  -> q
  -> PG.Client
  -> RIO r e out
queryUsing sym q client = fromExcept sym (PG.Client.query q client)

-- | Parameterized `query`. The SQL text uses `$1`, `$2`, ... and
-- | `ps` is the tuple of values bound to those placeholders. A
-- | single scalar binds `$1`; nested tuples (e.g. `1 /\ "foo"`)
-- | bind in left-to-right order. The driver does the escaping, so
-- | this is the safe way to pass user-controlled input into a
-- | query.
-- |
-- | ```purescript
-- | rows <- queryParams dbTag
-- |   "select id, label from items where owner = $1 and id > $2"
-- |   ("alice" /\ (100 :: Int))
-- | ```
queryParams
  :: forall sym ps out r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQueryParams ps
  => PG.FromRows out
  => Proxy sym
  -> String
  -> ps
  -> RIO (postgres :: Postgres | r) e out
queryParams sym text ps = query sym (text /\ ps)

-- | Variant of `queryParams` that runs on a supplied client. Use
-- | inside `withClient` / `withTransaction` to thread the same
-- | client through several parameterized operations.
queryParamsUsing
  :: forall sym ps out r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQueryParams ps
  => PG.FromRows out
  => Proxy sym
  -> String
  -> ps
  -> PG.Client
  -> RIO r e out
queryParamsUsing sym text ps client = queryUsing sym (text /\ ps) client

-- | Acquire a client, execute a statement (`INSERT`, `UPDATE`, ...),
-- | release the client. Returns the number of rows the statement
-- | affected, as reported by the driver.
exec
  :: forall sym q r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQuery q
  => Proxy sym
  -> q
  -> RIO (postgres :: Postgres | r) e Int
exec sym q = withClient sym (execUsing sym q)

-- | Variant of `exec` that runs on a supplied client. Use inside
-- | `withClient` / `withTransaction` to chain statements on the
-- | same connection.
execUsing
  :: forall sym q r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQuery q
  => Proxy sym
  -> q
  -> PG.Client
  -> RIO r e Int
execUsing sym q client = fromExcept sym (PG.Client.exec q client)

-- | Parameterized `exec`. The SQL text uses `$1`, `$2`, ... and
-- | `ps` is the tuple of values bound to those placeholders.
-- |
-- | ```purescript
-- | inserted <- execParams dbTag
-- |   "insert into items (id, label) values ($1, $2)"
-- |   (7 /\ "seven")
-- | ```
execParams
  :: forall sym ps r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQueryParams ps
  => Proxy sym
  -> String
  -> ps
  -> RIO (postgres :: Postgres | r) e Int
execParams sym text ps = exec sym (text /\ ps)

-- | Variant of `execParams` that runs on a supplied client. Use
-- | inside `withClient` / `withTransaction` to chain parameterized
-- | statements on the same connection.
execParamsUsing
  :: forall sym ps r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQueryParams ps
  => Proxy sym
  -> String
  -> ps
  -> PG.Client
  -> RIO r e Int
execParamsUsing sym text ps client = execUsing sym (text /\ ps) client

-- | Internal: a parameterized query carrying a prepared-statement
-- | name. node-postgres caches the parsed plan under `name` on the
-- | connection that first sees the query; subsequent calls with the
-- | same `name` on the same connection reuse the plan. The
-- | constructor is hidden; users go through `queryPrepared` /
-- | `execPrepared` and friends.
data PreparedQuery ps = PreparedQuery String String ps

instance asQueryPrepared :: PG.AsQueryParams ps => PG.AsQuery (PreparedQuery ps) where
  asQuery (PreparedQuery name text ps) = do
    values <- PG.asQueryParams ps
    pure (PG.Query { text, values, name: Just name })

-- | Run a prepared parameterized query. `name` identifies the
-- | cached plan on the underlying connection; `text` is the SQL
-- | with `$1`, `$2`, ... placeholders; `ps` is the tuple of bound
-- | values.
-- |
-- | The plan is cached per-connection: when this borrows a client
-- | from the pool, the cache hits whenever the pool hands back the
-- | same connection. For a guaranteed cache hit across calls, run
-- | several `queryPreparedUsing` calls inside a single
-- | `withClient` / `withTransaction` block on the same `Client`.
-- |
-- | ```purescript
-- | rows <- queryPrepared dbTag "find_item"
-- |   "select id, label from items where owner = $1"
-- |   "alice"
-- | ```
queryPrepared
  :: forall sym ps out r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQueryParams ps
  => PG.FromRows out
  => Proxy sym
  -> String
  -> String
  -> ps
  -> RIO (postgres :: Postgres | r) e out
queryPrepared sym name text ps =
  query sym (PreparedQuery name text ps)

-- | Variant of `queryPrepared` that runs on a supplied client. Use
-- | inside `withClient` / `withTransaction` to guarantee the cached
-- | plan is reused across calls.
queryPreparedUsing
  :: forall sym ps out r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQueryParams ps
  => PG.FromRows out
  => Proxy sym
  -> String
  -> String
  -> ps
  -> PG.Client
  -> RIO r e out
queryPreparedUsing sym name text ps client =
  queryUsing sym (PreparedQuery name text ps) client

-- | Run a prepared parameterized statement (`INSERT`, `UPDATE`, ...).
-- | See `queryPrepared` for the cache-hit story.
execPrepared
  :: forall sym ps r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQueryParams ps
  => Proxy sym
  -> String
  -> String
  -> ps
  -> RIO (postgres :: Postgres | r) e Int
execPrepared sym name text ps =
  exec sym (PreparedQuery name text ps)

-- | Variant of `execPrepared` that runs on a supplied client.
execPreparedUsing
  :: forall sym ps r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => PG.AsQueryParams ps
  => Proxy sym
  -> String
  -> String
  -> ps
  -> PG.Client
  -> RIO r e Int
execPreparedUsing sym name text ps client =
  execUsing sym (PreparedQuery name text ps) client

-- | Acquire a client, issue `BEGIN`, run `use`, then `COMMIT`. If
-- | `use` raises a typed failure, the transaction is rolled back
-- | and the failure is re-raised on the same tag. Defects (Aff
-- | exceptions) skip the rollback path; if you need rollback-on-
-- | defect, sandbox the inner action.
-- |
-- | The client is released to the pool on every exit path
-- | (success, typed failure, defect, or external kill), as with
-- | `withClient`.
withTransaction
  :: forall sym r e e' a
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> (PG.Client -> RIO (postgres :: Postgres | r) e a)
  -> RIO (postgres :: Postgres | r) e a
withTransaction sym use = withClient sym \client -> do
  _ <- execUsing sym ("begin" :: String) client
  catchAll
    ( \v -> do
        _ <- execUsing sym ("rollback" :: String) client
        rethrow v
    )
    ( do
        a <- use client
        _ <- execUsing sym ("commit" :: String) client
        pure a
    )
