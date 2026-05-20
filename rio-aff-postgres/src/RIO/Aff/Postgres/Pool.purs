-- | Pool inspection and warmup helpers.
-- |
-- | `poolStats` reads the three counters the pool exposes
-- | (`total`, `idle`, `waiting`). The pool mutates these as
-- | clients are checked out / returned, so a `poolStats` call
-- | inside a hot path is a synchronous read of the pool's
-- | bookkeeping, not a round-trip to Postgres. Useful for
-- | `/healthz` endpoints and for instrumenting how close the pool
-- | is to saturation.
-- |
-- | `warmup` pre-connects up to `n` clients by acquiring them in
-- | sequence and returning them to the pool together. After
-- | `warmup` returns, the pool has at least `n` idle clients
-- | available (capped by the pool's configured `max`). This is
-- | optional but smooths the latency profile of the first few
-- | requests after process start, where every fresh connection
-- | pays the TCP + TLS + authentication round-trip.
module RIO.Aff.Postgres.Pool
  ( PoolStats
  , poolStats
  , warmup
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect.Class (liftEffect)
import Effect.Postgres.Pool
  ( clientCount
  , clientIdleCount
  , clientWaitingCount
  ) as PG.Pool
import Prim.Row (class Cons) as Row
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO)
import RIO.Aff.Env (ask)
import RIO.Aff.Postgres (PgError, Postgres(..), withClient)

-- | A snapshot of the pool's internal counters.
-- |
-- |   * `total` is every client the pool currently owns, idle or
-- |     checked out.
-- |   * `idle` is the subset of `total` that's parked in the pool
-- |     ready to hand out.
-- |   * `waiting` is the number of pending `withClient` calls
-- |     parked because every client is currently checked out.
type PoolStats =
  { total :: Int
  , idle :: Int
  , waiting :: Int
  }

-- | Read the current pool counters. Synchronous; never round-trips
-- | to Postgres.
poolStats :: forall r e. RIO (postgres :: Postgres | r) e PoolStats
poolStats = do
  Postgres { pool } <- ask (Proxy :: Proxy "postgres")
  liftEffect do
    let
      total = PG.Pool.clientCount pool
      idle = PG.Pool.clientIdleCount pool
      waiting = PG.Pool.clientWaitingCount pool
    pure { total, idle, waiting }

-- | Pre-connect up to `n` clients and return them to the pool. On
-- | return the pool has at least `min n max` idle clients (where
-- | `max` is the pool's configured cap). Non-positive `n` is a
-- | no-op.
-- |
-- | If `n` exceeds the pool's `max`, acquisition past the cap
-- | blocks waiting for a client that no other call is holding, so
-- | passing `n > max` will hang. Callers should pass the pool's
-- | configured `max` (or smaller).
warmup
  :: forall sym r e e'
   . IsSymbol sym
  => Row.Cons sym PgError e' e
  => Proxy sym
  -> Int
  -> RIO (postgres :: Postgres | r) e Unit
warmup sym n
  | n <= 0 = pure unit
  | otherwise = withClient sym \_ -> warmup sym (n - 1)
