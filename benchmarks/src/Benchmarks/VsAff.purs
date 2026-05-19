-- | Head-to-head RIO vs raw `Aff` micro-benchmarks.
-- |
-- | The Main suite times the production RIO surface against a few
-- | reference baselines. This module pins the comparison: for each
-- | workload it runs the same shape twice, once on `RIO` and once on
-- | raw `Aff`, so the per-effect overhead is directly visible in the
-- | output.
-- |
-- | Three workloads:
-- |
-- |   1. Tight bind loop (10 000 iterations). Stresses monadic bind.
-- |   2. Parallel mapM over a 32-element array of pure work. Stresses
-- |      the parallel applicative machinery.
-- |   3. Fan-out / fan-in: fork 16 children, await every one, collect
-- |      the results. Stresses fork + join.
-- |
-- | Numbers are wall-clock per iteration via `process.hrtime()`. The
-- | RIO and Aff figures are printed back-to-back so a reader can
-- | eyeball the ratio without arithmetic.
module Benchmarks.VsAff
  ( runVsAff
  ) where

import Prelude

import Benchmarks.Harness (benchAffWith)
import Data.Array (range) as Array
import Data.Traversable (traverse)
import Effect.Aff (Aff, joinFiber, forkAff, parallel, sequential)
import Effect.Class (liftEffect)
import Effect.Console (log)
import RIO.Core (RIO, runRIO')
import RIO.Concurrency (awaitAll, fork, forkAll, forkAllUntracked, joinAll, parTraverse)

-- | Workload 1: a chain of `pure (acc + 1)` binds in RIO.
rioBindChain :: Int -> RIO () () Int
rioBindChain n = go 0 n
  where
  go :: Int -> Int -> RIO () () Int
  go acc 0 = pure acc
  go acc k = do
    x <- pure (acc + 1)
    go x (k - 1)

-- | Same chain, written directly in `Aff`.
affBindChain :: Int -> Aff Int
affBindChain n = go 0 n
  where
  go :: Int -> Int -> Aff Int
  go acc 0 = pure acc
  go acc k = do
    x <- pure (acc + 1)
    go x (k - 1)

-- | Workload 3 body: a single forked child returns `n + 1`.
rioChild :: Int -> RIO () () Int
rioChild n = pure (n + 1)

affChild :: Int -> Aff Int
affChild n = pure (n + 1)

-- | Run every head-to-head pair. Called from `Benchmarks.Main`.
runVsAff :: Aff Unit
runVsAff = do
  liftEffect do
    log ""
    log "================================================================"
    log "  RIO vs raw Aff (head-to-head)"
    log "================================================================"
    log ""

  let
    bindIters = 10000
    parArr = Array.range 1 32
    fanCount = 16
    fanArr = Array.range 1 fanCount
    sampleCount = 200

  -- Workload 1: tight bind loop.
  benchAffWith sampleCount
    "RIO bind chain (10000 binds)"
    (void (runRIO' (rioBindChain bindIters)))
  benchAffWith sampleCount
    "Aff bind chain (10000 binds)"
    (void (affBindChain bindIters))

  -- Workload 2: parallel mapM over a 32-element array.
  benchAffWith sampleCount
    "RIO parTraverse (32 elements, pure work)"
    ( void
        ( runRIO'
            (parTraverse (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )
  benchAffWith sampleCount
    "Aff parTraverse (32 elements, pure work)"
    (void (sequential (traverse (\n -> parallel (pure (n + 1) :: Aff Int)) parArr)))

  -- Workload 3: fan-out 16 children, await every one.
  benchAffWith sampleCount
    "RIO fan-out/fan-in (fork x16 + awaitAll)"
    ( void
        ( runRIO' do
            fibs <- traverse (\n -> fork (rioChild n)) fanArr
            awaitAll fibs
        )
    )
  benchAffWith sampleCount
    "RIO fan-out/fan-in (forkAll x16 + awaitAll)"
    ( void
        ( runRIO' do
            fibs <- forkAll (map rioChild fanArr)
            awaitAll fibs
        )
    )
  benchAffWith sampleCount
    "RIO fan-out/fan-in (forkAll x16 + joinAll)"
    ( void
        ( runRIO' do
            fibs <- forkAll (map rioChild fanArr)
            joinAll fibs
        )
    )
  benchAffWith sampleCount
    "RIO fan-out/fan-in (forkAllUntracked x16 + joinAll)"
    ( void
        ( runRIO' do
            fibs <- forkAllUntracked (map rioChild fanArr)
            joinAll fibs
        )
    )
  benchAffWith sampleCount
    "Aff fan-out/fan-in (forkAff x16 + joinFiber)"
    ( void do
        fibs <- traverse (\n -> forkAff (affChild n)) fanArr
        traverse joinFiber fibs
    )

  -- Workload 4: sequential traverse over a 32-element array with pure
  -- work. Mirrors the RIO and rio-fiber sequential-traverse rows so the
  -- per-effect overhead of traverse-driven bind chains is comparable.
  benchAffWith sampleCount
    "RIO sequential traverse (32 elements, pure work)"
    ( void
        ( runRIO'
            (traverse (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )
  benchAffWith sampleCount
    "Aff sequential traverse (32 elements, pure work)"
    (void (traverse (\n -> pure (n + 1) :: Aff Int) parArr))

  liftEffect do
    log ""
    log "Reading the table: the row below each RIO entry is the same"
    log "shape written directly in Aff. The ratio is the per-effect"
    log "overhead RIO adds for that workload."
    log ""
