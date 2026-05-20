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
import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Concurrency (awaitAll, fork, forkAll, forkAllUntracked, joinAll, parTraverse)

-- | Workload 1: a chain of `pure (acc + 1)` binds in RIO.Aff.
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

-- | Workload 5 helper: stack N `map (_ + 1)` calls on top of a single
-- | `pure`. Mirrors the rio-fiber `mapChain` so the per-layer Functor
-- | overhead is directly comparable across RIO, Aff, and rio-fiber.
rioMapChain :: Int -> RIO () () Int
rioMapChain n = go n (pure 0)
  where
  go :: Int -> RIO () () Int -> RIO () () Int
  go 0 acc = acc
  go k acc = go (k - 1) (map (_ + 1) acc)

affMapChain :: Int -> Aff Int
affMapChain n = go n (pure 0)
  where
  go :: Int -> Aff Int -> Aff Int
  go 0 acc = acc
  go k acc = go (k - 1) (map (_ + 1) acc)

-- | Workload 5 helper: stack N `<*>` applications of `pure (_ + 1)`
-- | on top of a single `pure`. Mirrors the rio-fiber `applyChain`.
rioApplyChain :: Int -> RIO () () Int
rioApplyChain n = go n (pure 0)
  where
  go :: Int -> RIO () () Int -> RIO () () Int
  go 0 acc = acc
  go k acc = go (k - 1) (pure (_ + 1) <*> acc)

affApplyChain :: Int -> Aff Int
affApplyChain n = go n (pure 0)
  where
  go :: Int -> Aff Int -> Aff Int
  go 0 acc = acc
  go k acc = go (k - 1) (pure (_ + 1) <*> acc)

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

  -- Workload 5: pure map / apply chains. Mirrors the rio-fiber rows in
  -- VsFiber so the per-layer Functor / Apply overhead is comparable
  -- across RIO (Aff-backed), raw Aff, and rio-fiber. RIO's default
  -- Functor / Apply are derived from Bind, so each layer pays a BIND +
  -- PURE allocation; rio-fiber's dedicated OP_MAP / OP_APPLY collapse
  -- that into a single op + K frame.
  benchAffWith sampleCount
    "RIO map chain (1000 maps over pure)"
    (void (runRIO' (rioMapChain 1000)))
  benchAffWith sampleCount
    "Aff map chain (1000 maps over pure)"
    (void (affMapChain 1000))
  benchAffWith sampleCount
    "RIO apply chain (1000 applies over pure)"
    (void (runRIO' (rioApplyChain 1000)))
  benchAffWith sampleCount
    "Aff apply chain (1000 applies over pure)"
    (void (affApplyChain 1000))

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
