-- | Head-to-head rio-fiber vs Aff-backed RIO and raw Aff.
-- |
-- | Mirrors `Benchmarks.VsAff`'s workloads against the fiber-backed
-- | `RIO.Aff.Fiber` runtime, so the per-iteration overhead of the custom
-- | fiber scheduler is directly comparable to the `Aff`-backed `RIO`.
-- | Numbers are wall-clock per iteration sampled with
-- | `process.hrtime()`.
-- |
-- | rio-fiber rows use the native synchronous runner (`runRIO'`),
-- | which returns `Effect a` directly. There is no Aff bridge in the
-- | measurement path: every iteration starts, runs, and completes
-- | inside one synchronous `runRIO'` call, so the numbers reflect the
-- | fiber runtime itself rather than fiber-plus-makeAff. The Aff-
-- | shaped `runAff` in `RIO.Fiber.Aff` is only a convenience for
-- | callers who already live in `Aff`; it is not on the hot path.
module Benchmarks.VsFiber
  ( runVsFiber
  ) where

import Prelude

import Benchmarks.Harness (benchSyncWith)
import Data.Array (range) as Array
import Data.Traversable (traverse)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import RIO.Fiber.Core (RIO, runRIO')
import RIO.Fiber.Core (forEach, fork, forkAll, forkAllInline, forkInline, join, joinAll, parTraverse) as F

affMapChain :: Int -> Aff Int
affMapChain n = go n (pure 0)
  where
  go :: Int -> Aff Int -> Aff Int
  go 0 acc = acc
  go k acc = go (k - 1) (map (_ + 1) acc)

affApplyChain :: Int -> Aff Int
affApplyChain n = go n (pure 0)
  where
  go :: Int -> Aff Int -> Aff Int
  go 0 acc = acc
  go k acc = go (k - 1) (pure (_ + 1) <*> acc)

fiberBindChain :: Int -> RIO () () Int
fiberBindChain n = go 0 n
  where
  go :: Int -> Int -> RIO () () Int
  go acc 0 = pure acc
  go acc k = do
    x <- pure (acc + 1)
    go x (k - 1)

fiberChild :: Int -> RIO () () Int
fiberChild n = pure (n + 1)

-- | Stack N `map (_ + 1)` calls on top of a single `pure`. Each layer
-- | goes through the Functor instance, which is backed by `opMap`.
mapChain :: Int -> RIO () () Int
mapChain n = go n (pure 0)
  where
  go :: Int -> RIO () () Int -> RIO () () Int
  go 0 acc = acc
  go k acc = go (k - 1) (map (_ + 1) acc)

-- | Stack N `<*>` applications of `pure (_ + 1)` on top of a single
-- | `pure`. Each layer goes through the Apply instance, which is
-- | backed by `opApply`.
applyChain :: Int -> RIO () () Int
applyChain n = go n (pure 0)
  where
  go :: Int -> RIO () () Int -> RIO () () Int
  go 0 acc = acc
  go k acc = go (k - 1) (pure (_ + 1) <*> acc)

runVsFiber :: Aff Unit
runVsFiber = do
  liftEffect do
    log ""
    log "================================================================"
    log "  rio-fiber vs Aff-backed RIO (head-to-head, native runner)"
    log "================================================================"
    log ""

  let
    bindIters = 10000
    parArr = Array.range 1 32
    fanCount = 16
    fanArr = Array.range 1 fanCount
    sampleCount = 200

  -- Workload 1: tight bind loop.
  benchSyncWith sampleCount
    "rio-fiber bind chain (10000 binds)"
    (void (runRIO' (fiberBindChain bindIters)))

  -- Workload 2: parallel mapM over a 32-element array.
  benchSyncWith sampleCount
    "rio-fiber parTraverse (32 elements, pure work)"
    ( void
        ( runRIO'
            (F.parTraverse (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )

  -- Workload 3: fan-out 16 children, join every one.
  benchSyncWith sampleCount
    "rio-fiber fan-out/fan-in (fork x16 + join each)"
    ( void
        ( runRIO' do
            fibs <- traverse (\n -> F.fork (fiberChild n)) fanArr
            traverse F.join fibs
        )
    )

  -- Workload 4: identical fan-out via forkInline. Each child is
  -- sync-bodied (`pure (n + 1)`), so forkInline drives the child to
  -- completion before the parent's next op and the subsequent `join`
  -- resolves without going through queueMicrotask. The expected
  -- delta versus workload 3 isolates the per-fork microtask hop.
  benchSyncWith sampleCount
    "rio-fiber fan-out/fan-in (forkInline x16 + join each)"
    ( void
        ( runRIO' do
            fibs <- traverse (\n -> F.forkInline (fiberChild n)) fanArr
            traverse F.join fibs
        )
    )

  -- Workload 5: identical fan-out via the specialized forkAll / joinAll
  -- ops. These walk the array in a single JS-side loop, so they should
  -- beat the traverse-built fork/join chains by skipping the per-element
  -- BIND nodes that `traverseArrayImpl` builds.
  benchSyncWith sampleCount
    "rio-fiber fan-out/fan-in (forkAll x16 + joinAll)"
    ( void
        ( runRIO' do
            fibs <- F.forkAll (map fiberChild fanArr)
            F.joinAll fibs
        )
    )

  -- Workload 5b: forkAllInline, the inline variant of forkAll. For
  -- sync-bodied children the per-child microtask hop disappears, so
  -- this row should beat workload 5 by roughly the same factor that
  -- forkInline beats fork on workload 4.
  benchSyncWith sampleCount
    "rio-fiber fan-out/fan-in (forkAllInline x16 + joinAll)"
    ( void
        ( runRIO' do
            fibs <- F.forkAllInline (map fiberChild fanArr)
            F.joinAll fibs
        )
    )

  -- Workload 6: sequential traverse over a 32-element array. Compares
  -- `traverse` (Prelude / Data.Traversable) against the specialized
  -- `forEach` op: traverse builds the same ~2N-node bind chain that
  -- forkAll/joinAll were designed to replace; forEach replaces it with
  -- a single K_FOR_EACH frame that the interpreter advances in place.
  benchSyncWith sampleCount
    "rio-fiber sequential traverse (32 elements, pure work)"
    ( void
        ( runRIO'
            (traverse (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )
  benchSyncWith sampleCount
    "rio-fiber sequential forEach (32 elements, pure work)"
    ( void
        ( runRIO'
            (F.forEach (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )

  -- Workload 7: pure map / apply through the dedicated OP_MAP and
  -- OP_APPLY ops. The default Functor / Apply derived from Bind would
  -- emit a BIND + PURE + closure per node; OP_MAP / OP_APPLY collapse
  -- that into a single op + K frame, and the smart constructors fuse
  -- adjacent maps at build time. Both rows here exercise tight
  -- map / apply chains so the saved allocations show up.
  benchSyncWith sampleCount
    "rio-fiber map chain (1000 maps over pure)"
    ( void
        ( runRIO'
            (mapChain 1000 :: RIO () () Int)
        )
    )
  -- Apples-to-apples Aff row: launchAff_ spins up a fresh Aff
  -- interpreter per iteration, mirroring runRIO''s per-iter fiber
  -- start/finish. (The Aff map-chain row in `Benchmarks.VsAff` runs
  -- inside an outer Aff loop, which collapses to a single bind into
  -- the parent continuation per iter and does not measure the
  -- interpreter-start cost.)
  benchSyncWith sampleCount
    "Aff map chain via launchAff_ (1000 maps over pure)"
    (launchAff_ (void (affMapChain 1000)))
  benchSyncWith sampleCount
    "rio-fiber apply chain (1000 applies over pure)"
    ( void
        ( runRIO'
            (applyChain 1000 :: RIO () () Int)
        )
    )
  benchSyncWith sampleCount
    "Aff apply chain via launchAff_ (1000 applies over pure)"
    (launchAff_ (void (affApplyChain 1000)))

  liftEffect do
    log ""
    log "Reading the table: compare each rio-fiber row to the matching"
    log "RIO and Aff rows above. The rio-fiber rows here use the native"
    log "runRIO' runner (Effect a), with zero Aff bridging on the hot"
    log "path. The forkInline row isolates the savings from skipping"
    log "the per-fork microtask hop for sync-bodied children. The"
    log "forkAll / joinAll row isolates the savings from skipping the"
    log "per-element bind chain that traverse builds. The forEach row"
    log "vs sequential-traverse isolates the same gain for the non-"
    log "forking traverse case. The two `launchAff_` Aff rows pair up"
    log "with the rio-fiber map / apply rows on the same harness, so"
    log "their ratio reflects the actual per-call interpreter cost."
    log ""
