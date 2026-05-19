-- | Head-to-head rio-fiber vs Aff-backed RIO and raw Aff.
-- |
-- | Mirrors `Benchmarks.VsAff`'s three workloads against the
-- | fiber-backed `RIO.Fiber` runtime, so the per-iteration overhead of
-- | the custom fiber scheduler is directly comparable to the
-- | `Aff`-backed `RIO`. Numbers are wall-clock per iteration sampled
-- | with `process.hrtime()`.
-- |
-- | The fiber runner is callback-shaped, so each call is bridged into
-- | `Aff` via `runFiberAff` (the same pattern the test helpers use).
-- | The bridge itself adds a fixed per-run cost; reading the numbers,
-- | subtract that from both rio-fiber rows.
module Benchmarks.VsFiber
  ( runVsFiber
  ) where

import Prelude

import Benchmarks.Harness (benchAffWith)
import Data.Array (range) as Array
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import RIO.Fiber.Aff (runAff) as F
import RIO.Fiber.Core (Outcome, RIO)
import RIO.Fiber.Core (forEach, fork, forkAll, forkAllInline, forkInline, join, joinAll, parTraverse) as F

runFiberAff :: forall e a. RIO () e a -> Aff (Outcome e a)
runFiberAff rio = F.runAff rio {}

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

-- | Workload 7 helper: stack N `map (_ + 1)` calls on top of a single
-- | `pure`. Each layer goes through the Functor instance, which is
-- | backed by `opMap`.
mapChain :: Int -> RIO () () Int
mapChain n = go n (pure 0)
  where
  go :: Int -> RIO () () Int -> RIO () () Int
  go 0 acc = acc
  go k acc = go (k - 1) (map (_ + 1) acc)

-- | Workload 7 helper: stack N `<*>` applications of `pure (_ + 1)` on
-- | top of a single `pure`. Each layer goes through the Apply
-- | instance, which is backed by `opApply`.
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
    log "  rio-fiber vs Aff-backed RIO (head-to-head)"
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
    "rio-fiber bind chain (10000 binds)"
    (void (runFiberAff (fiberBindChain bindIters)))

  -- Workload 2: parallel mapM over a 32-element array.
  benchAffWith sampleCount
    "rio-fiber parTraverse (32 elements, pure work)"
    ( void
        ( runFiberAff
            (F.parTraverse (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )

  -- Workload 3: fan-out 16 children, join every one.
  benchAffWith sampleCount
    "rio-fiber fan-out/fan-in (fork x16 + join each)"
    ( void
        ( runFiberAff do
            fibs <- traverse (\n -> F.fork (fiberChild n)) fanArr
            traverse F.join fibs
        )
    )

  -- Workload 4: identical fan-out via forkInline. Each child is
  -- sync-bodied (`pure (n + 1)`), so forkInline drives the child to
  -- completion before the parent's next op and the subsequent `join`
  -- resolves without going through queueMicrotask. The expected
  -- delta versus workload 3 isolates the per-fork microtask hop.
  benchAffWith sampleCount
    "rio-fiber fan-out/fan-in (forkInline x16 + join each)"
    ( void
        ( runFiberAff do
            fibs <- traverse (\n -> F.forkInline (fiberChild n)) fanArr
            traverse F.join fibs
        )
    )

  -- Workload 5: identical fan-out via the specialized forkAll / joinAll
  -- ops. These walk the array in a single JS-side loop, so they should
  -- beat the traverse-built fork/join chains by skipping the per-element
  -- BIND nodes that `traverseArrayImpl` builds.
  benchAffWith sampleCount
    "rio-fiber fan-out/fan-in (forkAll x16 + joinAll)"
    ( void
        ( runFiberAff do
            fibs <- F.forkAll (map fiberChild fanArr)
            F.joinAll fibs
        )
    )

  -- Workload 5b: forkAllInline, the inline variant of forkAll. For
  -- sync-bodied children the per-child microtask hop disappears, so
  -- this row should beat workload 5 by roughly the same factor that
  -- forkInline beats fork on workload 4.
  benchAffWith sampleCount
    "rio-fiber fan-out/fan-in (forkAllInline x16 + joinAll)"
    ( void
        ( runFiberAff do
            fibs <- F.forkAllInline (map fiberChild fanArr)
            F.joinAll fibs
        )
    )

  -- Workload 6: sequential traverse over a 32-element array. Compares
  -- `traverse` (Prelude / Data.Traversable) against the specialized
  -- `forEach` op: traverse builds the same ~2N-node bind chain that
  -- forkAll/joinAll were designed to replace; forEach replaces it with
  -- a single K_FOR_EACH frame that the interpreter advances in place.
  benchAffWith sampleCount
    "rio-fiber sequential traverse (32 elements, pure work)"
    ( void
        ( runFiberAff
            (traverse (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )
  benchAffWith sampleCount
    "rio-fiber sequential forEach (32 elements, pure work)"
    ( void
        ( runFiberAff
            (F.forEach (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )

  -- Workload 7: pure map / apply through the dedicated OP_MAP and
  -- OP_APPLY ops. The default Functor / Apply derived from Bind would
  -- emit a BIND + PURE + closure per node; OP_MAP / OP_APPLY collapse
  -- that into a single op + K frame. Both rows here exercise tight
  -- map / apply chains so the saved allocations show up.
  benchAffWith sampleCount
    "rio-fiber map chain (1000 maps over pure)"
    ( void
        ( runFiberAff
            (mapChain 1000 :: RIO () () Int)
        )
    )
  benchAffWith sampleCount
    "rio-fiber apply chain (1000 applies over pure)"
    ( void
        ( runFiberAff
            (applyChain 1000 :: RIO () () Int)
        )
    )

  liftEffect do
    log ""
    log "Reading the table: compare each rio-fiber row to the matching"
    log "RIO and Aff rows above. The runFiberAff bridge adds a fixed"
    log "per-iteration cost (one makeAff round-trip) that is paid by"
    log "every rio-fiber row here. The forkInline row isolates the"
    log "savings from skipping the per-fork microtask hop for sync-"
    log "bodied children. The forkAll / joinAll row isolates the savings"
    log "from skipping the per-element bind chain that traverse builds."
    log "The forEach row vs sequential-traverse isolates the same gain"
    log "for the non-forking traverse case."
    log ""
