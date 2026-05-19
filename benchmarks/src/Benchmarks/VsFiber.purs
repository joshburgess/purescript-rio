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
import RIO.Fiber.Core (fork, forkInline, join, parTraverse) as F

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

  liftEffect do
    log ""
    log "Reading the table: compare each rio-fiber row to the matching"
    log "RIO and Aff rows above. The runFiberAff bridge adds a fixed"
    log "per-iteration cost (one makeAff round-trip) that is paid by"
    log "every rio-fiber row here. The forkInline row isolates the"
    log "savings from skipping the per-fork microtask hop for sync-"
    log "bodied children."
    log ""
