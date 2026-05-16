-- | Phase 8.4 benchmark suite.
-- |
-- | Four scenarios, run end-to-end against the production RIO surface:
-- |
-- |   1. Monadic bind in a tight loop: a chain of `pure (n + 1)` binds.
-- |      Measures the per-`bind` overhead of `RIO` over the underlying
-- |      `Aff` chain.
-- |
-- |   2. Service lookup overhead: a loop that asks for a service and
-- |      reads one field per iteration. Measures `ask` + `Record.get`
-- |      vs. a raw `Aff` baseline.
-- |
-- |   3. Sequential vs parallel traversal: a 32-element array of
-- |      `pure`-only RIO actions traversed both ways. With purely
-- |      synchronous work `parTraverse` should be similar to `traverse`
-- |      or slightly slower (parallel bookkeeping cost without any
-- |      latency to overlap).
-- |
-- |   4. `Variant` failure construction: a loop that constructs a typed
-- |      failure and immediately catches it. Measures the round-trip
-- |      cost of `fail` + `catchTag`.
-- |
-- | All four loops are run inside one `Aff` block so the harness times
-- | the work without process-launch overhead. Each scenario reports
-- | mean / stddev / min / max wall-clock nanoseconds per iteration.
module Benchmarks.Main
  ( main
  ) where

import Prelude

import Benchmarks.Harness (benchAff, benchAffWith)
import Benchmarks.Instr
  ( bindChainInstr
  , catchLoopInstr
  , failCatchOnceInstr
  , runInstr
  , serviceLoopInstr
  )
import Data.Either (Either(..))
import Benchmarks.VsAff (runVsAff)
import Data.Array (range) as Array
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import RIO.Core
  ( RIO
  , ask
  , catchTag
  , fail
  , provideAll
  , runRIO
  , runRIO'
  )
import RIO.Concurrency (parTraverse)
import Type.Proxy (Proxy(..))

type Service = { lookup :: Int -> Int }

stubService :: Service
stubService = { lookup: \n -> n + 1 }

bindChain :: forall r e. Int -> RIO r e Int
bindChain n = go 0 n
  where
  go :: Int -> Int -> RIO r e Int
  go acc 0 = pure acc
  go acc k = do
    x <- pure (acc + 1)
    go x (k - 1)

serviceLoop :: forall r' e. Int -> RIO (svc :: Service | r') e Int
serviceLoop n = go 0 n
  where
  go :: Int -> Int -> RIO (svc :: Service | r') e Int
  go acc 0 = pure acc
  go acc k = do
    svc <- ask (Proxy :: Proxy "svc")
    go (svc.lookup acc) (k - 1)

failCatchOnce :: forall r. RIO r () Int
failCatchOnce =
  catchTag (Proxy :: Proxy "oops") (\n -> pure (n + 1))
    (fail (Proxy :: Proxy "oops") (1 :: Int))

catchLoop :: forall r. Int -> RIO r () Int
catchLoop n = go 0 n
  where
  go :: Int -> Int -> RIO r () Int
  go acc 0 = pure acc
  go acc k =
    catchTag (Proxy :: Proxy "oops")
      (\(x :: Int) -> go (acc + x) (k - 1))
      (fail (Proxy :: Proxy "oops") (1 :: Int))

main :: Effect Unit
main = launchAff_ do
  liftEffect do
    log ""
    log "================================================================"
    log "  rio benchmark suite (Phase 8.4)"
    log "================================================================"
    log "  All times are wall-clock per iteration. The harness samples"
    log "  process.hrtime() before and after each invocation."
    log "  Run with `--expose-gc` for the most stable numbers."
    log ""

  let bindIterations = 100

  benchAff
    ("bind chain (" <> show bindIterations <> " binds)")
    (void (runRIO' (bindChain bindIterations)))

  benchAff
    ("ask + Record.get (" <> show bindIterations <> " iterations)")
    ( void
        ( runRIO
            (provideAll { svc: stubService } (serviceLoop bindIterations))
        )
    )

  let arr = Array.range 1 32

  benchAff
    "traverse (32 elements, pure work)"
    ( void
        ( runRIO'
            (traverse (\n -> pure (n + 1) :: RIO () () Int) arr)
        )
    )

  benchAff
    "parTraverse (32 elements, pure work)"
    ( void
        ( runRIO'
            (parTraverse (\n -> pure (n + 1) :: RIO () () Int) arr)
        )
    )

  benchAff
    "fail + catchTag (1 round-trip)"
    (void (runRIO' failCatchOnce))

  -- Reference baseline: how long does a no-op pure RIO action take?
  benchAff "baseline runRIO' (pure unit)" (void (runRIO' (pure unit)))

  -- Reference baseline: how long does it take to just execute an Aff
  -- pure unit? Subtract from RIO numbers to see the wrapping overhead.
  benchAff "baseline Aff (pure unit)" (pure unit :: Aff Unit)

  -- A larger sustained loop, to amortise launch overhead.
  let
    deepIterations = 10000
  benchAff
    ("bind chain (" <> show deepIterations <> " binds, single sample)")
    (void (runRIO' (bindChain deepIterations)))

  -- Provide-loop with no service indirection, for comparison with the
  -- service variant.
  benchAff
    ("pure-only loop (" <> show bindIterations <> " iterations, no service)")
    (void (runRIO' (loopPure bindIterations)))

  runVsAff

  runInstrBench

  liftEffect (log "")
  liftEffect (log "rio-benchmarks: done.")
  liftEffect (log "")

-- | Spike: instruction-list-encoded mini-RIO with a hand-rolled
-- | synchronous interpreter. Compared head-to-head with the
-- | production closure-based RIO on the same workloads.
runInstrBench :: Aff Unit
runInstrBench = do
  liftEffect do
    log ""
    log "================================================================"
    log "  Instruction-list spike vs production RIO (head-to-head)"
    log "================================================================"
    log ""

  let
    bindIters = 10000
    serviceIters = 10000
    catchIters = 10000
    sampleCount = 200

  -- Sanity assertion: catch round-trip returns Right 2.
  sanity <- runInstr {} failCatchOnceInstr
  liftEffect case sanity of
    Right 2 -> log "  catchTag sanity OK: Right 2"
    other -> log ("  catchTag SANITY FAILED: " <> show other)

  benchAffWith sampleCount
    "RIO bind chain (10000 binds)"
    (void (runRIO' (bindChain bindIters)))

  benchAffWith sampleCount
    "Instr bind chain (10000 binds)"
    (void (runInstr {} (bindChainInstr bindIters)))

  benchAffWith sampleCount
    "RIO service loop (10000 ask + lookup)"
    ( void
        ( runRIO
            (provideAll { svc: stubService } (serviceLoop serviceIters))
        )
    )

  benchAffWith sampleCount
    "Instr service loop (10000 ask + lookup)"
    (void (runInstr { svc: stubService } (serviceLoopInstr serviceIters)))

  benchAffWith sampleCount
    "RIO fail + catchTag (1 round-trip)"
    (void (runRIO' failCatchOnce))

  benchAffWith sampleCount
    "Instr fail + catchTag (1 round-trip)"
    (void (runInstr {} failCatchOnceInstr))

  benchAffWith sampleCount
    "RIO catch loop (10000 round-trips)"
    (void (runRIO' (catchLoop catchIters)))

  benchAffWith sampleCount
    "Instr catch loop (10000 round-trips)"
    (void (runInstr {} (catchLoopInstr catchIters)))

  liftEffect do
    log ""
    log "Reading the table: the Instr rows are the spike interpreter"
    log "running the same workload shape. A smaller mean means the"
    log "instruction-list encoding is faster on that workload; a"
    log "comparable or larger mean means the spike is not worth the"
    log "rewrite cost."
    log ""

loopPure :: forall r e. Int -> RIO r e Int
loopPure n = go 0 n
  where
  go :: Int -> Int -> RIO r e Int
  go acc 0 = pure acc
  go acc k = do
    let acc' = acc + 1
    go acc' (k - 1)
