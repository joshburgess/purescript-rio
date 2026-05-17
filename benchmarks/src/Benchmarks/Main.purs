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
  ( Instr
  , asyncLoopInstr
  , asyncSanityInstr
  , bindChainInstr
  , bracketLoopInstr
  , bracketSanityInstr
  , catchLoopInstr
  , failCatchOnceInstr
  , fanOutFanInInstr
  , instrLocal
  , instrParTraverse
  , mixedLoopInstr
  , refCounterLoopInstr
  , runInstr
  , serviceLoopInstr
  )
import Data.Either (Either(..))
import Benchmarks.VsAff (runVsAff)
import Data.Array (range) as Array
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref as Ref
import RIO.Core
  ( RIO
  , ask
  , catchTag
  , fail
  , provideAll
  , runRIO
  , runRIO'
  )
import RIO.Concurrency (awaitAll, fork, parTraverse)
import RIO.Resource (bracket)
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

-- | RIO equivalent of `asyncLoopInstr`: lift `pure (acc + 1)` through
-- | `Aff` once per iteration. Each `liftAff` traverses Aff's per-step
-- | machinery; the Instr spike's `asyncLoopInstr` runs the same
-- | workload through its step/resume bridge.
asyncLoop :: forall r e. Int -> RIO r e Int
asyncLoop n = go 0 n
  where
  go :: Int -> Int -> RIO r e Int
  go acc 0 = pure acc
  go acc k = do
    x <- liftAff (pure (acc + 1))
    go x (k - 1)

-- | RIO counterpart of `bracketLoopInstr`: each iteration runs a
-- | full `bracket` round-trip with trivial acquire / use / release.
bracketLoop :: forall r e. Int -> RIO r e Int
bracketLoop n = go 0 n
  where
  go :: Int -> Int -> RIO r e Int
  go acc 0 = pure acc
  go acc k = do
    x <- bracket (pure (acc + 1)) (\_ -> pure unit) (\v -> pure v)
    go x (k - 1)

-- | RIO counterpart of `refCounterLoopInstr`: increment a shared
-- | `Effect.Ref` via `liftEffect` once per iteration.
refCounterLoop :: forall r e. Ref.Ref Int -> Int -> RIO r e Int
refCounterLoop ref n = go n
  where
  go :: Int -> RIO r e Int
  go 0 = liftEffect (Ref.read ref)
  go k = do
    _ <- liftEffect (Ref.modify (_ + 1) ref)
    go (k - 1)

-- | RIO counterpart of `mixedLoopInstr`: 9 synchronous binds for
-- | every `liftAff` to model a realistic ratio between in-process
-- | work and Aff suspensions.
mixedLoop :: forall r e. Int -> RIO r e Int
mixedLoop n = go 0 n
  where
  go :: Int -> Int -> RIO r e Int
  go acc 0 = pure acc
  go acc k = do
    a1 <- pure (acc + 1)
    a2 <- pure (a1 + 1)
    a3 <- pure (a2 + 1)
    a4 <- pure (a3 + 1)
    a5 <- pure (a4 + 1)
    a6 <- pure (a5 + 1)
    a7 <- pure (a6 + 1)
    a8 <- pure (a7 + 1)
    a9 <- pure (a8 + 1)
    a10 <- liftAff (pure (a9 + 1))
    go a10 (k - 1)

-- | Apples-to-apples with the production
-- | `provideAll { svc: stubService } (serviceLoop n)` workload:
-- | start with an empty env, install `svc` via `instrLocal`,
-- | then run the service loop that asks for it each iteration.
providedLoopInstr :: forall e. Int -> Instr () e Int
providedLoopInstr n =
  instrLocal (\_ -> { svc: stubService }) (serviceLoopInstr n)

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

  -- Sanity assertion: ASYNC round-trip returns Right 43.
  asyncSanity <- runInstr {} (asyncSanityInstr :: Instr () () Int)
  liftEffect case asyncSanity of
    Right 43 -> log "  ASYNC sanity OK: Right 43"
    other -> log ("  ASYNC SANITY FAILED: " <> show other)

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
    "Instr service loop (10000 ask + lookup, env via runInstr)"
    (void (runInstr { svc: stubService } (serviceLoopInstr serviceIters)))

  benchAffWith sampleCount
    "Instr provided service loop (10000 ask + lookup, env via instrLocal)"
    (void (runInstr {} (providedLoopInstr serviceIters)))

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

  -- Stack-safety smoke check: 1M binds in the interpreter loop
  -- must not blow the JS call stack and must complete in a
  -- reasonable budget. 10 samples is plenty; we just want to
  -- confirm the design is sound at scale.
  let stackIters = 1000000

  benchAffWith 10
    "Instr bind chain (1M binds, stack safety)"
    (void (runInstr {} (bindChainInstr stackIters)))

  benchAffWith 10
    "RIO bind chain (1M binds, stack safety)"
    (void (runRIO' (bindChain stackIters)))

  -- Phase 2: ASYNC bridge head-to-head. RIO's liftAff loop pays Aff
  -- per step; Instr's instrAsync loop pays one step/resume round-trip
  -- per iteration plus Aff for the inner pure.
  let asyncIters = 10000

  benchAffWith sampleCount
    ("RIO liftAff loop (" <> show asyncIters <> " iterations)")
    (void (runRIO' (asyncLoop asyncIters)))

  benchAffWith sampleCount
    ("Instr instrAsync loop (" <> show asyncIters <> " iterations)")
    (void (runInstr {} (asyncLoopInstr asyncIters)))

  -- Mixed workload: 9 sync binds per async hop. This is closer to
  -- realistic application code where most work is in-process and
  -- only occasional steps cross the Aff boundary.
  let mixedIters = 1000

  benchAffWith sampleCount
    ("RIO mixed loop (" <> show mixedIters <> " iters, 9 sync + 1 liftAff)")
    (void (runRIO' (mixedLoop mixedIters)))

  benchAffWith sampleCount
    ("Instr mixed loop (" <> show mixedIters <> " iters, 9 sync + 1 instrAsync)")
    (void (runInstr {} (mixedLoopInstr mixedIters)))

  -- Phase 3: concurrency head-to-head. parTraverse and fan-out/fan-in.
  let
    parArr = Array.range 1 32
    fanArr = Array.range 1 16

  benchAffWith sampleCount
    "RIO parTraverse (32 elements, pure work)"
    ( void
        ( runRIO'
            (parTraverse (\n -> pure (n + 1) :: RIO () () Int) parArr)
        )
    )

  benchAffWith sampleCount
    "Instr parTraverse (32 elements, pure work)"
    ( void
        ( runInstr {}
            (instrParTraverse (\n -> pure (n + 1) :: Instr () () Int) parArr)
        )
    )

  benchAffWith sampleCount
    "RIO fan-out/fan-in (fork x16 + awaitAll)"
    ( void
        ( runRIO' do
            fibs <- traverse (\n -> fork (pure (n + 1) :: RIO () () Int)) fanArr
            awaitAll fibs
        )
    )

  benchAffWith sampleCount
    "Instr fan-out/fan-in (instrForkFiber x16 + join)"
    (void (runInstr {} (fanOutFanInInstr fanArr)))

  -- Phase 4: bracket and Ref head-to-head.
  --
  -- Sanity: bracket must run release after use. Counter starts at
  -- 0; use bumps it to 1 (its read returns 1); release bumps it
  -- to 2. Bracket returns the use's result (1). Final counter
  -- read is 2.
  bracketCounter <- liftEffect (Ref.new 0)
  bracketResult <- runInstr {}
    (bracketSanityInstr bracketCounter :: Instr () () Int)
  finalCounter <- liftEffect (Ref.read bracketCounter)
  liftEffect case bracketResult, finalCounter of
    Right 1, 2 -> log "  bracket sanity OK: result=1, counter=2"
    Right r, c -> log
      ( "  bracket SANITY FAILED: result=Right "
          <> show r
          <> ", counter="
          <> show c
      )
    Left _, c -> log
      ("  bracket SANITY FAILED: typed failure, counter=" <> show c)

  let bracketIters = 10000

  benchAffWith sampleCount
    ("RIO bracket loop (" <> show bracketIters <> " round-trips)")
    (void (runRIO' (bracketLoop bracketIters)))

  benchAffWith sampleCount
    ("Instr bracket loop (" <> show bracketIters <> " round-trips)")
    (void (runInstr {} (bracketLoopInstr bracketIters)))

  -- Ref counter loop: mutable state via liftEffect, no new
  -- spike machinery (just SYNC under the hood).
  let refIters = 10000

  benchAffWith sampleCount
    ("RIO Ref counter loop (" <> show refIters <> " modifies)")
    ( void do
        ref <- liftEffect (Ref.new 0)
        runRIO' (refCounterLoop ref refIters)
    )

  benchAffWith sampleCount
    ("Instr Ref counter loop (" <> show refIters <> " modifies)")
    ( void do
        ref <- liftEffect (Ref.new 0)
        runInstr {} (refCounterLoopInstr ref refIters)
    )

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
