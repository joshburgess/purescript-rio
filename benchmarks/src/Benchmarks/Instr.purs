-- | Spike: instruction-list-encoded mini-RIO with a hand-rolled
-- | synchronous interpreter, for a like-for-like perf comparison
-- | against the production closure-based `RIO`.
-- |
-- | Phase 2 adds the `ASYNC` instruction. The interpreter is now a
-- | step/resume machine: `_stepInstr` runs the inner loop until the
-- | computation either completes or hits an `ASYNC` node. On `ASYNC`
-- | it stashes the pending `Aff` and returns; the PureScript driver
-- | runs the `Aff`, threads the result back via `_resumeInstr`, and
-- | calls `_stepInstr` again. Synchronous work still runs in a tight
-- | FFI loop with zero `Aff` overhead per bind; the `Aff` cost is
-- | paid only at actual async boundaries.
-- |
-- | See `INSTR_LIST_REWRITE_PLAN.md` at the repo root for the full
-- | roadmap and exit criteria.
module Benchmarks.Instr
  ( Instr
  , runInstr
  , instrPure
  , instrBind
  , instrAsk
  , instrLiftEffect
  , instrLiftAff
  , instrFail
  , instrFailTag
  , instrCatchTag
  , instrLocal
  , bindChainInstr
  , serviceLoopInstr
  , failCatchOnceInstr
  , catchLoopInstr
  , asyncSanityInstr
  , asyncLoopInstr
  , mixedLoopInstr
  , InstrFiber
  , instrForkFiber
  , instrJoinFiber
  , instrParTraverse
  , instrBracket
  , bracketSanityInstr
  , bracketLoopInstr
  , refCounterLoopInstr
  , fanOutFanInInstr
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence, traverse)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Prim.Row as Row
import Type.Proxy (Proxy(..))

-- | Opaque ADT-encoded computation. Each constructor is a tagged
-- | JS object built by the FFI factories; the interpreter reads
-- | the tag and dispatches.
foreign import data Instr :: Row Type -> Row Type -> Type -> Type

-- | Opaque mutable interpreter state. One per `runInstr` call.
foreign import data InstrState :: Row Type -> Row Type -> Type -> Type

-- | Opaque foreign value used to shuttle the result of an `ASYNC`
-- | `Aff` from the driver back into the interpreter without
-- | requiring any type witness at the PureScript boundary. The
-- | bind continuation that consumes it on the interpreter side
-- | already has the right type baked in by the user's program.
foreign import data ForeignValue :: Type

foreign import instrPure :: forall r e a. a -> Instr r e a

foreign import instrLiftEffect :: forall r e a. Effect a -> Instr r e a

foreign import instrBind
  :: forall r e a b
   . Instr r e a
  -> (a -> Instr r e b)
  -> Instr r e b

foreign import instrAsk :: forall r e. Instr r e (Record r)

foreign import instrFail :: forall r e a. Variant e -> Instr r e a

foreign import instrAsync :: forall r e a. Aff a -> Instr r e a

foreign import _instrCatchTag
  :: forall r e e' a x
   . String
  -> (x -> Instr r e' a)
  -> Instr r e a
  -> Instr r e' a

foreign import _instrLocal
  :: forall r r' e a
   . (Record r -> Record r')
  -> Instr r' e a
  -> Instr r e a

foreign import _initInstrState
  :: forall r e a. Record r -> Instr r e a -> Effect (InstrState r e a)

foreign import _stepInstr
  :: forall r e a. InstrState r e a -> Effect Unit

foreign import _resumeInstr
  :: forall r e a. InstrState r e a -> ForeignValue -> Effect Unit

-- | Combined resume + step. Installs the previous Aff's result
-- | as the next interpreter result and immediately runs the
-- | inner loop until the next suspension or completion. Folds
-- | one Aff bind out of the hot path on every iteration that
-- | suspends on `ASYNC`.
foreign import _resumeAndStep
  :: forall r e a. InstrState r e a -> ForeignValue -> Effect Unit

foreign import _isDone :: forall r e a. InstrState r e a -> Boolean
foreign import _isRightFinal :: forall r e a. InstrState r e a -> Boolean
foreign import _finalRight :: forall r e a. InstrState r e a -> a
foreign import _finalLeft :: forall r e a. InstrState r e a -> Variant e
foreign import _pendingAff :: forall r e a. InstrState r e a -> Aff ForeignValue

-- | Run an `Instr` against an environment record. The interpreter
-- | runs synchronously inside a tight FFI loop until it either
-- | completes or suspends on an `ASYNC` instruction; the driver
-- | here runs the pending `Aff`, threads its result back into the
-- | interpreter, and re-enters the loop. Synchronous-only programs
-- | pay only one `liftEffect` + one `Aff` `pure` total.
runInstr
  :: forall r e a
   . Record r
  -> Instr r e a
  -> Aff (Either (Variant e) a)
runInstr env instr = do
  state <- liftEffect (_initInstrState env instr)
  liftEffect (_stepInstr state)
  drive state
  where
  drive :: InstrState r e a -> Aff (Either (Variant e) a)
  drive state =
    if _isDone state then
      pure
        if _isRightFinal state then Right (_finalRight state)
        else Left (_finalLeft state)
    else do
      v <- _pendingAff state
      liftEffect (_resumeAndStep state v)
      drive state

instance functorInstr :: Functor (Instr r e) where
  map f i = instrBind i (\a -> instrPure (f a))

instance applyInstr :: Apply (Instr r e) where
  apply f x = instrBind f \fn -> instrBind x \v -> instrPure (fn v)

instance applicativeInstr :: Applicative (Instr r e) where
  pure = instrPure

instance bindInstr :: Bind (Instr r e) where
  bind = instrBind

instance monadInstr :: Monad (Instr r e)

-- | Canonical `Aff`-lifting primitive. Wraps any `Aff a` (sync or
-- | async) as an `Instr` node that suspends the interpreter,
-- | runs the `Aff` via the driver, and resumes with its result.
instrLiftAff :: forall r e a. Aff a -> Instr r e a
instrLiftAff = instrAsync

-- | Tagged failure constructor. Mirrors production
-- | `RIO.fail :: Proxy sym -> x -> RIO r e a`.
instrFailTag
  :: forall sym r e e' x a
   . IsSymbol sym
  => Row.Cons sym x e' e
  => Proxy sym
  -> x
  -> Instr r e a
instrFailTag proxy x = instrFail (Variant.inj proxy x)

-- | Catch a typed failure by label. The handler removes the
-- | caught label from the output failure row.
-- |
-- | Mirrors production `RIO.catchTag`.
instrCatchTag
  :: forall sym r e e' a x
   . IsSymbol sym
  => Row.Cons sym x e' e
  => Row.Lacks sym e'
  => Proxy sym
  -> (x -> Instr r e' a)
  -> Instr r e a
  -> Instr r e' a
instrCatchTag proxy handler m =
  _instrCatchTag (reflectSymbol proxy) handler m

-- | Run `inner` with the env transformed by `modify`. The
-- | interpreter records the current env on an env-restore stack,
-- | runs `inner` against the modified env, and restores on
-- | scope exit (normal or failure). `provide` / `provideAll`
-- | are built on top of this.
instrLocal
  :: forall r r' e a
   . (Record r -> Record r')
  -> Instr r' e a
  -> Instr r e a
instrLocal modify inner = _instrLocal modify inner

-- | The same bind-chain workload `bindChain` runs against `RIO`,
-- | rewritten against `Instr`. The body never touches the env.
bindChainInstr :: forall r e. Int -> Instr r e Int
bindChainInstr n = go 0 n
  where
  go :: Int -> Int -> Instr r e Int
  go acc 0 = instrPure acc
  go acc k = instrBind (instrPure (acc + 1)) \x -> go x (k - 1)

-- | A service-loop workload that exercises the `Ask` instruction
-- | per iteration. Mirrors `serviceLoop` from the production
-- | bench.
serviceLoopInstr
  :: forall r' e
   . Int
  -> Instr (svc :: { lookup :: Int -> Int } | r') e Int
serviceLoopInstr n = go 0 n
  where
  go :: Int -> Int -> Instr (svc :: { lookup :: Int -> Int } | r') e Int
  go acc 0 = instrPure acc
  go acc k = instrBind instrAsk \env ->
    let
      _ = Proxy :: Proxy "svc"
    in
      go (env.svc.lookup acc) (k - 1)

-- | Same shape as production `failCatchOnce`: throw a typed
-- | failure and catch it immediately. Single round-trip.
failCatchOnceInstr :: forall r. Instr r () Int
failCatchOnceInstr =
  instrCatchTag (Proxy :: Proxy "oops") (\(n :: Int) -> instrPure (n + 1))
    (instrFailTag (Proxy :: Proxy "oops") 1)

-- | A loop that does one catchTag round-trip per iteration.
-- | Measures the per-iteration cost of pushing a catch frame,
-- | failing, unwinding, and resuming via the handler.
catchLoopInstr :: forall r. Int -> Instr r () Int
catchLoopInstr n = go 0 n
  where
  go :: Int -> Int -> Instr r () Int
  go acc 0 = instrPure acc
  go acc k =
    instrCatchTag (Proxy :: Proxy "oops")
      (\(x :: Int) -> go (acc + x) (k - 1))
      (instrFailTag (Proxy :: Proxy "oops") 1)

-- | Sanity check for the `ASYNC` bridge: lift `pure 42` from `Aff`,
-- | bind the result, and add one. The driver must suspend, run the
-- | inner `Aff`, resume with `42`, and let the continuation see it
-- | so the final answer is `Right 43`.
asyncSanityInstr :: forall r e. Instr r e Int
asyncSanityInstr =
  instrBind (instrAsync (pure 42)) \n -> instrPure (n + 1)

-- | A loop that suspends on `ASYNC` once per iteration. The inner
-- | `Aff` is just `pure (acc + 1)` so the only async cost is the
-- | step/resume round-trip; this isolates that overhead from any
-- | actual scheduling work. Mirrors the kind of workload `RIO`'s
-- | `liftAff (pure ...)` loop measures.
asyncLoopInstr :: forall r e. Int -> Instr r e Int
asyncLoopInstr n = go 0 n
  where
  go :: Int -> Int -> Instr r e Int
  go acc 0 = instrPure acc
  go acc k = instrBind (instrAsync (pure (acc + 1))) \x -> go x (k - 1)

-- | A more realistic workload: 9 synchronous binds between each
-- | `ASYNC` suspension. The synchronous portion runs entirely inside
-- | the FFI loop so it pays no `Aff` cost; only the per-iteration
-- | `ASYNC` round-trip touches the driver. This shows the
-- | crossover where the instruction-list encoding wins again, even
-- | with async in the mix.
mixedLoopInstr :: forall r e. Int -> Instr r e Int
mixedLoopInstr n = go 0 n
  where
  go :: Int -> Int -> Instr r e Int
  go acc 0 = instrPure acc
  go acc k =
    instrBind (instrPure (acc + 1)) \a1 ->
      instrBind (instrPure (a1 + 1)) \a2 ->
        instrBind (instrPure (a2 + 1)) \a3 ->
          instrBind (instrPure (a3 + 1)) \a4 ->
            instrBind (instrPure (a4 + 1)) \a5 ->
              instrBind (instrPure (a5 + 1)) \a6 ->
                instrBind (instrPure (a6 + 1)) \a7 ->
                  instrBind (instrPure (a7 + 1)) \a8 ->
                    instrBind (instrPure (a8 + 1)) \a9 ->
                      instrBind (instrAsync (pure (a9 + 1))) \a10 ->
                        go a10 (k - 1)

-- | Spike-side fiber handle. Wraps the underlying `Aff` fiber that
-- | runs `runInstr` to completion. Phase 3 is intentionally
-- | thin: production RIO's `Fiber` carries an `Exit` Ref and a
-- | `FiberId`; the spike just needs join semantics, so we keep
-- | the wrapper minimal until those features are needed.
newtype InstrFiber e a =
  InstrFiber (Aff.Fiber (Either (Variant e) a))

-- | Fork an `Instr` into a new `Aff` fiber. The child captures the
-- | current env via `instrAsk` so it runs in the same environment
-- | record as the parent. The parent is infallible: the `e'` row
-- | on the result is polymorphic because no typed failure is
-- | produced by the act of forking itself.
instrForkFiber
  :: forall r e e' a
   . Instr r e a
  -> Instr r e' (InstrFiber e a)
instrForkFiber inner = instrBind instrAsk \env ->
  instrAsync do
    fib <- Aff.forkAff (runInstr env inner)
    pure (InstrFiber fib)

-- | Wait for a forked fiber to complete and unwrap its result.
-- | A typed failure on the child re-fails on the parent;
-- | a successful result is the value of `instrJoinFiber`.
instrJoinFiber :: forall r e a. InstrFiber e a -> Instr r e a
instrJoinFiber (InstrFiber fib) =
  instrBind (instrAsync (Aff.joinFiber fib)) case _ of
    Right a -> instrPure a
    Left v -> instrFail v

-- | Parallel traversal. Each `f x` is run as an independent
-- | `Aff` via `Aff.parallel`, all merged with `Aff.sequential`,
-- | so the scheduler interleaves them. If any sub-computation
-- | fails, we propagate the first failure; otherwise we return
-- | the collected results in input order.
instrParTraverse
  :: forall r e a b
   . (a -> Instr r e b)
  -> Array a
  -> Instr r e (Array b)
instrParTraverse f arr = instrBind instrAsk \env ->
  let
    runOne x = Aff.parallel (runInstr env (f x))
  in
    instrBind
      (instrAsync (Aff.sequential (traverse runOne arr)))
      \results -> case sequence results of
        Right xs -> instrPure xs
        Left v -> instrFail v

-- | Fan-out / fan-in workload mirroring the production
-- | `RIO.fork x16 + awaitAll` shape. Forks `n` children, each of
-- | which does a trivial pure computation, then joins all of them
-- | in input order.
fanOutFanInInstr :: forall r e. Array Int -> Instr r e (Array Int)
fanOutFanInInstr arr =
  instrBind (traverse (\n -> instrForkFiber (instrPure (n + 1))) arr) \fibs ->
    traverse instrJoinFiber fibs

-- | Acquire / use / release with guaranteed cleanup. Mirrors
-- | production `RIO.Resource.bracket`:
-- |
-- |   * `release` runs on every termination of `use` (success,
-- |     typed failure, defect, or external fiber kill).
-- |   * Typed failures from `release` itself are silently
-- |     swallowed - the spike does not surface a separate
-- |     `acquireRelease` for now.
-- |   * Defects from `release` propagate (Aff exceptions
-- |     observable via `sandbox`).
-- |
-- | Implementation delegates to `Aff.bracket`, with `runInstr`
-- | running each of the three sub-`Instr`s against the captured
-- | env. Typed failures are threaded through the bracket via the
-- | `Either (Variant e) _` carrier.
instrBracket
  :: forall r e a b
   . Instr r e a
  -> (a -> Instr r e Unit)
  -> (a -> Instr r e b)
  -> Instr r e b
instrBracket acquire release use = instrBind instrAsk \env ->
  let
    runOne :: forall x. Instr r e x -> Aff (Either (Variant e) x)
    runOne = runInstr env

    body :: Aff (Either (Variant e) b)
    body = Aff.bracket
      (runOne acquire)
      ( \aE -> case aE of
          Right a -> void (runOne (release a))
          Left _ -> pure unit
      )
      ( \aE -> case aE of
          Right a -> runOne (use a)
          Left v -> pure (Left v)
      )
  in
    instrBind (instrAsync body) case _ of
      Right b -> instrPure b
      Left v -> instrFail v

-- | Sanity workload for `instrBracket`: acquire a Ref counter,
-- | bump it inside `use`, and bump it again inside `release`.
-- | A correct interpreter returns `release` having run after
-- | `use`, so the final Ref value is 2 and the workload's result
-- | is 1 (the value `use` saw after its own bump).
bracketSanityInstr :: forall r e. Ref Int -> Instr r e Int
bracketSanityInstr counter =
  instrBracket
    (instrLiftEffect (Ref.read counter))
    (\_ -> instrLiftEffect (Ref.modify_ (_ + 1) counter))
    ( \_ -> do
        _ <- instrLiftEffect (Ref.modify_ (_ + 1) counter)
        instrLiftEffect (Ref.read counter)
    )

-- | Bench workload: acquire/use/release loop. Each iteration is
-- | a complete bracket round-trip with a trivial acquire and a
-- | trivial release. Measures the per-bracket interpreter cost
-- | (two extra `runInstr` invocations plus an `Aff.bracket`).
bracketLoopInstr :: forall r e. Int -> Instr r e Int
bracketLoopInstr n = go 0 n
  where
  go :: Int -> Int -> Instr r e Int
  go acc 0 = instrPure acc
  go acc k =
    instrBind
      ( instrBracket
          (instrPure (acc + 1))
          (\_ -> instrPure unit)
          (\x -> instrPure x)
      )
      \x -> go x (k - 1)

-- | A loop that reads, increments, and writes an `Effect.Ref` once
-- | per iteration, all lifted through `instrLiftEffect`. There is
-- | no new interpreter machinery here - this just shows that
-- | mutable state composes naturally through the `SYNC` tag we
-- | already have. Mirrors a `Ref`-based loop one would write
-- | against production `RIO` with `liftEffect` + `Effect.Ref`.
refCounterLoopInstr :: forall r e. Ref Int -> Int -> Instr r e Int
refCounterLoopInstr ref n = go 0 n
  where
  go :: Int -> Int -> Instr r e Int
  go acc 0 = instrPure acc
  go _ k =
    instrBind (instrLiftEffect (Ref.modify (_ + 1) ref)) \x ->
      go x (k - 1)
