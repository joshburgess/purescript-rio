-- | Spike: operation-list-encoded mini-RIO with a hand-rolled
-- | synchronous interpreter, for a like-for-like perf comparison
-- | against the production closure-based `RIO`.
-- |
-- | Phase 2 adds the `ASYNC` operation. The interpreter is now a
-- | step/resume machine: `_stepOp` runs the inner loop until the
-- | computation either completes or hits an `ASYNC` node. On `ASYNC`
-- | it stashes the pending `Aff` and returns; the PureScript driver
-- | runs the `Aff`, threads the result back via `_resumeOp`, and
-- | calls `_stepOp` again. Synchronous work still runs in a tight
-- | FFI loop with zero `Aff` overhead per bind; the `Aff` cost is
-- | paid only at actual async boundaries.
module Benchmarks.Op
  ( Op
  , runOp
  , opPure
  , opBind
  , opAsk
  , opLiftEffect
  , opLiftAff
  , opFail
  , opFailTag
  , opCatchTag
  , opLocal
  , bindChainOp
  , serviceLoopOp
  , failCatchOnceOp
  , catchLoopOp
  , asyncSanityOp
  , asyncLoopOp
  , mixedLoopOp
  , OpFiber
  , opForkFiber
  , opJoinFiber
  , opParTraverse
  , opBracket
  , bracketSanityOp
  , bracketLoopOp
  , refCounterLoopOp
  , fanOutFanInOp
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
foreign import data Op :: Row Type -> Row Type -> Type -> Type

-- | Opaque mutable interpreter state. One per `runOp` call.
foreign import data OpState :: Row Type -> Row Type -> Type -> Type

-- | Opaque foreign value used to shuttle the result of an `ASYNC`
-- | `Aff` from the driver back into the interpreter without
-- | requiring any type witness at the PureScript boundary. The
-- | bind continuation that consumes it on the interpreter side
-- | already has the right type baked in by the user's program.
foreign import data ForeignValue :: Type

foreign import opPure :: forall r e a. a -> Op r e a

foreign import opLiftEffect :: forall r e a. Effect a -> Op r e a

foreign import opBind
  :: forall r e a b
   . Op r e a
  -> (a -> Op r e b)
  -> Op r e b

foreign import opAsk :: forall r e. Op r e (Record r)

foreign import opFail :: forall r e a. Variant e -> Op r e a

foreign import opAsync :: forall r e a. Aff a -> Op r e a

foreign import _opCatchTag
  :: forall r e e' a x
   . String
  -> (x -> Op r e' a)
  -> Op r e a
  -> Op r e' a

foreign import _opLocal
  :: forall r r' e a
   . (Record r -> Record r')
  -> Op r' e a
  -> Op r e a

foreign import _initOpState
  :: forall r e a. Record r -> Op r e a -> Effect (OpState r e a)

foreign import _stepOp
  :: forall r e a. OpState r e a -> Effect Unit

foreign import _resumeOp
  :: forall r e a. OpState r e a -> ForeignValue -> Effect Unit

-- | Combined resume + step. Installs the previous Aff's result
-- | as the next interpreter result and immediately runs the
-- | inner loop until the next suspension or completion. Folds
-- | one Aff bind out of the hot path on every iteration that
-- | suspends on `ASYNC`.
foreign import _resumeAndStep
  :: forall r e a. OpState r e a -> ForeignValue -> Effect Unit

foreign import _isDone :: forall r e a. OpState r e a -> Boolean
foreign import _isRightFinal :: forall r e a. OpState r e a -> Boolean
foreign import _finalRight :: forall r e a. OpState r e a -> a
foreign import _finalLeft :: forall r e a. OpState r e a -> Variant e
foreign import _pendingAff :: forall r e a. OpState r e a -> Aff ForeignValue

-- | Run an `Op` against an environment record. The interpreter
-- | runs synchronously inside a tight FFI loop until it either
-- | completes or suspends on an `ASYNC` operation; the driver
-- | here runs the pending `Aff`, threads its result back into the
-- | interpreter, and re-enters the loop. Synchronous-only programs
-- | pay only one `liftEffect` + one `Aff` `pure` total.
runOp
  :: forall r e a
   . Record r
  -> Op r e a
  -> Aff (Either (Variant e) a)
runOp env op = do
  state <- liftEffect (_initOpState env op)
  liftEffect (_stepOp state)
  drive state
  where
  drive :: OpState r e a -> Aff (Either (Variant e) a)
  drive state =
    if _isDone state then
      pure
        if _isRightFinal state then Right (_finalRight state)
        else Left (_finalLeft state)
    else do
      v <- _pendingAff state
      liftEffect (_resumeAndStep state v)
      drive state

instance functorOp :: Functor (Op r e) where
  map f i = opBind i (\a -> opPure (f a))

instance applyOp :: Apply (Op r e) where
  apply f x = opBind f \fn -> opBind x \v -> opPure (fn v)

instance applicativeOp :: Applicative (Op r e) where
  pure = opPure

instance bindOp :: Bind (Op r e) where
  bind = opBind

instance monadOp :: Monad (Op r e)

-- | Canonical `Aff`-lifting primitive. Wraps any `Aff a` (sync or
-- | async) as an `Op` node that suspends the interpreter,
-- | runs the `Aff` via the driver, and resumes with its result.
opLiftAff :: forall r e a. Aff a -> Op r e a
opLiftAff = opAsync

-- | Tagged failure constructor. Mirrors production
-- | `RIO.fail :: Proxy sym -> x -> RIO r e a`.
opFailTag
  :: forall sym r e e' x a
   . IsSymbol sym
  => Row.Cons sym x e' e
  => Proxy sym
  -> x
  -> Op r e a
opFailTag proxy x = opFail (Variant.inj proxy x)

-- | Catch a typed failure by label. The handler removes the
-- | caught label from the output failure row.
-- |
-- | Mirrors production `RIO.catchTag`.
opCatchTag
  :: forall sym r e e' a x
   . IsSymbol sym
  => Row.Cons sym x e' e
  => Row.Lacks sym e'
  => Proxy sym
  -> (x -> Op r e' a)
  -> Op r e a
  -> Op r e' a
opCatchTag proxy handler m =
  _opCatchTag (reflectSymbol proxy) handler m

-- | Run `inner` with the env transformed by `modify`. The
-- | interpreter records the current env on an env-restore stack,
-- | runs `inner` against the modified env, and restores on
-- | scope exit (normal or failure). `provide` / `provideAll`
-- | are built on top of this.
opLocal
  :: forall r r' e a
   . (Record r -> Record r')
  -> Op r' e a
  -> Op r e a
opLocal modify inner = _opLocal modify inner

-- | The same bind-chain workload `bindChain` runs against `RIO`,
-- | rewritten against `Op`. The body never touches the env.
bindChainOp :: forall r e. Int -> Op r e Int
bindChainOp n = go 0 n
  where
  go :: Int -> Int -> Op r e Int
  go acc 0 = opPure acc
  go acc k = opBind (opPure (acc + 1)) \x -> go x (k - 1)

-- | A service-loop workload that exercises the `Ask` operation
-- | per iteration. Mirrors `serviceLoop` from the production
-- | bench.
serviceLoopOp
  :: forall r' e
   . Int
  -> Op (svc :: { lookup :: Int -> Int } | r') e Int
serviceLoopOp n = go 0 n
  where
  go :: Int -> Int -> Op (svc :: { lookup :: Int -> Int } | r') e Int
  go acc 0 = opPure acc
  go acc k = opBind opAsk \env ->
    let
      _ = Proxy :: Proxy "svc"
    in
      go (env.svc.lookup acc) (k - 1)

-- | Same shape as production `failCatchOnce`: throw a typed
-- | failure and catch it immediately. Single round-trip.
failCatchOnceOp :: forall r. Op r () Int
failCatchOnceOp =
  opCatchTag (Proxy :: Proxy "oops") (\(n :: Int) -> opPure (n + 1))
    (opFailTag (Proxy :: Proxy "oops") 1)

-- | A loop that does one catchTag round-trip per iteration.
-- | Measures the per-iteration cost of pushing a catch frame,
-- | failing, unwinding, and resuming via the handler.
catchLoopOp :: forall r. Int -> Op r () Int
catchLoopOp n = go 0 n
  where
  go :: Int -> Int -> Op r () Int
  go acc 0 = opPure acc
  go acc k =
    opCatchTag (Proxy :: Proxy "oops")
      (\(x :: Int) -> go (acc + x) (k - 1))
      (opFailTag (Proxy :: Proxy "oops") 1)

-- | Sanity check for the `ASYNC` bridge: lift `pure 42` from `Aff`,
-- | bind the result, and add one. The driver must suspend, run the
-- | inner `Aff`, resume with `42`, and let the continuation see it
-- | so the final answer is `Right 43`.
asyncSanityOp :: forall r e. Op r e Int
asyncSanityOp =
  opBind (opAsync (pure 42)) \n -> opPure (n + 1)

-- | A loop that suspends on `ASYNC` once per iteration. The inner
-- | `Aff` is just `pure (acc + 1)` so the only async cost is the
-- | step/resume round-trip; this isolates that overhead from any
-- | actual scheduling work. Mirrors the kind of workload `RIO`'s
-- | `liftAff (pure ...)` loop measures.
asyncLoopOp :: forall r e. Int -> Op r e Int
asyncLoopOp n = go 0 n
  where
  go :: Int -> Int -> Op r e Int
  go acc 0 = opPure acc
  go acc k = opBind (opAsync (pure (acc + 1))) \x -> go x (k - 1)

-- | A more realistic workload: 9 synchronous binds between each
-- | `ASYNC` suspension. The synchronous portion runs entirely inside
-- | the FFI loop so it pays no `Aff` cost; only the per-iteration
-- | `ASYNC` round-trip touches the driver. This shows the
-- | crossover where the operation-list encoding wins again, even
-- | with async in the mix.
mixedLoopOp :: forall r e. Int -> Op r e Int
mixedLoopOp n = go 0 n
  where
  go :: Int -> Int -> Op r e Int
  go acc 0 = opPure acc
  go acc k =
    opBind (opPure (acc + 1)) \a1 ->
      opBind (opPure (a1 + 1)) \a2 ->
        opBind (opPure (a2 + 1)) \a3 ->
          opBind (opPure (a3 + 1)) \a4 ->
            opBind (opPure (a4 + 1)) \a5 ->
              opBind (opPure (a5 + 1)) \a6 ->
                opBind (opPure (a6 + 1)) \a7 ->
                  opBind (opPure (a7 + 1)) \a8 ->
                    opBind (opPure (a8 + 1)) \a9 ->
                      opBind (opAsync (pure (a9 + 1))) \a10 ->
                        go a10 (k - 1)

-- | Spike-side fiber handle. Wraps the underlying `Aff` fiber that
-- | runs `runOp` to completion. Phase 3 is intentionally
-- | thin: production RIO's `Fiber` carries an `Exit` Ref and a
-- | `FiberId`; the spike just needs join semantics, so we keep
-- | the wrapper minimal until those features are needed.
newtype OpFiber e a =
  OpFiber (Aff.Fiber (Either (Variant e) a))

-- | Fork an `Op` into a new `Aff` fiber. The child captures the
-- | current env via `opAsk` so it runs in the same environment
-- | record as the parent. The parent is infallible: the `e'` row
-- | on the result is polymorphic because no typed failure is
-- | produced by the act of forking itself.
opForkFiber
  :: forall r e e' a
   . Op r e a
  -> Op r e' (OpFiber e a)
opForkFiber inner = opBind opAsk \env ->
  opAsync do
    fib <- Aff.forkAff (runOp env inner)
    pure (OpFiber fib)

-- | Wait for a forked fiber to complete and unwrap its result.
-- | A typed failure on the child re-fails on the parent;
-- | a successful result is the value of `opJoinFiber`.
opJoinFiber :: forall r e a. OpFiber e a -> Op r e a
opJoinFiber (OpFiber fib) =
  opBind (opAsync (Aff.joinFiber fib)) case _ of
    Right a -> opPure a
    Left v -> opFail v

-- | Parallel traversal. Each `f x` is run as an independent
-- | `Aff` via `Aff.parallel`, all merged with `Aff.sequential`,
-- | so the scheduler interleaves them. If any sub-computation
-- | fails, we propagate the first failure; otherwise we return
-- | the collected results in input order.
opParTraverse
  :: forall r e a b
   . (a -> Op r e b)
  -> Array a
  -> Op r e (Array b)
opParTraverse f arr = opBind opAsk \env ->
  let
    runOne x = Aff.parallel (runOp env (f x))
  in
    opBind
      (opAsync (Aff.sequential (traverse runOne arr)))
      \results -> case sequence results of
        Right xs -> opPure xs
        Left v -> opFail v

-- | Fan-out / fan-in workload mirroring the production
-- | `RIO.fork x16 + awaitAll` shape. Forks `n` children, each of
-- | which does a trivial pure computation, then joins all of them
-- | in input order.
fanOutFanInOp :: forall r e. Array Int -> Op r e (Array Int)
fanOutFanInOp arr =
  opBind (traverse (\n -> opForkFiber (opPure (n + 1))) arr) \fibs ->
    traverse opJoinFiber fibs

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
-- | Implementation delegates to `Aff.bracket`, with `runOp`
-- | running each of the three sub-`Op`s against the captured
-- | env. Typed failures are threaded through the bracket via the
-- | `Either (Variant e) _` carrier.
opBracket
  :: forall r e a b
   . Op r e a
  -> (a -> Op r e Unit)
  -> (a -> Op r e b)
  -> Op r e b
opBracket acquire release use = opBind opAsk \env ->
  let
    runOne :: forall x. Op r e x -> Aff (Either (Variant e) x)
    runOne = runOp env

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
    opBind (opAsync body) case _ of
      Right b -> opPure b
      Left v -> opFail v

-- | Sanity workload for `opBracket`: acquire a Ref counter,
-- | bump it inside `use`, and bump it again inside `release`.
-- | A correct interpreter returns `release` having run after
-- | `use`, so the final Ref value is 2 and the workload's result
-- | is 1 (the value `use` saw after its own bump).
bracketSanityOp :: forall r e. Ref Int -> Op r e Int
bracketSanityOp counter =
  opBracket
    (opLiftEffect (Ref.read counter))
    (\_ -> opLiftEffect (Ref.modify_ (_ + 1) counter))
    ( \_ -> do
        _ <- opLiftEffect (Ref.modify_ (_ + 1) counter)
        opLiftEffect (Ref.read counter)
    )

-- | Bench workload: acquire/use/release loop. Each iteration is
-- | a complete bracket round-trip with a trivial acquire and a
-- | trivial release. Measures the per-bracket interpreter cost
-- | (two extra `runOp` invocations plus an `Aff.bracket`).
bracketLoopOp :: forall r e. Int -> Op r e Int
bracketLoopOp n = go 0 n
  where
  go :: Int -> Int -> Op r e Int
  go acc 0 = opPure acc
  go acc k =
    opBind
      ( opBracket
          (opPure (acc + 1))
          (\_ -> opPure unit)
          (\x -> opPure x)
      )
      \x -> go x (k - 1)

-- | A loop that reads, increments, and writes an `Effect.Ref` once
-- | per iteration, all lifted through `opLiftEffect`. There is
-- | no new interpreter machinery here - this just shows that
-- | mutable state composes naturally through the `SYNC` tag we
-- | already have. Mirrors a `Ref`-based loop one would write
-- | against production `RIO` with `liftEffect` + `Effect.Ref`.
refCounterLoopOp :: forall r e. Ref Int -> Int -> Op r e Int
refCounterLoopOp ref n = go 0 n
  where
  go :: Int -> Int -> Op r e Int
  go acc 0 = opPure acc
  go _ k =
    opBind (opLiftEffect (Ref.modify (_ + 1) ref)) \x ->
      go x (k - 1)
