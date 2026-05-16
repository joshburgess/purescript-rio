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
  , instrFlatMap
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
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
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

foreign import instrFlatMap
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
  drive state
  where
  drive :: InstrState r e a -> Aff (Either (Variant e) a)
  drive state = do
    liftEffect (_stepInstr state)
    if _isDone state then
      pure
        if _isRightFinal state then Right (_finalRight state)
        else Left (_finalLeft state)
    else do
      v <- _pendingAff state
      liftEffect (_resumeInstr state v)
      drive state

instance functorInstr :: Functor (Instr r e) where
  map f i = instrFlatMap i (\a -> instrPure (f a))

instance applyInstr :: Apply (Instr r e) where
  apply f x = instrFlatMap f \fn -> instrFlatMap x \v -> instrPure (fn v)

instance applicativeInstr :: Applicative (Instr r e) where
  pure = instrPure

instance bindInstr :: Bind (Instr r e) where
  bind = instrFlatMap

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
  go acc k = instrFlatMap (instrPure (acc + 1)) \x -> go x (k - 1)

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
  go acc k = instrFlatMap instrAsk \env ->
    let _ = Proxy :: Proxy "svc"
    in go (env.svc.lookup acc) (k - 1)

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
  instrFlatMap (instrAsync (pure 42)) \n -> instrPure (n + 1)

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
  go acc k = instrFlatMap (instrAsync (pure (acc + 1))) \x -> go x (k - 1)

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
    instrFlatMap (instrPure (acc + 1)) \a1 ->
      instrFlatMap (instrPure (a1 + 1)) \a2 ->
        instrFlatMap (instrPure (a2 + 1)) \a3 ->
          instrFlatMap (instrPure (a3 + 1)) \a4 ->
            instrFlatMap (instrPure (a4 + 1)) \a5 ->
              instrFlatMap (instrPure (a5 + 1)) \a6 ->
                instrFlatMap (instrPure (a6 + 1)) \a7 ->
                  instrFlatMap (instrPure (a7 + 1)) \a8 ->
                    instrFlatMap (instrPure (a8 + 1)) \a9 ->
                      instrFlatMap (instrAsync (pure (a9 + 1))) \a10 ->
                        go a10 (k - 1)
