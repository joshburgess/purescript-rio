-- | Spike: instruction-list-encoded mini-RIO with a hand-rolled
-- | synchronous interpreter, for a like-for-like perf comparison
-- | against the production closure-based `RIO`.
-- |
-- | This module is intentionally minimal: just enough primitives
-- | (`pure`, `bind`, `ask`, `liftEffect`, `fail`, `catchTag`) to
-- | run the same workloads the `VsAff` head-to-head bench uses,
-- | with no fork / async / bracket machinery. The interpreter
-- | runs entirely synchronously inside an `Effect`, and the
-- | public entry point lifts the terminal `Either` into `Aff`
-- | so the harness can time it the same way it times production
-- | `RIO`.
-- |
-- | Phase 1 of the rewrite plan adds the synchronous failure
-- | primitives (`catchTag`) on top of the original spike. See
-- | `INSTR_LIST_REWRITE_PLAN.md` at the repo root for the full
-- | roadmap and exit criteria.
module Benchmarks.Instr
  ( Instr
  , runInstr
  , instrPure
  , instrFlatMap
  , instrAsk
  , instrLiftEffect
  , instrFail
  , instrFailTag
  , instrCatchTag
  , instrLocal
  , bindChainInstr
  , serviceLoopInstr
  , failCatchOnceInstr
  , catchLoopInstr
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

foreign import instrPure :: forall r e a. a -> Instr r e a

foreign import instrLiftEffect :: forall r e a. Effect a -> Instr r e a

foreign import instrFlatMap
  :: forall r e a b
   . Instr r e a
  -> (a -> Instr r e b)
  -> Instr r e b

foreign import instrAsk :: forall r e. Instr r e (Record r)

foreign import instrFail :: forall r e a. Variant e -> Instr r e a

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

foreign import _runInstr
  :: forall r e a
   . (Variant e -> Either (Variant e) a)
  -> (a -> Either (Variant e) a)
  -> Record r
  -> Instr r e a
  -> Effect (Either (Variant e) a)

-- | Run an `Instr` against an environment record. The interpreter
-- | is a tight hand-rolled while loop in FFI with an explicit
-- | continuation stack and a parallel catch-frame stack; the
-- | result is lifted into `Aff` so it composes with the rest of
-- | the bench harness.
runInstr :: forall r e a. Record r -> Instr r e a -> Aff (Either (Variant e) a)
runInstr env instr = liftEffect (_runInstr Left Right env instr)

instance functorInstr :: Functor (Instr r e) where
  map f i = instrFlatMap i (\a -> instrPure (f a))

instance applyInstr :: Apply (Instr r e) where
  apply f x = instrFlatMap f \fn -> instrFlatMap x \v -> instrPure (fn v)

instance applicativeInstr :: Applicative (Instr r e) where
  pure = instrPure

instance bindInstr :: Bind (Instr r e) where
  bind = instrFlatMap

instance monadInstr :: Monad (Instr r e)

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
