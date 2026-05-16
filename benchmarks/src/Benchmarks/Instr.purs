-- | Spike: instruction-list-encoded mini-RIO with a hand-rolled
-- | synchronous interpreter, for a like-for-like perf comparison
-- | against the production closure-based `RIO`.
-- |
-- | This module is intentionally minimal: just enough primitives
-- | (`pure`, `bind`, `ask`, `liftEffect`, `fail`, `catchTag`) to run
-- | the same workloads the `VsAff` head-to-head bench uses, with no
-- | fork / async / bracket machinery. The interpreter runs entirely
-- | synchronously inside an `Effect`, and the public entry point
-- | lifts the terminal `Either` into `Aff` so the harness can time
-- | it the same way it times production `RIO`.
-- |
-- | The point of the spike is to answer one question: does an
-- | instruction-list encoding with a custom interpreter give a
-- | meaningful perf win over the current `Record r -> Aff a`
-- | encoding on the workloads where bind cost dominates? If yes, a
-- | full migration may be worth the cost. If not, we abandon and
-- | stick with the current encoding.
module Benchmarks.Instr
  ( Instr
  , runInstr
  , instrPure
  , instrFlatMap
  , instrAsk
  , instrLiftEffect
  , instrFail
  , bindChainInstr
  , serviceLoopInstr
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Type.Proxy (Proxy(..))

-- | Opaque ADT-encoded computation. Each constructor is a tagged
-- | JS object built by the FFI factories; the interpreter reads the
-- | tag and dispatches.
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

foreign import _runInstr
  :: forall r e a
   . (Variant e -> Either (Variant e) a)
  -> (a -> Either (Variant e) a)
  -> Record r
  -> Instr r e a
  -> Effect (Either (Variant e) a)

-- | Run an `Instr` against an environment record. The interpreter
-- | is a tight hand-rolled while loop in FFI with an explicit
-- | continuation stack; the result is lifted into `Aff` so it
-- | composes with the rest of the bench harness.
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

-- | The same bind-chain workload `bindChain` runs against `RIO`,
-- | rewritten against `Instr`. The body never touches the env.
bindChainInstr :: forall r e. Int -> Instr r e Int
bindChainInstr n = go 0 n
  where
  go :: Int -> Int -> Instr r e Int
  go acc 0 = instrPure acc
  go acc k = instrFlatMap (instrPure (acc + 1)) \x -> go x (k - 1)

-- | A service-loop workload that exercises the `Ask` instruction
-- | per iteration. Mirrors `serviceLoop` from the production bench.
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
