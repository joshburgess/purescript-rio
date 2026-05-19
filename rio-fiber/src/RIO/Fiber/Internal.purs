-- | Internal definition of the fiber-backed `RIO` newtype.
-- |
-- | This is the rio-fiber prototype: a custom fiber runtime rather than
-- | `Effect.Aff`. The `Op` instruction tree is foreign-imported and built
-- | by FFI smart constructors; the step interpreter in `Internal.js`
-- | reads the tag and dispatches in a tight loop. The MVP supports pure
-- | values, synchronous effects, bind, typed failure, catch-all, and
-- | environment reads. Async, fork/join/interrupt, and preemption land
-- | in later phases.
module RIO.Fiber.Internal
  ( RIO(..)
  , Op
  , runFiber
  , opPure
  , opLiftEffect
  , opBind
  , opFail
  , opCatchAll
  , opAsk
  , opLocal
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (Error)

-- | Opaque instruction tree built by FFI factories and stepped by the
-- | interpreter. Users never construct or pattern-match on it.
foreign import data Op :: Row Type -> Row Type -> Type -> Type

-- | The user-facing computation. A description of effects that, when run
-- | against a record of services in row `r`, either produces an `a` or
-- | a typed failure in `Variant e` (or a defect through `Error`).
newtype RIO :: Row Type -> Row Type -> Type -> Type
newtype RIO r e a = RIO (Op r e a)

instance functorRIO :: Functor (RIO r e) where
  map f (RIO m) = RIO (opBind m (\a -> opPure (f a)))

instance applyRIO :: Apply (RIO r e) where
  apply (RIO mf) (RIO ma) =
    RIO (opBind mf (\f -> opBind ma (\a -> opPure (f a))))

instance applicativeRIO :: Applicative (RIO r e) where
  pure = RIO <<< opPure

instance bindRIO :: Bind (RIO r e) where
  bind (RIO m) k = RIO (opBind m (\a -> case k a of RIO m' -> m'))

instance monadRIO :: Monad (RIO r e)

foreign import opPure :: forall r e a. a -> Op r e a
foreign import opLiftEffect :: forall r e a. Effect a -> Op r e a
foreign import opBind
  :: forall r e a b. Op r e a -> (a -> Op r e b) -> Op r e b
foreign import opAsk :: forall r e. Op r e (Record r)
foreign import opFail :: forall r e a. Variant e -> Op r e a
foreign import opCatchAll
  :: forall r e e' a
   . (Variant e -> Op r e' a)
  -> Op r e a
  -> Op r e' a
foreign import opLocal
  :: forall r r' e a. (Record r -> Record r') -> Op r' e a -> Op r e a

-- | Tagged result handed back by the JS interpreter. One of:
-- |   { ok :: a } | { fail :: Variant e } | { die :: Error }
foreign import data FiberResult :: Row Type -> Type -> Type

foreign import _runFiber
  :: forall r e a. Op r e a -> Record r -> Effect (FiberResult e a)

foreign import _resultIsOk :: forall e a. FiberResult e a -> Boolean
foreign import _resultIsFail :: forall e a. FiberResult e a -> Boolean
foreign import _resultOk :: forall e a. FiberResult e a -> a
foreign import _resultFail :: forall e a. FiberResult e a -> Variant e
foreign import _resultDie :: forall e a. FiberResult e a -> Error

-- | Run a fully-discharged RIO program against a record environment.
-- | Returns the outcome as either a typed failure `Variant e`, a defect
-- | `Error`, or a success `a`. Synchronous for now; once Async lands,
-- | this will dispatch into the fiber runtime and return through a
-- | callback / promise / Aff bridge.
runFiber
  :: forall r e a
   . RIO r e a
  -> Record r
  -> Effect (Either (Either Error (Variant e)) a)
runFiber (RIO op) env = do
  r <- _runFiber op env
  pure
    if _resultIsOk r then Right (_resultOk r)
    else if _resultIsFail r then Left (Right (_resultFail r))
    else Left (Left (_resultDie r))
