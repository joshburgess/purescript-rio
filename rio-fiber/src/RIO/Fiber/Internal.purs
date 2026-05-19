-- | Internal definition of the fiber-backed `RIO` newtype.
-- |
-- | This is the rio-fiber prototype: a custom fiber runtime rather than
-- | `Effect.Aff`. The `Op` instruction tree is foreign-imported and built
-- | by FFI smart constructors; the step interpreter in `Internal.js`
-- | reads the tag and dispatches in a tight loop. The MVP supports pure
-- | values, synchronous effects, bind, typed failure, catch-all,
-- | environment reads, async, fork / join / interrupt. Tick-budgeted
-- | preemption and richer Cause tracking land in later phases.
module RIO.Fiber.Internal
  ( RIO(..)
  , Op
  , Fiber
  , Outcome(..)
  , runFiber
  , runFiberSync
  , startFiber
  , observeFiber
  , interruptFiber
  , fiberIsDone
  , opPure
  , opLiftEffect
  , opBind
  , opFail
  , opCatchAll
  , opAsk
  , opLocal
  , opAsync
  , opFork
  , opJoin
  , opInterrupt
  , opEnsuring
  , opUninterruptible
  , opRace
  , opParTraverse
  , opPeel
  , FiberResult
  , peelToCauseEither
  , Scope
  , _newScope
  , _addFinalizerEff
  , _closeScope
  , FiberRef
  , _newFiberRef
  , opGetFiberRef
  , opSetFiberRef
  , opModifyFiberRef
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (Error)
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause

-- | Opaque instruction tree built by FFI factories and stepped by the
-- | interpreter. Users never construct or pattern-match on it.
foreign import data Op :: Row Type -> Row Type -> Type -> Type

-- | A live fiber: a handle that lets callers observe completion or
-- | request interruption. Phantom rows track the error / success types
-- | the fiber will produce.
foreign import data Fiber :: Row Type -> Type -> Type

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

-- | Async primitive. The register function receives two callbacks
-- | (success and typed failure) and returns a canceller. Calling the
-- | canceller is best-effort: the resume callbacks remain single-shot.
foreign import opAsync
  :: forall r e a
   . ( (a -> Effect Unit)
       -> (Variant e -> Effect Unit)
       -> Effect (Effect Unit)
     )
  -> Op r e a

foreign import opFork :: forall r e a. Op r e a -> Op r e (Fiber e a)
foreign import opJoin :: forall r e a. Fiber e a -> Op r e a
foreign import opInterrupt :: forall r e a. Fiber e a -> Op r e Unit

-- | Attach a finalizer that runs after the action regardless of how
-- | it terminates: success, typed failure, defect, or interrupt. The
-- | finalizer runs inside an uninterruptible region.
foreign import opEnsuring
  :: forall r e a. Op r e Unit -> Op r e a -> Op r e a

-- | Run the wrapped op inside an uninterruptible mask. Interrupts
-- | are deferred (the flag remains set; the loop just doesn't act
-- | on it) until the mask is released.
foreign import opUninterruptible :: forall r e a. Op r e a -> Op r e a

-- | Run two ops concurrently; resume with the first non-interrupted
-- | outcome and interrupt the loser. If both are interrupted the
-- | parent inherits the interrupt.
foreign import opRace :: forall r e a. Op r e a -> Op r e a -> Op r e a

-- | Fork one fiber per item and await all results in order. Fails
-- | fast: the first non-success outcome interrupts the siblings and
-- | resumes the parent with that outcome.
foreign import opParTraverse
  :: forall r e a b. (a -> Op r e b) -> Array a -> Op r e (Array b)

-- | Run the wrapped op and capture its outcome (success, typed
-- | failure, defect, or interrupt) as a `FiberResult`. The outer
-- | error row is independent: the caller may discharge it or thread
-- | a different one.
foreign import opPeel :: forall r e e' a. Op r e a -> Op r e' (FiberResult e a)

-- | A scope: a holder for `Effect Unit` finalizers that all fire
-- | in LIFO order when the scope is closed. The MVP finalizer
-- | shape is fire-and-forget; async cleanup that needs to be
-- | awaited has to bridge that itself.
foreign import data Scope :: Type

foreign import _newScope :: Effect Scope
foreign import _addFinalizerEff :: Scope -> Effect Unit -> Effect Unit
foreign import _closeScope :: Scope -> Effect Unit

-- | A per-fiber mutable cell. Each fiber owns an isolated copy of
-- | every `FiberRef` value; forking inherits the parent's value at
-- | the moment of fork, and subsequent writes in either fiber are
-- | not observed by the other. The phantom `a` is the cell's
-- | element type.
foreign import data FiberRef :: Type -> Type

foreign import _newFiberRef :: forall a. a -> Effect (FiberRef a)
foreign import opGetFiberRef :: forall r e a. FiberRef a -> Op r e a
foreign import opSetFiberRef :: forall r e a. FiberRef a -> a -> Op r e Unit
foreign import opModifyFiberRef
  :: forall r e a. FiberRef a -> (a -> a) -> Op r e Unit

-- | The full outcome of running a fiber. Includes interrupt as a
-- | dedicated case; defects come through `Die`.
data Outcome e a
  = Success a
  | Fail (Variant e)
  | Die Error
  | Interrupted

derive instance functorOutcome :: Functor (Outcome e)

-- | Tagged result handed back by the JS interpreter.
foreign import data FiberResult :: Row Type -> Type -> Type

foreign import _startFiber
  :: forall r e a. Op r e a -> Record r -> Effect (Fiber e a)

foreign import _fiberIsDone :: forall e a. Fiber e a -> Boolean
foreign import _fiberResult :: forall e a. Fiber e a -> FiberResult e a
foreign import _fiberObserve
  :: forall e a
   . Fiber e a
  -> (FiberResult e a -> Effect Unit)
  -> Effect Unit
foreign import _fiberInterrupt :: forall e a. Fiber e a -> Effect Unit

foreign import _resultIsOk :: forall e a. FiberResult e a -> Boolean
foreign import _resultIsFail :: forall e a. FiberResult e a -> Boolean
foreign import _resultIsInterrupted :: forall e a. FiberResult e a -> Boolean
foreign import _resultOk :: forall e a. FiberResult e a -> a
foreign import _resultFail :: forall e a. FiberResult e a -> Variant e
foreign import _resultDie :: forall e a. FiberResult e a -> Error

resultToOutcome :: forall e a. FiberResult e a -> Outcome e a
resultToOutcome r
  | _resultIsOk r = Success (_resultOk r)
  | _resultIsFail r = Fail (_resultFail r)
  | _resultIsInterrupted r = Interrupted
  | otherwise = Die (_resultDie r)

-- | Convert a `FiberResult` (the JS-tagged outcome carried by `peel`)
-- | into an `Either (Cause e) a`. `Right` carries the success value;
-- | `Left` carries the leaf cause (a single `Fail` / `Die` /
-- | `Interrupt`). Composed causes from finalizer-then-action or
-- | parallel-both will land here once the interpreter threads Cause
-- | through every mode.
peelToCauseEither :: forall e a. FiberResult e a -> Either (Cause e) a
peelToCauseEither r
  | _resultIsOk r = Right (_resultOk r)
  | _resultIsFail r = Left (Cause.fail (_resultFail r))
  | _resultIsInterrupted r = Left Cause.interrupt
  | otherwise = Left (Cause.die (_resultDie r))

-- | Start a fiber executing the given program against `env`. Returns
-- | the fiber handle synchronously; the fiber may already have
-- | completed (if its body was fully synchronous).
startFiber :: forall r e a. RIO r e a -> Record r -> Effect (Fiber e a)
startFiber (RIO op) = _startFiber op

-- | Has this fiber completed?
fiberIsDone :: forall e a. Fiber e a -> Boolean
fiberIsDone = _fiberIsDone

-- | Install a one-shot observer: the callback fires when the fiber
-- | completes, with the full outcome.
observeFiber
  :: forall e a
   . Fiber e a
  -> (Outcome e a -> Effect Unit)
  -> Effect Unit
observeFiber f cb = _fiberObserve f (cb <<< resultToOutcome)

-- | Request interruption. Best-effort: the fiber will complete with
-- | the `Interrupted` outcome at its next safe point.
interruptFiber :: forall e a. Fiber e a -> Effect Unit
interruptFiber = _fiberInterrupt

-- | Run a fully-discharged RIO program against a record environment.
-- | The callback fires when the program completes. The returned
-- | `Effect Unit` requests interruption of the running fiber.
runFiber
  :: forall r e a
   . RIO r e a
  -> Record r
  -> (Outcome e a -> Effect Unit)
  -> Effect (Effect Unit)
runFiber rio env cb = do
  f <- startFiber rio env
  observeFiber f cb
  pure (interruptFiber f)

-- | Synchronous runner. Starts the fiber and inspects its status. If
-- | the program completed without suspending it returns `Just outcome`;
-- | otherwise `Nothing` (caller should use the callback-style runner
-- | or an Aff bridge). The fiber keeps running in the background even
-- | when `Nothing` is returned.
runFiberSync
  :: forall r e a
   . RIO r e a
  -> Record r
  -> Effect (Maybe (Outcome e a))
runFiberSync rio env = do
  f <- startFiber rio env
  pure
    if _fiberIsDone f then Just (resultToOutcome (_fiberResult f))
    else Nothing
