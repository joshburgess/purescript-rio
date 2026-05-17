-- | Internal definition of the `RIO` newtype.
-- |
-- | Phase 5 replaces the original `Record r -> Aff a` closure encoding with
-- | an instruction-list (ADT) encoding interpreted by a hand-rolled
-- | step / resume machine. The newtype now wraps an opaque `Instr` whose
-- | constructors are tagged JS objects (see `Internal.js`). The hot path
-- | for synchronous binds runs entirely inside the FFI loop, paying no
-- | per-`bind` `Aff` cost; only true async work goes through the driver
-- | loop here.
-- |
-- | Two compatibility helpers preserve the throw-based contract that the
-- | rest of the library was written against:
-- |
-- |   * `unsafeUnRIO :: RIO r e a -> Record r -> Aff a` runs the
-- |     interpreter and rethrows typed failures through `Aff`'s exception
-- |     channel as tagged exceptions (same as before).
-- |   * `unRIO :: RIO r e a -> Record r -> Aff (Either (Variant e) a)`
-- |     reifies typed failures back to `Left`. Defects keep propagating.
-- |
-- | `mkRIO :: (Record r -> Aff a) -> RIO r e a` is the new bridge for
-- | call sites that historically wrote `RIO \r -> ...`. It wraps the
-- | closure as an `Instr` LIFT node so the interpreter can drive it.
module RIO.Internal
  ( RIO(..)
  , Instr
  , mkRIO
  , mkEffectRIO
  , unRIO
  , unsafeUnRIO
  , rioFail
  , matchTypedFailure
  , mkTypedFailureError
  , instrPure
  , instrBind
  , instrLiftEffect
  , instrLiftAff
  , instrLift
  , instrSyncLift
  , instrAsk
  , instrFail
  , instrCatchTag
  , instrCatchAll
  , instrLocal
  , runInstr
  , InstrCounts
  , dumpInstrCounts
  , resetInstrCounts
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)

-- | Opaque ADT-encoded computation. Each constructor is a tagged JS
-- | object built by the FFI factories; the interpreter reads the tag
-- | and dispatches in a tight while-loop. Users never see `Instr`
-- | directly; library combinators build it through the factory
-- | functions and the `RIO` newtype hides the wrapper.
foreign import data Instr :: Row Type -> Row Type -> Type -> Type

-- | Opaque mutable interpreter state. One per `runInstr` call.
foreign import data InstrState :: Row Type -> Row Type -> Type -> Type

-- | Opaque foreign value used to shuttle the result of an async
-- | suspension from the driver back into the interpreter without
-- | requiring a type witness at the PureScript boundary. The bind
-- | continuation that consumes it already has the right type baked
-- | in by the user's program.
foreign import data ForeignValue :: Type

foreign import instrPure :: forall r e a. a -> Instr r e a
foreign import instrLiftEffect :: forall r e a. Effect a -> Instr r e a
foreign import instrLiftAff :: forall r e a. Aff a -> Instr r e a
foreign import instrBind
  :: forall r e a b. Instr r e a -> (a -> Instr r e b) -> Instr r e b
foreign import instrAsk :: forall r e. Instr r e (Record r)
foreign import instrFail :: forall r e a. Variant e -> Instr r e a
foreign import instrCatchTag
  :: forall r e e' a x
   . String
  -> (x -> Instr r e' a)
  -> Instr r e a
  -> Instr r e' a

-- | Catch every typed failure raised inside `m` with a single handler
-- | that receives the full `Variant e`. Mirrors the surface `catchAll`
-- | combinator; defects (untagged Aff exceptions) keep propagating.
foreign import instrCatchAll
  :: forall r e e' a
   . (Variant e -> Instr r e' a)
  -> Instr r e a
  -> Instr r e' a
foreign import instrLocal
  :: forall r r' e a
   . (Record r -> Record r')
  -> Instr r' e a
  -> Instr r e a

-- | LIFT: run a `Record r -> Aff a` closure under the interpreter.
-- | The driver suspends, applies `f` to the current env, runs the
-- | resulting `Aff`, and resumes with its value. Typed failures
-- | (tagged exceptions on the `Aff` channel) are reflected back as
-- | FAIL nodes; defects propagate.
foreign import instrLift
  :: forall r e a. (Record r -> Aff a) -> Instr r e a

-- | SYNC_LIFT: env-aware `Record r -> Effect a` bridge. Unlike
-- | LIFT, the interpreter runs the resulting `Effect` synchronously
-- | inside the inner loop, never suspending. Use when the work is
-- | genuinely synchronous and just needs the env.
foreign import instrSyncLift
  :: forall r e a. (Record r -> Effect a) -> Instr r e a

foreign import _initInstrState
  :: forall r e a. Record r -> Instr r e a -> Effect (InstrState r e a)

foreign import _stepInstr
  :: forall r e a. InstrState r e a -> Effect Unit

foreign import _resumeAndStep
  :: forall r e a. InstrState r e a -> ForeignValue -> Effect Unit

foreign import _failAndStep
  :: forall r e a. InstrState r e a -> Variant e -> Effect Unit

foreign import _isDone :: forall r e a. InstrState r e a -> Boolean
foreign import _isRightFinal :: forall r e a. InstrState r e a -> Boolean
foreign import _finalRight :: forall r e a. InstrState r e a -> a
foreign import _finalLeft :: forall r e a. InstrState r e a -> Variant e
foreign import _pendingAff :: forall r e a. InstrState r e a -> Aff ForeignValue

foreign import _isPureInstr :: forall r e a. Instr r e a -> Boolean
foreign import _purePayload :: forall r e a. Instr r e a -> a
foreign import _isSyncInstr :: forall r e a. Instr r e a -> Boolean
foreign import _syncEff :: forall r e a. Instr r e a -> Effect a

-- | `RIO r e a` is a computation that, given an environment of services
-- | in row `r`, performs `Aff` work that either fails with a tagged
-- | error in row `e` or produces a value of type `a`.
-- |
-- | Internally `RIO` wraps an `Instr r e a` and is driven by the
-- | step / resume interpreter in `Internal.js`. Typed failures are
-- | thrown as tagged `Aff` exceptions only when we cross back into
-- | `Aff` (at `unsafeUnRIO` / `runRIO` boundaries); inside the
-- | instruction list they live as FAIL nodes and unwind cheaply via
-- | the interpreter's catch stack.
newtype RIO :: Row Type -> Row Type -> Type -> Type
newtype RIO r e a = RIO (Instr r e a)

-- | Bridge for legacy call sites that wrote `RIO \r -> ...affWork r`.
-- | Wraps the closure as a LIFT instruction; the interpreter applies
-- | it to the current env and suspends on the resulting `Aff`.
mkRIO :: forall r e a. (Record r -> Aff a) -> RIO r e a
mkRIO f = RIO (instrLift f)

-- | Bridge for call sites that need the env but do only synchronous
-- | Effect work. Wraps the closure as a SYNC_LIFT instruction; the
-- | interpreter runs the resulting Effect inside its inner loop and
-- | never suspends through Aff. Strictly faster than `mkRIO \r ->
-- | liftEffect ...` on hot paths.
mkEffectRIO :: forall r e a. (Record r -> Effect a) -> RIO r e a
mkEffectRIO f = RIO (instrSyncLift f)

-- | Run an `Instr` against an environment record. The interpreter
-- | dispatches synchronously until it either completes or suspends
-- | on an async node; the driver here runs the pending `Aff`,
-- | threads its result back into the interpreter, and re-enters the
-- | loop. Typed-failure `Aff` exceptions are reflected into FAIL
-- | nodes so the interpreter's catch frames can handle them.
runInstr
  :: forall r e a
   . Record r
  -> Instr r e a
  -> Aff (Either (Variant e) a)
runInstr env instr = do
  state <- liftEffect do
    s <- _initInstrState env instr
    _stepInstr s
    pure s
  if _isDone state then
    pure
      if _isRightFinal state then Right (_finalRight state)
      else Left (_finalLeft state)
  else
    tailRecM step state
  where
  step :: InstrState r e a -> Aff (Step (InstrState r e a) (Either (Variant e) a))
  step state =
    if _isDone state then
      pure
        ( Done
            if _isRightFinal state then Right (_finalRight state)
            else Left (_finalLeft state)
        )
    else do
      attempted <- attempt (_pendingAff state)
      case attempted of
        Right v -> do
          liftEffect (_resumeAndStep state v)
          pure (Loop state)
        Left err -> case matchTypedFailure err of
          Just variant -> do
            liftEffect (_failAndStep state variant)
            pure (Loop state)
          Nothing -> throwError err

-- | Raw projection of the newtype. Result type does NOT mention
-- | `Either` because typed failures stay on the `Aff` throw track.
-- | Use this for internal composition; reach for `unRIO` when you
-- | need to reify the failure shape.
unsafeUnRIO :: forall r e a. RIO r e a -> Record r -> Aff a
unsafeUnRIO (RIO instr) r =
  if _isPureInstr instr then
    pure (_purePayload instr)
  else if _isSyncInstr instr then
    liftEffect (_syncEff instr)
  else do
    result <- runInstr r instr
    case result of
      Right a -> pure a
      Left v -> rioFail v

-- | Public boundary: peel the newtype and reify any tagged
-- | typed-failure exception back to `Left (Variant e)`. Defects
-- | (untagged `Aff` exceptions) keep propagating.
unRIO :: forall r e a. RIO r e a -> Record r -> Aff (Either (Variant e) a)
unRIO (RIO instr) r =
  if _isPureInstr instr then
    pure (Right (_purePayload instr))
  else if _isSyncInstr instr then
    liftEffect (Right <$> _syncEff instr)
  else
    runInstr r instr

-- | Raise a typed failure through `Aff`'s error channel. The thrown
-- | exception carries the `Variant` payload on a marker property so
-- | `matchTypedFailure` can recover it; defects (any other `Aff`
-- | exception) are passed through unchanged.
rioFail :: forall e a. Variant e -> Aff a
rioFail v = throwError (_mkTypedFailure v)

-- | Match a tagged typed-failure exception. Returns `Just v` if the
-- | error object carries the typed-failure marker; otherwise
-- | returns `Nothing` (the error is a defect and should be
-- | re-thrown).
matchTypedFailure :: forall e. Error -> Maybe (Variant e)
matchTypedFailure err = _matchTypedFailure Nothing Just err

-- | Construct the tagged exception value used to encode a typed
-- | failure on `Aff`'s error channel. Use this when interoperating
-- | with `Aff` primitives that take an `Either Error a` (such as
-- | `makeAff`'s `resume`) and you need to deliver a typed failure
-- | rather than a defect.
mkTypedFailureError :: forall e. Variant e -> Error
mkTypedFailureError = _mkTypedFailure

foreign import _mkTypedFailure :: forall e. Variant e -> Error
foreign import _matchTypedFailure
  :: forall e r
   . r
  -> (Variant e -> r)
  -> Error
  -> r

-- | Per-instruction-tag dispatch counts. Populated only when the
-- | interpreter is built with `RIO_INSTR_PROFILE=1` in the
-- | environment at module load time; otherwise all fields read zero.
type InstrCounts =
  { "PURE" :: Int
  , "SYNC" :: Int
  , "BIND" :: Int
  , "ASK" :: Int
  , "FAIL" :: Int
  , "CATCH" :: Int
  , "LOCAL" :: Int
  , "ASYNC" :: Int
  , "LIFT" :: Int
  , "SYNC_LIFT" :: Int
  , "CATCH_ALL" :: Int
  }

foreign import _dumpInstrCounts :: Effect InstrCounts
foreign import _resetInstrCounts :: Effect Unit

-- | Snapshot the per-tag dispatch counters. Reads zeros unless the
-- | profiling build of the interpreter is in effect.
dumpInstrCounts :: Effect InstrCounts
dumpInstrCounts = _dumpInstrCounts

-- | Zero the per-tag dispatch counters in place.
resetInstrCounts :: Effect Unit
resetInstrCounts = _resetInstrCounts

instance functorRIO :: Functor (RIO r e) where
  map f (RIO m) = RIO (instrBind m \a -> instrPure (f a))

instance applyRIO :: Apply (RIO r e) where
  apply (RIO f) (RIO x) =
    RIO (instrBind f \fn -> instrBind x \v -> instrPure (fn v))

instance applicativeRIO :: Applicative (RIO r e) where
  pure a = RIO (instrPure a)

-- | `bind` is a single BIND instruction in the interpreter; the
-- | continuation runs without leaving the FFI loop unless it hits
-- | an async / lift node. The `case ... of RIO i -> i` destructure
-- | is the same trick the closure encoding used: the newtype unwrap
-- | compiles to a no-op.
instance bindRIO :: Bind (RIO r e) where
  bind (RIO m) k = RIO (instrBind m \a -> case k a of RIO i -> i)

instance monadRIO :: Monad (RIO r e)

instance monadEffectRIO :: MonadEffect (RIO r e) where
  liftEffect eff = RIO (instrLiftEffect eff)

instance monadAffRIO :: MonadAff (RIO r e) where
  liftAff aff = RIO (instrLiftAff aff)
