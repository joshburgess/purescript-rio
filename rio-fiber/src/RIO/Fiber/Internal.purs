-- | Internal definition of the fiber-backed `RIO` newtype.
-- |
-- | This is the rio-fiber prototype: a custom fiber runtime rather than
-- | `Effect.Aff`. The `Op` instruction tree is foreign-imported and built
-- | by FFI smart constructors; the step interpreter in `Internal.js`
-- | reads the tag and dispatches in a tight loop. The runtime supports
-- | pure values, synchronous effects, bind, typed failure, catch-all,
-- | environment reads, async, fork / join / interrupt, ensuring /
-- | uninterruptible, race / parTraverse, FiberRef, peel, and structured
-- | Cause composition. The step loop is tick-budgeted: each fiber may
-- | execute up to `TICK_BUDGET` ops before yielding to the microtask
-- | queue, so a long synchronous chain cannot monopolise the event loop.
module RIO.Fiber.Internal
  ( RIO(..)
  , Op
  , Fiber
  , Outcome(..)
  , runFiber
  , runFiberSync
  , _runFiberSyncOrThrow
  , _runFiberSyncEither
  , startFiber
  , observeFiber
  , interruptFiber
  , fiberIsDone
  , fiberOutcome
  , opPure
  , opLiftEffect
  , opBind
  , opFail
  , opCatchAll
  , opAsk
  , opCurrentFiberId
  , opCurrentFiberLabel
  , opSetCurrentFiberLabel
  , opYieldNow
  , opCheckInterruptible
  , NullableLabel
  , nullableLabelToMaybe
  , _fiberId
  , _fiberLabel
  , _fiberSetLabel
  , _fiberStatusCode
  , opLocal
  , opAsync
  , opFork
  , opForkInline
  , opForkAll
  , opForkAllInline
  , opJoin
  , opJoinAll
  , opInterrupt
  , opEnsuring
  , opUninterruptible
  , opMaskWithRestore
  , opRace
  , opRaceAll
  , opParTraverse
  , opForEach
  , opMap
  , opApply
  , opPeel
  , FiberResult
  , peelToCauseEither
  , Scope
  , _newScope
  , _addFinalizerEff
  , _closeScope
  , _closeScopeExit
  , _scopePendingCause
  , _scopeAddJoinable
  , _scopeJoinables
  , _scopeClearJoinables
  , NullableCause
  , maybeCauseToNullable
  , nullableCauseToMaybe
  , FiberRef
  , _newFiberRef
  , opGetFiberRef
  , opSetFiberRef
  , opModifyFiberRef
  , opFailCause
  , JSCause
  , causeToJS
  , _registerSupervisor
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Exception (Error)
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.FiberId as FiberId
import Unsafe.Coerce (unsafeCoerce)

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
  -- `opMap` is a dedicated MAP op rather than `bind m (\a -> pure (f a))`.
  -- That saves a BIND + PURE + closure allocation per `map`, which adds
  -- up under `traverse` (the default Traversable instance for Array
  -- builds a balanced tree of `map` / `apply` over the bind machinery).
  --
  -- The unsafeCoerce makes the instance dictionary's `map` field point
  -- at the foreign `opMap` directly. RIO is a newtype over Op, so the
  -- runtime representations match; we skip the wrapper closure that the
  -- `map f (RIO m) = RIO (opMap f m)` form would compile to, which
  -- visibly tightens map-heavy hot paths (Functor laws still hold via
  -- opMap's own properties).
  map = unsafeCoerce opMap

instance applyRIO :: Apply (RIO r e) where
  -- Same reasoning as `map`: opApply emits a dedicated APPLY op whose
  -- interpreter handles the two-stage evaluation in K_APPLY / K_APPLY2
  -- frames without going through bind. unsafeCoerce strips the newtype
  -- wrapper closure for the same reason `map` above does.
  apply = unsafeCoerce opApply

instance applicativeRIO :: Applicative (RIO r e) where
  pure = RIO <<< opPure

instance bindRIO :: Bind (RIO r e) where
  -- RIO is a newtype over Op, so `k :: a -> RIO r e b` has the same
  -- runtime shape as `a -> Op r e b`. unsafeCoerce makes the instance
  -- dictionary's `bind` field point at `opBind` directly, skipping the
  -- newtype-unwrap closure that `bind (RIO m) k = RIO (opBind m
  -- (unsafeCoerce k))` would otherwise compile to. The bind fast paths
  -- in the interpreter chain consecutive BIND nodes without an outer-
  -- loop tick, so reducing the per-bind closure overhead matters.
  bind = unsafeCoerce opBind

instance monadRIO :: Monad (RIO r e)

foreign import opPure :: forall r e a. a -> Op r e a
foreign import opLiftEffect :: forall r e a. Effect a -> Op r e a
foreign import opBind
  :: forall r e a b. Op r e a -> (a -> Op r e b) -> Op r e b

foreign import opAsk :: forall r e. Op r e (Record r)
foreign import opCurrentFiberId :: forall r e. Op r e Int
foreign import opCurrentFiberLabel
  :: forall r e. Op r e (NullableLabel)
foreign import opSetCurrentFiberLabel
  :: forall r e. String -> Op r e Unit
foreign import opYieldNow :: forall r e. Op r e Unit
foreign import opCheckInterruptible :: forall r e. Op r e Boolean

foreign import data NullableLabel :: Type
foreign import _nullableLabelIsJust :: NullableLabel -> Boolean
foreign import _nullableLabelValue :: NullableLabel -> String

nullableLabelToMaybe :: NullableLabel -> Maybe String
nullableLabelToMaybe n =
  if _nullableLabelIsJust n then Just (_nullableLabelValue n)
  else Nothing
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

-- | Like `opFork` but drive the child synchronously up to its first
-- | suspension (or completion) before returning the handle. If the
-- | child's body is fully synchronous it completes inline and a later
-- | `opJoin` resolves without scheduling.
foreign import opForkInline :: forall r e a. Op r e a -> Op r e (Fiber e a)

foreign import opJoin :: forall r e a. Fiber e a -> Op r e a
foreign import opInterrupt :: forall r e a. Fiber e a -> Op r e Unit

-- | Specialized array fork: spawn one fiber per op and return the
-- | handles in order. Equivalent to `traverse fork ops` but bypasses
-- | the per-element bind chain that `traverse` would build.
foreign import opForkAll :: forall r e a. Array (Op r e a) -> Op r e (Array (Fiber e a))

-- | Like `opForkAll` but each child is stepped synchronously once
-- | before its handle lands in the result array. PURE / SYNC bodies
-- | complete inline (no scheduler entry); anything else gets exactly
-- | one step so its first ASYNC callback is registered before the
-- | parent makes further observable progress.
foreign import opForkAllInline :: forall r e a. Array (Op r e a) -> Op r e (Array (Fiber e a))

-- | Specialized array join: wait on a batch of pre-forked fibers and
-- | resume with their results in order. Suspends until every fiber
-- | completes (or the first non-success outcome propagates).
foreign import opJoinAll :: forall r e a. Array (Fiber e a) -> Op r e (Array a)

-- | Attach a finalizer that runs after the action regardless of how
-- | it terminates: success, typed failure, defect, or interrupt. The
-- | finalizer runs inside an uninterruptible region.
foreign import opEnsuring
  :: forall r e a. Op r e Unit -> Op r e a -> Op r e a

-- | Run the wrapped op inside an uninterruptible mask. Interrupts
-- | are deferred (the flag remains set; the loop just doesn't act
-- | on it) until the mask is released.
foreign import opUninterruptible :: forall r e a. Op r e a -> Op r e a

-- | Run the body uninterruptibly, but pass the body a "restore" wrapper
-- | that temporarily drops the mask back to its pre-block value. If the
-- | surrounding region was already masked, the restore wrapper is a
-- | no-op; otherwise it makes its argument interruptible for the
-- | duration of that argument's execution. The runtime never inspects
-- | the inner Op's type, so the wrapper is parametric in `b`. We
-- | express that with a phantom `b` here and let `Core.uninterruptibleMask`
-- | promote it to a true rank-2 type via `unsafeCoerce` on the body.
foreign import opMaskWithRestore
  :: forall r e a b
   . ((Op r e b -> Op r e b) -> Op r e a)
  -> Op r e a

-- | Run two ops concurrently; resume with the first non-interrupted
-- | outcome and interrupt the loser. If both are interrupted the
-- | parent inherits the interrupt.
foreign import opRace :: forall r e a. Op r e a -> Op r e a -> Op r e a

-- | Flat fan-out race over an array of ops. Resumes with the first
-- | success and interrupts the rest. If every branch is interrupted
-- | the parent inherits the interrupt; if every branch fails the
-- | failures are composed with `Cause.both`. An empty input array
-- | raises a defect (no defined winner). A single-element array is
-- | stepped directly into the inner op (no fan-out).
foreign import opRaceAll :: forall r e a. Array (Op r e a) -> Op r e a

-- | Fork one fiber per item and await all results in order. Fails
-- | fast: the first non-success outcome interrupts the siblings and
-- | resumes the parent with that outcome.
foreign import opParTraverse
  :: forall r e a b. (a -> Op r e b) -> Array a -> Op r e (Array b)

-- | Sequential traverse: run `fn item` for each item in order and
-- | collect the results. Bypasses the bind chain that the generic
-- | `Traversable` instance for `Array` would build.
foreign import opForEach
  :: forall r e a b. (a -> Op r e b) -> Array a -> Op r e (Array b)

-- | Dedicated MAP op backing the Functor instance. Carries the function
-- | directly rather than wrapping it in a `\a -> pure (f a)` closure.
foreign import opMap :: forall r e a b. (a -> b) -> Op r e a -> Op r e b

-- | Dedicated APPLY op backing the Apply instance. Skips the two-bind
-- | encoding that `\mf ma -> bind mf (\f -> bind ma (\a -> pure (f a)))`
-- | would otherwise allocate.
foreign import opApply
  :: forall r e a b. Op r e (a -> b) -> Op r e a -> Op r e b

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

-- | Close the scope with a known exit cause. `null` means the scope
-- | closed because the body succeeded; a non-null `JSCause` means the
-- | body failed, defected, or was interrupted. The cause is staged on
-- | the scope object so exit-aware finalizers can read it via
-- | `_scopePendingCause`.
foreign import _closeScopeExit
  :: forall e. Scope -> NullableCause e -> Effect Unit

-- | Read the scope's currently-staged close cause. Returns `null`
-- | outside the close window or when the scope closed with success.
foreign import _scopePendingCause
  :: forall e. Scope -> Effect (NullableCause e)

-- | Register a fiber with the scope so an awaiting close can suspend
-- | until it has resolved. Heterogeneous types are erased: any
-- | `Fiber e a` may be registered, and the awaiting side does not
-- | observe the inner result. Registering after the scope has closed
-- | is a no-op.
foreign import _scopeAddJoinable
  :: forall e a. Scope -> Fiber e a -> Effect Unit

-- | Snapshot the scope's currently-registered joinable list.
foreign import _scopeJoinables
  :: forall e a. Scope -> Effect (Array (Fiber e a))

-- | Drop the scope's joinable list. Used by the awaiting close path
-- | after it has consumed the snapshot, so a re-close doesn't double-
-- | await the same fibers.
foreign import _scopeClearJoinables :: Scope -> Effect Unit

-- | Phantom newtype over a possibly-null `JSCause`. The PureScript
-- | side uses `Nullable` semantics: `null` ↔ `Nothing`,
-- | non-null ↔ `Just cause`.
foreign import data NullableCause :: Row Type -> Type

foreign import _causeNullable :: forall e. Effect (NullableCause e)
foreign import _causeFromCause
  :: forall e. JSCause e -> NullableCause e
foreign import _causeNullableIsJust
  :: forall e. NullableCause e -> Boolean
foreign import _causeNullableValue
  :: forall e. NullableCause e -> JSCause e

-- | PS-side helper: convert a `Maybe (Cause e)` to its FFI nullable
-- | form. `Nothing` becomes the null cell; `Just c` is forwarded
-- | through `causeToJS`.
maybeCauseToNullable :: forall e. Maybe (Cause e) -> Effect (NullableCause e)
maybeCauseToNullable Nothing = _causeNullable
maybeCauseToNullable (Just c) = pure (_causeFromCause (causeToJS c))

-- | PS-side helper: convert an FFI nullable cause to `Maybe (Cause e)`.
nullableCauseToMaybe :: forall e. NullableCause e -> Maybe (Cause e)
nullableCauseToMaybe n
  | _causeNullableIsJust n = Just (jsToCause (_causeNullableValue n))
  | otherwise = Nothing

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

-- | Opaque JS-side representation of a `Cause` carried through the
-- | interpreter. Built by `causeToJS` and walked by `jsToCause`.
foreign import data JSCause :: Row Type -> Type

foreign import opFailCause :: forall r e a. JSCause e -> Op r e a

foreign import _causeEmpty :: forall e. JSCause e
foreign import _causeFail :: forall e. Variant e -> JSCause e
foreign import _causeDie :: forall e. Error -> JSCause e
foreign import _causeInterrupt :: forall e. Int -> JSCause e
foreign import _causeThen :: forall e. JSCause e -> JSCause e -> JSCause e
foreign import _causeBoth :: forall e. JSCause e -> JSCause e -> JSCause e

foreign import _causeTag :: forall e. JSCause e -> Int
foreign import _causeFailValue :: forall e. JSCause e -> Variant e
foreign import _causeDieValue :: forall e. JSCause e -> Error
foreign import _causeInterruptValue :: forall e. JSCause e -> Int
foreign import _causeLeft :: forall e. JSCause e -> JSCause e
foreign import _causeRight :: forall e. JSCause e -> JSCause e

-- | Register a supervisor with the runtime; returns an unregister
-- | action. Used by `RIO.Fiber.Supervisor`.
foreign import _registerSupervisor
  :: { onStart :: Int -> Effect Unit
     , onEnd :: Int -> Effect Unit
     }
  -> Effect (Effect Unit)

-- | Convert a `Cause` to its JS representation for the interpreter.
causeToJS :: forall e. Cause e -> JSCause e
causeToJS Cause.Empty = _causeEmpty
causeToJS (Cause.Fail v) = _causeFail v
causeToJS (Cause.Die err) = _causeDie err
causeToJS (Cause.Interrupt fid) = _causeInterrupt (FiberId.unFiberId fid)
causeToJS (Cause.Then a b) = _causeThen (causeToJS a) (causeToJS b)
causeToJS (Cause.Both a b) = _causeBoth (causeToJS a) (causeToJS b)

-- | Reconstruct a `Cause` from a JS Cause tree.
jsToCause :: forall e. JSCause e -> Cause e
jsToCause c =
  let
    t = _causeTag c
  in
    if t == 0 then Cause.Empty
    else if t == 1 then Cause.Fail (_causeFailValue c)
    else if t == 2 then Cause.Die (_causeDieValue c)
    else if t == 3 then Cause.Interrupt (FiberId.FiberId (_causeInterruptValue c))
    else if t == 4 then Cause.Then (jsToCause (_causeLeft c)) (jsToCause (_causeRight c))
    else Cause.Both (jsToCause (_causeLeft c)) (jsToCause (_causeRight c))

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

foreign import _fiberId :: forall e a. Fiber e a -> Int
foreign import _fiberLabel :: forall e a. Fiber e a -> Effect NullableLabel
foreign import _fiberSetLabel
  :: forall e a. Fiber e a -> String -> Effect Unit

-- | 0 = Running, 1 = Suspended, 2 = Done.
foreign import _fiberStatusCode :: forall e a. Fiber e a -> Effect Int

-- | Fused sync runner used by `runRIO'`. Starts the fiber, then if it
-- | completed with M_OK returns the value directly; otherwise throws.
-- | Skips the Maybe / Outcome / Either round-trip the layered runner
-- | builds. Statically `M_FAIL` cannot happen at the call site
-- | (`runRIO'`'s error row is uninhabited); the FFI defends against
-- | misuse by throwing rather than silently returning.
foreign import _runFiberSyncOrThrow
  :: forall r e a. Op r e a -> Record r -> Effect a

-- | Fused sync runner used by `runRIO`. Returns `Right a` for the OK
-- | path, `Left variant` for typed failure (M_FAIL or the Fail leaf of
-- | a raised Cause). Throws on defect, interrupt, or unfinished. Skips
-- | the layered runner's Outcome / Maybe round-trip.
foreign import _runFiberSyncEither
  :: forall r e a. Op r e a -> Record r -> Effect (Either (Variant e) a)

foreign import _resultIsOk :: forall e a. FiberResult e a -> Boolean
foreign import _resultIsFail :: forall e a. FiberResult e a -> Boolean
foreign import _resultIsInterrupted :: forall e a. FiberResult e a -> Boolean
foreign import _resultIsCause :: forall e a. FiberResult e a -> Boolean
foreign import _resultOk :: forall e a. FiberResult e a -> a
foreign import _resultFail :: forall e a. FiberResult e a -> Variant e
foreign import _resultDie :: forall e a. FiberResult e a -> Error
foreign import _resultInterruptedBy :: forall e a. FiberResult e a -> Int
foreign import _resultCause :: forall e a. FiberResult e a -> JSCause e

resultToOutcome :: forall e a. FiberResult e a -> Outcome e a
resultToOutcome r
  | _resultIsOk r = Success (_resultOk r)
  | _resultIsFail r = Fail (_resultFail r)
  | _resultIsInterrupted r = Interrupted
  | _resultIsCause r = causeToOutcome (jsToCause (_resultCause r))
  | otherwise = Die (_resultDie r)

-- | Project a composed `Cause` onto the flat `Outcome` view, which
-- | only has a leaf shape per failure mode. Defects shadow typed
-- | failures (defect dominates), and interrupts dominate typed
-- | failures. A pure composition of typed failures keeps the first
-- | leaf.
causeToOutcome :: forall e a. Cause e -> Outcome e a
causeToOutcome c = case findLeaf c of
  Just leaf -> leaf
  Nothing -> Interrupted
  where
  findLeaf :: Cause e -> Maybe (Outcome e a)
  findLeaf Cause.Empty = Nothing
  findLeaf (Cause.Die err) = Just (Die err)
  findLeaf (Cause.Interrupt _) = Just Interrupted
  findLeaf (Cause.Fail v) = Just (Fail v)
  findLeaf (Cause.Then a b) = preferDefect (findLeaf a) (findLeaf b)
  findLeaf (Cause.Both a b) = preferDefect (findLeaf a) (findLeaf b)

  preferDefect :: Maybe (Outcome e a) -> Maybe (Outcome e a) -> Maybe (Outcome e a)
  preferDefect (Just (Die e)) _ = Just (Die e)
  preferDefect _ (Just (Die e)) = Just (Die e)
  preferDefect (Just x) _ = Just x
  preferDefect Nothing y = y

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
  | _resultIsInterrupted r =
      Left (Cause.interrupt (FiberId.FiberId (_resultInterruptedBy r)))
  | _resultIsCause r = Left (jsToCause (_resultCause r))
  | otherwise = Left (Cause.die (_resultDie r))

-- | Start a fiber executing the given program against `env`. Returns
-- | the fiber handle synchronously; the fiber may already have
-- | completed (if its body was fully synchronous).
startFiber :: forall r e a. RIO r e a -> Record r -> Effect (Fiber e a)
startFiber (RIO op) = _startFiber op

-- | Has this fiber completed?
fiberIsDone :: forall e a. Fiber e a -> Boolean
fiberIsDone = _fiberIsDone

-- | Read the outcome from a fiber that the caller has already
-- | confirmed is `fiberIsDone`. Unsafe to call on a running fiber; the
-- | result object is null until `_complete` installs it. Used by the
-- | Aff bridge to skip the makeAff round-trip when a program finishes
-- | synchronously inside `startFiber`.
fiberOutcome :: forall e a. Fiber e a -> Outcome e a
fiberOutcome f = resultToOutcome (_fiberResult f)

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
