-- | Resource-safety primitives for `RIO`.
-- |
-- | `acquireRelease` is the bracket-style primitive that guarantees a
-- | release action runs on every path: success, typed failure, defect,
-- | or fiber kill. `Scope` and `scoped` give LIFO finalizers for
-- | resources that share a lifetime.
-- |
-- | All of these build directly on `Effect.Aff.bracket`, whose release
-- | phase is uninterruptible by default. See
-- | `spikes/aff-interruption/FINDINGS.md` scenario S6 for the
-- | underlying evidence.
module RIO.Aff.Resource
  ( acquireRelease
  , bracket
  , ensuring
  , onInterrupt
  , Scope(..)
  , SomeCause
  , addFinalizer
  , addFinalizerExit
  , addFinalizerRIO
  , scoped
  , supervised
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, finally)
import Effect.Aff (bracket, generalBracket) as Aff
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Record.Unsafe (unsafeSet)
import Unsafe.Coerce (unsafeCoerce)

import RIO.Aff.Cause (Cause(..)) as Cause
import RIO.Aff.Cause (Cause)
import RIO.Aff.Internal (RIO(..), mkRIO, matchTypedFailure, unsafeUnRIO)

-- | Run `acquire`, then `use`, then `release`, guaranteeing that
-- | `release` runs no matter how `use` ends: success, typed failure,
-- | defect (`die` or any `Aff` exception), or external fiber kill.
-- |
-- | The release path has an empty error row (`()`); a release action
-- | cannot fail with a typed error because there's no caller-visible
-- | place to put one. Defects in the release path *will* propagate as
-- | `Aff` exceptions and are observable via `sandbox` at the call site.
-- |
-- | If `acquire` itself fails (typed or defect), `release` is **not**
-- | called, because there is nothing to release. The typed failure or
-- | defect propagates unchanged.
-- |
-- | ```purescript
-- | -- open a file, read it, guarantee the handle is closed
-- | readContents :: forall r e. String -> RIO r e String
-- | readContents path = acquireRelease
-- |   (liftAff (FS.openRead path))
-- |   (\h -> liftAff (FS.close h))
-- |   (\h -> liftAff (FS.readAll h))
-- | ```
acquireRelease
  :: forall r e a b
   . RIO r e a
  -> (a -> RIO r () Unit)
  -> (a -> RIO r e b)
  -> RIO r e b
acquireRelease acquire release use = mkRIO \r ->
  Aff.bracket
    (unsafeUnRIO acquire r)
    (\a -> unsafeUnRIO (release a) r)
    (\a -> unsafeUnRIO (use a) r)

-- | Top-level bracket sugar: the same shape as `acquireRelease` but
-- | the release action shares the *use* error row. Any typed failure
-- | thrown during release is silently swallowed so the `use` result is
-- | the one that surfaces.
-- |
-- | Reach for `bracket` when you want a quick acquire / use / release
-- | wrapper without having to pre-handle the release path's errors.
-- | If you need to *observe* release failures, use `acquireRelease`
-- | (whose release row is `()`) and surface the cleanup result
-- | explicitly.
-- |
-- | Same termination guarantees as `acquireRelease`: release runs on
-- | success, typed failure, defect, and fiber kill, in the
-- | uninterruptible release phase of the underlying `Aff` bracket.
-- |
-- | ```purescript
-- | -- open / use / close, ignoring close failures
-- | withConn :: forall r e a. RIO r e Conn -> (Conn -> RIO r e Unit) -> (Conn -> RIO r e a) -> RIO r e a
-- | withConn = bracket
-- | ```
bracket
  :: forall r e a b
   . RIO r e a
  -> (a -> RIO r e Unit)
  -> (a -> RIO r e b)
  -> RIO r e b
bracket acquire release use = mkRIO \r ->
  Aff.bracket
    (unsafeUnRIO acquire r)
    ( \a -> do
        outcome <- attempt (unsafeUnRIO (release a) r)
        case outcome of
          Right _ -> pure unit
          -- Release typed failures are silently swallowed; use
          -- `acquireRelease` directly if you need to observe them.
          -- Defects propagate.
          Left err -> case matchTypedFailure err of
            Just _ -> pure unit
            Nothing -> throwError err
    )
    (\a -> unsafeUnRIO (use a) r)

-- | `finally`-style guarantor: run `finalizer` after `action`, on
-- | every termination path (success, typed failure, defect, or
-- | external fiber kill). Use it when you have a cleanup to attach
-- | but no acquire/use split worth modelling with `acquireRelease`.
-- |
-- | The finalizer's error row is `()`: it cannot fail with a typed
-- | error because there's no caller-visible place to surface one.
-- | Defects from the finalizer propagate as `Aff` exceptions and are
-- | observable via `RIO.Aff.Error.sandbox`.
-- |
-- | The finalizer runs in the underlying `Aff` `finally`'s release
-- | phase, which is uninterruptible: a kill landing during the
-- | finalizer is queued until it completes.
-- |
-- | ```purescript
-- | -- guarantee the connection pool is drained, no matter how the
-- | -- inner action ends
-- | drainOnExit = ensuring serveRequests drainPool
-- | ```
ensuring :: forall r e a. RIO r e a -> RIO r () Unit -> RIO r e a
ensuring action finalizer = mkRIO \r ->
  finally (unsafeUnRIO finalizer r) (unsafeUnRIO action r)

-- | Run `finalizer` only when `action` is interrupted (the
-- | fiber is killed). Normal completion, typed failure, and
-- | defects all skip the finalizer; this is the
-- | cancellation-specific counterpart to `ensuring` (which
-- | fires on every termination path).
-- |
-- | Mirrors ZIO `ZIO.onInterrupt` / Effect-TS `Effect.onInterrupt`.
-- | Use it when the cleanup is *the rollback you owe specifically
-- | on cancellation*, distinct from cleanup you would run on any
-- | exit: enqueue a "request was cancelled by the client" entry,
-- | release a half-claimed lease, mark a half-applied write as
-- | aborted. For "always-on" cleanup, reach for `ensuring` or
-- | `acquireRelease` instead.
-- |
-- | The finalizer's error row is `()`; it cannot fail with a
-- | typed error. Defects raised inside the finalizer propagate as
-- | `Aff` exceptions and are observable via `RIO.Aff.Error.sandbox`
-- | at the call site. The finalizer runs in the underlying `Aff`
-- | bracket's release phase, which is uninterruptible: a kill
-- | landing during the finalizer is queued until it completes.
-- |
-- | ```purescript
-- | -- mark a pending write aborted only if the caller cancelled
-- | applyWrite = onInterrupt
-- |   (commitTwoPhase writeId)
-- |   (markAborted writeId)
-- | ```
onInterrupt :: forall r e a. RIO r e a -> RIO r () Unit -> RIO r e a
onInterrupt action finalizer = mkRIO \r ->
  Aff.generalBracket
    (pure unit)
    { killed: \_ _ -> unsafeUnRIO finalizer r
    , failed: \_ _ -> pure unit
    , completed: \_ _ -> pure unit
    }
    (\_ -> unsafeUnRIO action r)

-- | A scope is a place to register finalizers that will run on exit.
-- |
-- | Use `scoped` to introduce a scope; use `addFinalizer` to push an
-- | `Aff` action onto its finalizer list. On exit (success, typed
-- | failure, defect, or kill), every registered finalizer runs in
-- | last-in-first-out order, in the uninterruptible release phase of
-- | the underlying `Aff` bracket.
-- |
-- | Each finalizer is allowed to throw; its exception is caught and
-- | does not stop subsequent finalizers from running. (We can't yet
-- | aggregate finalizer errors; for now they are swallowed by design,
-- | so a leak in one finalizer doesn't cascade.)
-- |
-- | The data constructor is exported for use inside this library
-- | (specifically `RIO.Aff.Layer.provideLayer`, which needs to share one
-- | scope across a layer-build phase and a program-run phase).
-- | `RIO.Aff.Core` re-exports only the opaque type, so user code that
-- | reaches the library through that module cannot construct a
-- | `Scope` directly.
-- |
-- | The `pendingCause` Ref is set by `scoped` (and `supervised`)
-- | right before its finalizer pass starts, and is observable from
-- | inside exit-aware finalizers registered with `addFinalizerExit`.
-- | It is stored under an erased `SomeCause` shape so the storage
-- | does not have to mention the body's typed-error row; the
-- | exit-aware finalizer recovers its own `Cause e` via
-- | `unsafeCoerce` at observation time, which is safe because the
-- | only writer is the matching `scoped` block.
newtype Scope = Scope
  { finalizers :: Ref (Array (Aff Unit))
  , pendingCause :: Ref (Maybe SomeCause)
  }

-- | Type-erased `Cause e` used as scope storage. `eraseCause`/
-- | `reifyCause` are the only allowed bridges; both are
-- | `unsafeCoerce` because `Cause` is a regular ADT and the
-- | observed-row witness is restored by the exit-aware finalizer's
-- | own type signature.
foreign import data SomeCause :: Type

eraseCause :: forall e. Cause e -> SomeCause
eraseCause = unsafeCoerce

reifyCause :: forall e. SomeCause -> Cause e
reifyCause = unsafeCoerce

-- | Push an `Aff` action onto a scope's finalizer stack.
-- |
-- | This is a plain `RIO r e Unit`; it doesn't introduce a service row
-- | because the `Scope` is passed in as a value. Inside a `scoped`
-- | block you typically obtain the scope via `ask (Proxy :: _ "scope")`
-- | when the scope is provided as a service, or by direct argument
-- | from a layer-style helper.
-- |
-- | ```purescript
-- | -- inside a scope, register cleanup on exit
-- | scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   conn <- liftAff openConnection
-- |   addFinalizer scope (closeConnection conn)
-- |   useConnection conn
-- | ```
addFinalizer :: forall r e. Scope -> Aff Unit -> RIO r e Unit
addFinalizer (Scope { finalizers }) fin = mkRIO \_ ->
  liftEffect (Ref.modify_ (\xs -> [ fin ] <> xs) finalizers)

-- | Register an exit-aware cleanup with the scope. The handler is
-- | invoked with `Nothing` when the scope closes from a success path
-- | and `Just c` when it closes because the body failed (typed
-- | failure), defected, or was killed. Order with other finalizers
-- | is LIFO across registrations, regardless of which `addFinalizer`
-- | variant was used.
-- |
-- | The handler runs as part of the scope's release phase, which is
-- | uninterruptible. The handler is `Effect Unit` (not `Aff`) to
-- | match the rio-fiber shape; reach for `addFinalizer` or
-- | `addFinalizerRIO` when you need to suspend.
-- |
-- | ```purescript
-- | scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   addFinalizerExit scope \mc -> case mc of
-- |     Nothing -> Console.log "ok"
-- |     Just c -> Console.error ("scope closed with " <> prettyCause showFn c)
-- |   ...
-- | ```
addFinalizerExit
  :: forall r e
   . Scope
  -> (Maybe (Cause e) -> Effect Unit)
  -> RIO r e Unit
addFinalizerExit (Scope { finalizers, pendingCause }) handler =
  mkRIO \_ ->
    liftEffect (Ref.modify_ (\xs -> [ runHandler ] <> xs) finalizers)
  where
  runHandler :: Aff Unit
  runHandler = liftEffect do
    mc <- Ref.read pendingCause
    handler (map reifyCause mc)

-- | Register an `RIO`-valued cleanup with the scope. The action's
-- | environment record is captured at registration time, so any
-- | services it uses come from the scope's creator, not the closer.
-- | The error row is `()`: cleanup actions cannot raise typed
-- | failures because the close path has no caller-visible place to
-- | put one. Defects from the cleanup propagate as `Aff` exceptions
-- | observable via `RIO.Aff.Error.sandbox` at the scope owner.
-- |
-- | The action runs as part of the scope's release phase, which is
-- | uninterruptible.
-- |
-- | ```purescript
-- | scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   conn <- openConn
-- |   addFinalizerRIO scope (closeConnRIO conn)
-- |   useConn conn
-- | ```
addFinalizerRIO :: forall r e. Scope -> RIO r () Unit -> RIO r e Unit
addFinalizerRIO scope action = mkRIO \r ->
  unsafeUnRIO (addFinalizer scope (unsafeUnRIO action r)) r

-- | Run an inner computation in a fresh scope provided as a service
-- | under the label `scope`. Finalizers registered via the scope run
-- | LIFO on exit, on every termination path.
-- |
-- | The inner program's environment is `(scope :: Scope | r)`; the
-- | resulting program needs only `r`. The error and value channels are
-- | preserved unchanged.
-- |
-- | ```purescript
-- | -- everything inside `scoped` shares a finalizer stack; both
-- | -- finalizers fire LIFO when the block exits (success or failure)
-- | program = scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   resA <- openA
-- |   addFinalizer scope (closeA resA)
-- |   resB <- openB resA
-- |   addFinalizer scope (closeB resB)
-- |   useBoth resA resB
-- | ```
scoped
  :: forall r e a
   . RIO (scope :: Scope | r) e a
  -> RIO r e a
scoped = scopedAs "scope"

-- | Like `scoped`, but the inner program sees the fresh scope under
-- | the `supervisor` row label. Pair with `RIO.Aff.Concurrency.forkSupervised`
-- | to fork child fibers whose lifetime is bounded by this scope.
-- |
-- | The body's signature is `RIO (supervisor :: Scope | r) e a`; the
-- | resulting program needs only `r`. The error and value channels
-- | are preserved unchanged. Exit cause (success / typed failure /
-- | defect / kill) is threaded through to exit-aware finalizers
-- | registered with `addFinalizerExit`.
-- |
-- | ```purescript
-- | supervised do
-- |   -- every `forkSupervised` child here is interrupted when the
-- |   -- block exits, regardless of how it exits.
-- |   _ <- forkSupervised pollerOne
-- |   _ <- forkSupervised pollerTwo
-- |   serveForever
-- | ```
supervised
  :: forall r e a
   . RIO (supervisor :: Scope | r) e a
  -> RIO r e a
supervised = scopedAs "supervisor"

-- | Shared implementation behind `scoped` and `supervised`. The
-- | scope is installed under the supplied label and the exit cause
-- | is recorded into the scope's `pendingCause` Ref before the
-- | finalizer pass, so `addFinalizerExit` handlers can observe the
-- | shape of the close (success, typed failure, defect, kill).
scopedAs
  :: forall r r' e a
   . String
  -> RIO r' e a
  -> RIO r e a
scopedAs label inner = mkRIO \r -> do
  finRef <- liftEffect (Ref.new [])
  causeRef <- liftEffect (Ref.new Nothing)
  let
    scope :: Scope
    scope = Scope { finalizers: finRef, pendingCause: causeRef }
    -- `unsafeSet` lets us extend the row without a `Lacks` constraint;
    -- the resulting record's type is inferred from `unsafeUnRIO inner`'s
    -- argument shape, which pins it to `(<label> :: Scope | r)`. Same
    -- trust pattern as `provide` in `RIO.Aff.Env`.
    extended :: Record r'
    extended = unsafeSet label scope r
    runFinalizers :: Aff Unit
    runFinalizers = do
      fins <- liftEffect (Ref.read finRef)
      -- LIFO already (we prepend on registration). Swallow per-
      -- finalizer exceptions so one bad finalizer doesn't strand
      -- the rest.
      foldr (\fin acc -> attempt fin *> acc) (pure unit) fins
  Aff.generalBracket
    (pure unit)
    { killed: \err _ -> do
        liftEffect (Ref.write (Just (eraseCause (Cause.Die err))) causeRef)
        runFinalizers
    , failed: \err _ -> do
        let
          cause = case matchTypedFailure err of
            Just v -> Cause.Fail v
            Nothing -> Cause.Die err
        liftEffect (Ref.write (Just (eraseCause cause)) causeRef)
        runFinalizers
    , completed: \_ _ -> runFinalizers
    }
    (\_ -> unsafeUnRIO inner extended)
