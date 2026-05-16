-- | Resource-safety primitives for `RIO`.
-- |
-- | Phase 4.1 adds `acquireRelease`, the bracket-style primitive that
-- | guarantees a release action runs on every path: success, typed
-- | failure, defect, or fiber kill. Phase 4.2 adds `Scope` and
-- | `scoped` for LIFO finalizers.
-- |
-- | All of these build directly on `Effect.Aff.bracket`, whose release
-- | phase is uninterruptible by default (verified by the Phase 0.5
-- | spike). See `spikes/aff-interruption/FINDINGS.md` scenario S6 for
-- | the underlying evidence.
module RIO.Resource
  ( acquireRelease
  , bracket
  , ensuring
  , onInterrupt
  , Scope(..)
  , addFinalizer
  , scoped
  ) where

import Prelude

import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Aff (Aff, attempt, finally)
import Effect.Aff (bracket, generalBracket) as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Record.Unsafe (unsafeSet)

import RIO.Internal (RIO(..), unRIO)

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
acquireRelease acquire release use = RIO \r ->
  Aff.bracket
    (unRIO acquire r)
    ( case _ of
        Left _ -> pure unit
        Right a -> do
          relRes <- unRIO (release a) r
          case relRes of
            Right _ -> pure unit
            Left v -> Variant.case_ v
    )
    ( case _ of
        Left v -> pure (Left v)
        Right a -> unRIO (use a) r
    )

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
bracket acquire release use = RIO \r ->
  Aff.bracket
    (unRIO acquire r)
    ( case _ of
        Left _ -> pure unit
        Right a -> do
          relRes <- unRIO (release a) r
          case relRes of
            Right _ -> pure unit
            -- Release errors are silently swallowed; use
            -- `acquireRelease` directly if you need to observe them.
            Left _ -> pure unit
    )
    ( case _ of
        Left v -> pure (Left v)
        Right a -> unRIO (use a) r
    )

-- | `finally`-style guarantor: run `finalizer` after `action`, on
-- | every termination path (success, typed failure, defect, or
-- | external fiber kill). Use it when you have a cleanup to attach
-- | but no acquire/use split worth modelling with `acquireRelease`.
-- |
-- | The finalizer's error row is `()`: it cannot fail with a typed
-- | error because there's no caller-visible place to surface one.
-- | Defects from the finalizer propagate as `Aff` exceptions and are
-- | observable via `RIO.Error.sandbox`.
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
ensuring action finalizer = RIO \r ->
  let
    finAff = do
      res <- unRIO finalizer r
      case res of
        Right _ -> pure unit
        Left v -> Variant.case_ v
  in
    finally finAff (unRIO action r)

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
-- | `Aff` exceptions and are observable via `RIO.Error.sandbox`
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
onInterrupt action finalizer = RIO \r ->
  let
    finAff = do
      res <- unRIO finalizer r
      case res of
        Right _ -> pure unit
        Left v -> Variant.case_ v
  in
    Aff.generalBracket
      (pure unit)
      { killed: \_ _ -> finAff
      , failed: \_ _ -> pure unit
      , completed: \_ _ -> pure unit
      }
      (\_ -> unRIO action r)

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
-- | (specifically `RIO.Layer.provideLayer`, which needs to share one
-- | scope across a layer-build phase and a program-run phase).
-- | `RIO.Core` re-exports only the opaque type, so user code that
-- | reaches the library through that module cannot construct a
-- | `Scope` directly.
newtype Scope = Scope (Ref.Ref (Array (Aff Unit)))

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
addFinalizer (Scope ref) fin = RIO \_ -> do
  liftEffect (Ref.modify_ (\xs -> [ fin ] <> xs) ref)
  pure (Right unit)

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
scoped inner = RIO \r -> do
  ref <- liftEffect (Ref.new [])
  let
    scope = Scope ref
    -- `unsafeSet` lets us extend the row without a `Lacks` constraint;
    -- the resulting record's type is inferred from `unRIO inner`'s
    -- argument shape, which pins it to `(scope :: Scope | r)`. Same
    -- trust pattern as `provide` in `RIO.Env`.
    extended = unsafeSet "scope" scope r
  Aff.bracket
    (pure unit)
    ( \_ -> do
        fins <- liftEffect (Ref.read ref)
        -- Run each finalizer, swallowing exceptions so a single bad
        -- finalizer doesn't prevent the rest from executing. `fins`
        -- is already LIFO because we prepend on registration.
        foldr (\fin acc -> attempt fin *> acc) (pure unit) fins
    )
    (\_ -> unRIO inner extended)
