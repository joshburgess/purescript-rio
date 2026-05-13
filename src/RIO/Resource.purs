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
  , Scope(..)
  , addFinalizer
  , scoped
  ) where

import Prelude

import Data.Array (foldr)
import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Aff (Aff, attempt, bracket)
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
acquireRelease
  :: forall r e a b
   . RIO r e a
  -> (a -> RIO r () Unit)
  -> (a -> RIO r e b)
  -> RIO r e b
acquireRelease acquire release use = RIO \r ->
  bracket
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
  bracket
    (pure unit)
    ( \_ -> do
        fins <- liftEffect (Ref.read ref)
        -- Run each finalizer, swallowing exceptions so a single bad
        -- finalizer doesn't prevent the rest from executing. `fins`
        -- is already LIFO because we prepend on registration.
        foldr (\fin acc -> attempt fin *> acc) (pure unit) fins
    )
    (\_ -> unRIO inner extended)
