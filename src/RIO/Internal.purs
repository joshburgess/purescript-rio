-- | Internal definition of the `RIO` newtype.
-- |
-- | This module exposes the data constructor and is intended for use *within*
-- | the library only. Library consumers should import from `RIO.Core` (or
-- | from the top-level `RIO` module, when added), which re-exports `RIO` as
-- | an opaque type.
-- |
-- | Cross-module helpers that need to peel the newtype back to its underlying
-- | `Record r -> Aff a` representation belong here. The two peeling helpers
-- | exist for different purposes:
-- |
-- |   * `unsafeUnRIO` is the raw projection: `RIO r e a -> Record r -> Aff a`.
-- |     The result type does NOT mention `Either`, because internally typed
-- |     failures are thrown through `Aff`'s error channel as tagged
-- |     exceptions. Use this in cross-module composition where you intend to
-- |     keep the failure on the throw track (every other `RIO` combinator
-- |     stays on the throw track and only reifies at the boundary).
-- |
-- |   * `unRIO` is the reifying boundary: `RIO r e a -> Record r -> Aff (Either
-- |     (Variant e) a)`. It catches any tagged typed-failure exception and
-- |     reflects it back as `Left`. Defects (untagged exceptions) keep
-- |     propagating. Use this at the public boundary (`runRIO`, `runRIO'`,
-- |     `unsafeRunRIO`, custom runners, FFI bridges) where you actually need
-- |     to inspect the failure shape.
module RIO.Internal
  ( RIO(..)
  , unRIO
  , unsafeUnRIO
  , rioFail
  , matchTypedFailure
  , mkTypedFailureError
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)

-- | `RIO r e a` is a computation that, given an environment of services in
-- | row `r`, performs `Aff` work that either fails with a tagged error in
-- | row `e` or produces a value of type `a`.
-- |
-- | The three parameters are independent:
-- |
-- |   * `r` (services / Reader): a row of typed records the computation can
-- |     read from. Use `ask` / `asks` to retrieve services, `provide` to
-- |     supply them.
-- |   * `e` (errors): a row of named, typed failure cases. Use `fail` to
-- |     raise one, `catchTag` to handle one.
-- |   * `a` (success value): the result on the happy path.
-- |
-- | Both `r` and `e` are open rows by default, so requirements aggregate
-- | automatically on composition and shrink as they're handled or provided.
-- |
-- | Internally, typed failures are thrown through `Aff`'s error channel as
-- | tagged exceptions and reified back to `Either (Variant e) a` only at
-- | public boundaries (`runRIO`, `runRIO'`, `unsafeRunRIO`). This keeps the
-- | per-`bind` overhead close to raw `Aff`: every successful bind is a single
-- | `Aff` bind plus one record argument pass, with no `Either` wrapping.
newtype RIO :: Row Type -> Row Type -> Type -> Type
newtype RIO r e a = RIO (Record r -> Aff a)

-- | Raw projection of the newtype. Result type does NOT mention `Either`
-- | because typed failures stay on the `Aff` throw track. Use this for
-- | internal composition; reach for `unRIO` only when you need to reify the
-- | failure shape.
unsafeUnRIO :: forall r e a. RIO r e a -> Record r -> Aff a
unsafeUnRIO (RIO f) = f

-- | Public boundary: peel the newtype and reify any tagged typed-failure
-- | exception back to `Left (Variant e)`. Defects (untagged Aff exceptions)
-- | keep propagating.
unRIO :: forall r e a. RIO r e a -> Record r -> Aff (Either (Variant e) a)
unRIO (RIO f) r = do
  outcome <- attempt (f r)
  case outcome of
    Right a -> pure (Right a)
    Left err -> case matchTypedFailure err of
      Just v -> pure (Left v)
      Nothing -> throwError err

-- | Raise a typed failure through `Aff`'s error channel. The thrown
-- | exception carries the `Variant` payload on a marker property so
-- | `matchTypedFailure` can recover it; defects (any other `Aff` exception)
-- | are passed through unchanged.
rioFail :: forall e a. Variant e -> Aff a
rioFail v = throwError (_mkTypedFailure v)

-- | Match a tagged typed-failure exception. Returns `Just v` if the error
-- | object carries the typed-failure marker; otherwise returns `Nothing`
-- | (the error is a defect and should be re-thrown).
matchTypedFailure :: forall e. Error -> Maybe (Variant e)
matchTypedFailure err = _matchTypedFailure Nothing Just err

-- | Construct the tagged exception value used to encode a typed failure on
-- | `Aff`'s error channel. Use this when interoperating with `Aff` primitives
-- | that take an `Either Error a` (such as `makeAff`'s `resume`) and you need
-- | to deliver a typed failure rather than a defect.
mkTypedFailureError :: forall e. Variant e -> Error
mkTypedFailureError = _mkTypedFailure

foreign import _mkTypedFailure :: forall e. Variant e -> Error
foreign import _matchTypedFailure
  :: forall e r
   . r
  -> (Variant e -> r)
  -> Error
  -> r

-- | `map` threads the environment through the underlying function and
-- | applies `f` only to the success branch. Typed failures propagate via the
-- | underlying `Aff`'s exception channel and skip `f` automatically.
instance functorRIO :: Functor (RIO r e) where
  map f (RIO g) = RIO \r -> map f (g r)

-- | `apply` runs the function and argument computations sequentially against
-- | the same environment. A typed failure on either side short-circuits via
-- | the underlying `Aff`'s exception channel.
instance applyRIO :: Apply (RIO r e) where
  apply (RIO f) (RIO g) = RIO \r -> f r <*> g r

-- | `pure` ignores the environment and lifts the value through `Aff`.
instance applicativeRIO :: Applicative (RIO r e) where
  pure a = RIO \_ -> pure a

-- | `bind` is a single `Aff` bind plus one record-argument pass. Typed
-- | failures propagate via the underlying `Aff`'s exception channel; the
-- | continuation is invoked only on success.
instance bindRIO :: Bind (RIO r e) where
  bind (RIO m) k = RIO \r -> m r >>= \a -> unsafeUnRIO (k a) r

instance monadRIO :: Monad (RIO r e)

-- | Lift an `Effect` action into `RIO`. The environment is ignored and the
-- | error row remains polymorphic; effects raised via `liftEffect` never
-- | produce a typed failure (uncaught exceptions surface as `Aff` defects,
-- | recoverable through `RIO.Error.sandbox`).
instance monadEffectRIO :: MonadEffect (RIO r e) where
  liftEffect eff = RIO \_ -> liftEffect eff

-- | Lift an arbitrary `Aff` action into `RIO`. Same semantics as
-- | `MonadEffect`: environment ignored, error row left polymorphic.
instance monadAffRIO :: MonadAff (RIO r e) where
  liftAff aff = RIO \_ -> aff

