-- | Internal definition of the `RIO` newtype.
-- |
-- | This module exposes the data constructor and is intended for use *within*
-- | the library only. Library consumers should import from `RIO.Core` (or
-- | from the top-level `RIO` module, when added), which re-exports `RIO` as
-- | an opaque type.
-- |
-- | Cross-module helpers that need to peel the newtype back to its underlying
-- | `Record r -> Aff (Either (Variant e) a)` representation belong here.
module RIO.Internal
  ( RIO(..)
  , unRIO
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)

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
newtype RIO :: Row Type -> Row Type -> Type -> Type
newtype RIO r e a = RIO (Record r -> Aff (Either (Variant e) a))

-- | Peel the newtype. Internal helper; not part of the public surface.
unRIO :: forall r e a. RIO r e a -> Record r -> Aff (Either (Variant e) a)
unRIO (RIO f) = f

-- | `map` threads the environment through the underlying function and
-- | applies `f` only to the success branch; a `Left` short-circuits
-- | unchanged.
instance functorRIO :: Functor (RIO r e) where
  map f (RIO g) = RIO \r -> map (map f) (g r)

-- | `apply` runs the function and argument computations sequentially
-- | against the same environment. If the function side produces `Left`,
-- | the argument side is not run; this matches the monadic short-circuit
-- | semantics of the error channel.
instance applyRIO :: Apply (RIO r e) where
  apply (RIO f) (RIO g) = RIO \r -> do
    rf <- f r
    case rf of
      Left e -> pure (Left e)
      Right h -> map (map h) (g r)

-- | `pure` ignores the environment and produces `Right a` in `Aff`.
instance applicativeRIO :: Applicative (RIO r e) where
  pure a = RIO \_ -> pure (Right a)

-- | `bind` short-circuits on the first `Left` it sees. The continuation
-- | is invoked only on `Right`, and receives the same environment.
instance bindRIO :: Bind (RIO r e) where
  bind (RIO m) k = RIO \r -> do
    res <- m r
    case res of
      Left e -> pure (Left e)
      Right a -> unRIO (k a) r

instance monadRIO :: Monad (RIO r e)

-- | Lift an `Effect` action into `RIO`. The environment is ignored and
-- | the error row remains polymorphic; effects raised via `liftEffect`
-- | never produce a typed failure (uncaught exceptions surface as
-- | `Aff` defects, to be exposed by `sandbox` in Phase 3.3).
instance monadEffectRIO :: MonadEffect (RIO r e) where
  liftEffect eff = RIO \_ -> map Right (liftEffect eff)

-- | Lift an arbitrary `Aff` action into `RIO`. Same semantics as
-- | `MonadEffect`: environment ignored, error row left polymorphic.
instance monadAffRIO :: MonadAff (RIO r e) where
  liftAff aff = RIO \_ -> map Right aff
