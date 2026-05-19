-- | Composable service constructors.
-- |
-- | A `Layer e rIn rOut` describes how to build a record of services
-- | in row `rOut` from an input environment in row `rIn`, possibly
-- | failing with a typed error in row `e`. Use `provide` to plug a
-- | layer into a program that requires its services, or
-- | `provideScoped` when the layer needs to allocate resources whose
-- | cleanup should run after the inner program finishes.
-- |
-- | The MVP keeps composition linear (`>>>`) and parallel
-- | (`mergeLayers`); cyclic / dependency-graph wiring is left to
-- | application code.
module RIO.Fiber.Layer
  ( Layer
  , fromValue
  , fromRIO
  , chainLayer
  , mergeLayers
  , provide
  , provideScoped
  ) where

import Prelude

import Prim.Row (class Nub, class Union)
import Record as Record
import RIO.Fiber.Internal (RIO(..))
import RIO.Fiber.Internal as Internal
import RIO.Fiber.Scope (Scope, scoped)

-- | A layer that consumes an environment of row `rIn` and builds a
-- | record of services in row `rOut`. The build may raise typed
-- | failures in row `e`.
newtype Layer e rIn rOut = Layer (RIO rIn e (Record rOut))

-- | A layer that ignores its input and returns the given record.
fromValue :: forall e rIn rOut. Record rOut -> Layer e rIn rOut
fromValue r = Layer (pure r)

-- | A layer built from an arbitrary RIO that runs in the input env.
fromRIO :: forall e rIn rOut. RIO rIn e (Record rOut) -> Layer e rIn rOut
fromRIO = Layer

-- | Sequential composition: build `rOut` from `rMid`, then run the
-- | first layer against `rIn` to produce `rMid`. The result needs
-- | only `rIn`.
chainLayer
  :: forall e rIn rMid rOut
   . Layer e rIn rMid
  -> Layer e rMid rOut
  -> Layer e rIn rOut
chainLayer (Layer build1) (Layer build2) = Layer do
  rMid <- build1
  case build2 of
    RIO op -> RIO (Internal.opLocal (\_ -> rMid) op)

-- | Parallel composition: both layers see the same input env, and
-- | their outputs are merged into a single record. The two output
-- | rows must have no overlap (enforced by `Nub`).
mergeLayers
  :: forall e rIn r1 r2 rOut
   . Union r1 r2 rOut
  => Nub rOut rOut
  => Layer e rIn r1
  -> Layer e rIn r2
  -> Layer e rIn rOut
mergeLayers (Layer a) (Layer b) = Layer do
  r1 <- a
  r2 <- b
  pure (Record.merge r1 r2)

-- | Run a program against the environment a layer produces. The
-- | layer builds against the outer env; the inner program sees the
-- | layer's output as its env.
provide
  :: forall e rIn rOut a
   . Layer e rIn rOut
  -> RIO rOut e a
  -> RIO rIn e a
provide (Layer build) (RIO inner) = do
  env' <- build
  RIO (Internal.opLocal (\_ -> env') inner)

-- | Like `provide` but threads a `Scope` so the layer can register
-- | finalizers that run when the inner program exits.
provideScoped
  :: forall e rIn rOut a
   . (Scope -> RIO rIn e (Record rOut))
  -> RIO rOut e a
  -> RIO rIn e a
provideScoped build (RIO inner) = scoped \scope -> do
  env' <- build scope
  RIO (Internal.opLocal (\_ -> env') inner)
