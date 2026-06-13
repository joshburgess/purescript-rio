-- | Composable service constructors.
-- |
-- | A `Layer e rIn rOut` describes how to build a record of services
-- | in row `rOut` from an input environment in row `rIn`, possibly
-- | failing with a typed error in row `e`. Use `provide` to plug a
-- | layer into a program that requires its services, or
-- | `provideScoped` when the layer needs to allocate resources whose
-- | cleanup should run after the inner program finishes.
-- |
-- | Every layer is built against a `Scope` so that resourceful layers
-- | (`scoped`) can register finalizers that fire when the layer's
-- | enclosing `provide` exits. Layers built from `fromValue` /
-- | `fromRIO` simply ignore the scope.
-- |
-- | Composition is linear (`chainLayer`) or parallel (`mergeLayers`);
-- | cyclic / dependency-graph wiring is left to application code.
-- |
-- | Use `memoize` when the same layer appears in multiple
-- | compositions and you want a single shared build. Use `fresh` to
-- | document that a layer should not be shared.
module RIO.Fiber.Layer
  ( Layer
  , fromValue
  , fromRIO
  , scoped
  , chainLayer
  , mergeLayers
  , passthrough
  , provide
  , provideScoped
  , memoize
  , fresh
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import Prim.Row (class Nub, class Union)
import Record as Record
import RIO.Fiber.Cause (Cause)
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Core as F
import RIO.Fiber.Deferred (Deferred)
import RIO.Fiber.Deferred as Deferred
import RIO.Fiber.FiberId as FiberId
import RIO.Fiber.Internal (RIO(..))
import RIO.Fiber.Internal as Internal
import RIO.Fiber.Scope (Scope)
import RIO.Fiber.Scope as Scope

-- | A layer that consumes an environment of row `rIn` and builds a
-- | record of services in row `rOut`. The build runs against a
-- | `Scope` (so resourceful layers can register finalizers) and may
-- | raise typed failures in row `e`.
newtype Layer e rIn rOut = Layer (Scope -> RIO rIn e (Record rOut))

-- | A layer that ignores its input and returns the given record.
fromValue :: forall e rIn rOut. Record rOut -> Layer e rIn rOut
fromValue r = Layer (\_ -> pure r)

-- | A layer built from an arbitrary RIO that runs in the input env.
-- | The build ignores the scope; for finalizer-aware layers use
-- | `scoped` instead.
fromRIO :: forall e rIn rOut. RIO rIn e (Record rOut) -> Layer e rIn rOut
fromRIO build = Layer (\_ -> build)

-- | Build a layer that may register finalizers on the supplied scope.
-- | The scope is closed when the enclosing `provide` exits, so
-- | finalizers run regardless of how the inner program terminates.
scoped
  :: forall e rIn rOut
   . (Scope -> RIO rIn e (Record rOut))
  -> Layer e rIn rOut
scoped = Layer

-- | Sequential composition: run the first layer against `rIn` to
-- | produce `rMid`, then run the second layer against `rMid` to
-- | produce `rOut`. The result needs only `rIn`. Both layers share
-- | the same scope, so finalizers from either side run when
-- | `provide` exits.
chainLayer
  :: forall e rIn rMid rOut
   . Layer e rIn rMid
  -> Layer e rMid rOut
  -> Layer e rIn rOut
chainLayer (Layer build1) (Layer build2) = Layer \scope -> do
  rMid <- build1 scope
  case build2 scope of
    RIO op -> RIO (Internal.opLocal (\_ -> rMid) op)

-- | Parallel composition: both layers see the same input env, and
-- | their outputs are merged into a single record. The two output
-- | rows must have no overlap (enforced by `Nub`). Both halves share
-- | the supplied scope.
mergeLayers
  :: forall e rIn r1 r2 rOut
   . Union r1 r2 rOut
  => Nub rOut rOut
  => Layer e rIn r1
  -> Layer e rIn r2
  -> Layer e rIn rOut
mergeLayers (Layer a) (Layer b) = Layer \scope -> do
  r1 <- a scope
  r2 <- b scope
  pure (Record.merge r1 r2)

-- | Re-export the layer's input row as part of its output. The
-- | resulting layer hands downstream consumers both the produced
-- | services *and* the upstream ones the layer was built against.
-- |
-- | The plain `chainLayer` "consumes" the input row: chaining
-- | `configLayer` and then `dbLayer` yields a layer whose output is
-- | just `(db :: Database)`, even though `dbLayer`'s caller probably
-- | also wants `(config :: Config)` at hand. `passthrough` adds the
-- | input row back into the output:
-- |
-- | ```purescript
-- | -- configLayer :: Layer e ()                 (config :: Config)
-- | -- dbLayer     :: Layer e (config :: Config) (db :: Database)
-- | --
-- | -- without passthrough: only `db` is visible downstream
-- | base = chainLayer configLayer dbLayer
-- |
-- | -- with passthrough on dbLayer: both are visible downstream
-- | base = chainLayer configLayer (passthrough dbLayer)
-- | -- :: Layer e () (config :: Config, db :: Database)
-- | ```
-- |
-- | The required output row is supplied by the `Union` constraint:
-- | `rIn` plus the layer's own output `rOut` equals `rPassed`. If the
-- | rows aren't disjoint, the compiler rejects the call.
passthrough
  :: forall e rIn rOut rPassed
   . Union rOut rIn rPassed
  => Layer e rIn rOut
  -> Layer e rIn rPassed
passthrough (Layer build) = Layer \scope -> do
  outRec <- build scope
  inRec <- F.ask
  pure (Record.union outRec inRec :: Record rPassed)

-- | Run a program against the environment a layer produces. A fresh
-- | `Scope` is opened for the layer's lifetime and closed once the
-- | inner program finishes (any outcome), running every finalizer
-- | the layer registered.
provide
  :: forall e rIn rOut a
   . Layer e rIn rOut
  -> RIO rOut e a
  -> RIO rIn e a
provide (Layer build) (RIO inner) = Scope.scoped \scope -> do
  env' <- build scope
  RIO (Internal.opLocal (\_ -> env') inner)

-- | Backwards-compatible alias: build a scoped layer inline and run
-- | it. Equivalent to `provide (scoped build) inner`.
provideScoped
  :: forall e rIn rOut a
   . (Scope -> RIO rIn e (Record rOut))
  -> RIO rOut e a
  -> RIO rIn e a
provideScoped build = provide (scoped build)

-- | Wrap a layer so its build is shared across every use. The first
-- | call to the returned layer runs the underlying build; concurrent
-- | first calls single-flight on a `Deferred`; subsequent calls return
-- | the cached record without re-running the build.
-- |
-- | The shared build allocates resources against the scope of the
-- | first caller. If that scope closes before later callers finish,
-- | the cached record's finalizers have already run. For long-lived
-- | sharing, ensure the first `provide` outlives every later use.
-- |
-- | A typed failure or defect produced by the build is captured and
-- | replayed on every subsequent call: the underlying action does not
-- | run again to "try again". Wrap the layer in a retry schedule
-- | *before* memoizing if retry semantics are required.
memoize
  :: forall e rIn rOut
   . Layer e rIn rOut
  -> Effect (Layer e rIn rOut)
memoize (Layer build) = do
  cell <- Ref.new Nothing
  pure (Layer (memoCell build cell))

memoCell
  :: forall e rIn rOut
   . (Scope -> RIO rIn e (Record rOut))
  -> Ref.Ref (Maybe (Deferred () (Either (Cause e) (Record rOut))))
  -> Scope
  -> RIO rIn e (Record rOut)
memoCell build cell scope = do
  decision <- F.liftEffect do
    existing <- Ref.read cell
    case existing of
      Just d -> pure (Awaiter d)
      Nothing -> do
        d <- Deferred.make
        Ref.write (Just d) cell
        pure (Owner d)
  case decision of
    Awaiter d -> Deferred.awaitPure d >>= reproduce
    Owner d ->
      F.ensuring
        (void (Deferred.succeed d (Left (Cause.interrupt FiberId.externalFiberId))))
        ( do
            outcome <- F.causeOf (build scope)
            _ <- Deferred.succeed d outcome
            reproduce outcome
        )

reproduce
  :: forall r e rOut
   . Either (Cause e) (Record rOut)
  -> RIO r e (Record rOut)
reproduce = case _ of
  Right r -> pure r
  Left cause -> case Cause.firstFailure cause of
    Just v -> F.fail v
    Nothing -> F.failCause cause

data Decision e rOut
  = Awaiter (Deferred () (Either (Cause e) (Record rOut)))
  | Owner (Deferred () (Either (Cause e) (Record rOut)))

-- | Mark a layer as "do not share". In our current model layers are
-- | already built fresh on every `provide`, so this is the identity
-- | function; it exists for compositional symmetry with `memoize` and
-- | as a forward-compatible marker if implicit sharing is ever added.
fresh :: forall e rIn rOut. Layer e rIn rOut -> Layer e rIn rOut
fresh = identity
