-- | `Layer rIn e rOut`: a recipe for constructing a record of services
-- | `rOut` from a record of services `rIn`, possibly failing with a
-- | typed error in `Variant e`. Layers compose vertically (`andThen`,
-- | infix `(>>>)`) and horizontally (`combine`, infix `(<+>)`), and
-- | they may register finalizers in the surrounding scope so resources
-- | are released when the providing scope exits.
-- |
-- | Phase 5.1 introduced the type and `buildLayer`. Phase 5.2 adds
-- | `andThen` / `combine` (and their operator aliases). The
-- | user-facing combinator `provideLayer` lands in Phase 5.3.
-- |
-- | The infix `(>>>)` shadows `Control.Semigroupoid.(>>>)` from
-- | `Prelude` when both are imported. Hide one or use the named form
-- | (`andThen`) when both are needed in the same module.
module RIO.Layer
  ( Layer
  , andThen
  , buildLayer
  , combine
  , fromRecord
  , fromRIO
  , provideLayer
  , unLayer
  , (>>>)
  , (<+>)
  ) where

import Prelude hiding ((>>>))

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant (expand) as Variant
import Effect.Aff (Aff, attempt, bracket)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Array (foldr)
import Prim.Row (class Union) as Row
import Record (union) as Record
import Record.Unsafe (unsafeGet, unsafeSet)
import Unsafe.Coerce (unsafeCoerce)

import RIO.Internal (RIO(..), unRIO)
import RIO.Resource (Scope(..), scoped)

-- | A layer is an `RIO` that runs in the surrounding `Scope` and
-- | returns a record of services.
-- |
-- | The constructor is hidden; build layers with `fromRecord` (for a
-- | static record), `fromRIO` (for one that asks the input row, lifts
-- | `Aff`, or registers finalizers via the scope service), or with
-- | the composition combinators in this module.
newtype Layer rIn e rOut =
  Layer (RIO (scope :: Scope | rIn) e (Record rOut))

-- | The simplest layer: hand back a fixed record of services. The
-- | input row is left free; horizontal and sequential composition
-- | unify it with the surrounding context.
fromRecord :: forall rIn e rOut. Record rOut -> Layer rIn e rOut
fromRecord r = Layer (pure r)

-- | Build a layer from an explicit `RIO` action. Inside the action
-- | you can `ask` for services from the input row, lift `Aff`, and
-- | register finalizers via the `scope` service. The returned record
-- | is what downstream programs receive when the layer is provided.
fromRIO
  :: forall rIn e rOut
   . RIO (scope :: Scope | rIn) e (Record rOut)
  -> Layer rIn e rOut
fromRIO = Layer

-- | Reveal the underlying `RIO` for use by other parts of the
-- | library (composition, `provideLayer`). Not exported from
-- | `RIO.Core`; consumers should compose layers via the combinators
-- | rather than reaching for this.
unLayer
  :: forall rIn e rOut
   . Layer rIn e rOut
  -> RIO (scope :: Scope | rIn) e (Record rOut)
unLayer (Layer rio) = rio

-- | Sequentially compose two layers: the first layer's output becomes
-- | the second layer's input. Both layers run in the same surrounding
-- | scope, so finalizers from either fire (in LIFO order) when that
-- | scope exits.
-- |
-- | If the first layer fails the second never runs; if either fails
-- | the failure propagates unchanged on the (shared) error row.
andThen
  :: forall rIn rMid rOut e
   . Layer rIn e rMid
  -> Layer rMid e rOut
  -> Layer rIn e rOut
andThen (Layer first) (Layer second) = Layer $ RIO \env -> do
  res1 <- unRIO first env
  case res1 of
    Left v -> pure (Left v)
    Right rMidRec -> do
      let
        scope :: Scope
        scope = unsafeGet "scope" env
        env' = unsafeSet "scope" scope rMidRec
      unRIO second env'

infixr 1 andThen as >>>

-- | Horizontally combine two layers with the same input requirements
-- | into one whose output row is the union of both. Both layers run in
-- | the same surrounding scope; their finalizers join the scope's
-- | stack and fire LIFO on exit.
-- |
-- | The two output rows must be disjoint. Sharing a label produces an
-- | ill-formed combined row and the compiler will reject the call.
combine
  :: forall rIn e r1Out r2Out rOut
   . Row.Union r1Out r2Out rOut
  => Layer rIn e r1Out
  -> Layer rIn e r2Out
  -> Layer rIn e rOut
combine (Layer l1) (Layer l2) = Layer $ RIO \env -> do
  res1 <- unRIO l1 env
  case res1 of
    Left v -> pure (Left v)
    Right r1Rec -> do
      res2 <- unRIO l2 env
      case res2 of
        Left v -> pure (Left v)
        Right r2Rec -> pure (Right (Record.union r1Rec r2Rec))

infixr 7 combine as <+>

-- | Run a closed layer (input row `()`) in a fresh scope and hand
-- | back its produced record.
-- |
-- | The scope is opened and closed inside `buildLayer`, so any
-- | finalizers the layer registered fire *before* this function
-- | returns. That makes `buildLayer` appropriate for stateless test
-- | layers (a logger, a static config) but unsafe for resource-owning
-- | layers, whose returned services would reference released
-- | resources. Reach for `provideLayer` for the resource-safe path.
buildLayer
  :: forall e rOut
   . Layer () e rOut
  -> Aff (Either (Variant e) (Record rOut))
buildLayer (Layer rio) = unRIO (scoped rio) {}

-- | Plumb a layer into a program: build the layer's services,
-- | feed them as the inner program's environment, and run the
-- | program. Layer-build failures and program failures are unioned
-- | into a single output error row.
-- |
-- | One scope spans the entire call: any finalizers the layer
-- | registers run after the inner program completes, on every
-- | termination path. This is what makes layers resource-safe.
-- | (See `spikes/aff-interruption/FINDINGS.md` scenario S6 for the
-- | underlying cancellation guarantee.)
-- |
-- | The error-row union is the shape the Phase 0.4 row-inference
-- | spike recommended re-confirming in Phase 5.3; it works as
-- | predicted. The forward direction (`Variant e -> Variant eOut`)
-- | is handled by `Data.Variant.expand` against the supplied
-- | `Union e e' eOut`; the program-side direction (`Variant e' ->
-- | Variant eOut`) is `unsafeCoerce` because PureScript's row
-- | solver can't recover the symmetric `Union e' e eOut` instance
-- | from the user-supplied one. The cast is safe at runtime:
-- | `expand` itself is `unsafeCoerce`, and the constraint already
-- | proves every label of `e'` is present in `eOut`.
provideLayer
  :: forall rIn rOut e e' eOut a
   . Row.Union e e' eOut
  => Layer rIn e rOut
  -> RIO rOut e' a
  -> RIO rIn eOut a
provideLayer (Layer layerRio) program = RIO \rInRec ->
  bracket
    (liftEffect (Ref.new []))
    ( \ref -> do
        fins <- liftEffect (Ref.read ref)
        foldr (\fin acc -> attempt fin *> acc) (pure unit) fins
    )
    ( \ref -> do
        let
          scope = Scope ref
          envWithScope = unsafeSet "scope" scope rInRec
        layerRes <- unRIO layerRio envWithScope
        case layerRes of
          Left v -> pure (Left (Variant.expand v))
          Right outRec -> do
            progRes <- unRIO program outRec
            case progRes of
              Left v -> pure (Left ((unsafeCoerce :: Variant e' -> Variant eOut) v))
              Right a -> pure (Right a)
    )
