-- | True per-fiber reference cells.
-- |
-- | A `FiberRef a` is a typed cell whose value is *private to the
-- | reading fiber*: a child fiber inherits its parent's value at
-- | fork time but subsequent writes from either side do not bleed
-- | into the other. This is the semantic ZIO's `FiberRef` and
-- | Effect-TS's `FiberRef` ship; `RIO.Local` is the simpler
-- | shared-Ref model with scoped overrides.
-- |
-- | The implementation is a per-fiber storage map carried in the
-- | environment row under the `fiberRefs` label. `make`, `get`,
-- | and `set` consult the calling fiber's map; `forkFiber` clones
-- | the map (snapshotting every cell) before forking. The clone
-- | is shallow: each entry's value is copied into a fresh `Ref`,
-- | so the parent's later writes do not affect the child and
-- | vice versa.
-- |
-- | ## When to reach for this versus Local
-- |
-- | Use `Local` when:
-- |
-- |   * The value is set once at entry and read by every
-- |     downstream piece of work (typical for a request id, a
-- |     correlation token, a tenant tag).
-- |   * You want the simplest shape with no fork-time machinery.
-- |
-- | Use `FiberRef` when:
-- |
-- |   * A child fiber must mutate its own copy without affecting
-- |     the parent.
-- |   * You want ZIO-style snapshot-on-fork semantics.
-- |
-- | ```purescript
-- | -- Parent and child each have their own counter.
-- | program = do
-- |   counter <- FiberRef.make 0
-- |   fib <- FiberRef.forkFiber do
-- |     FiberRef.set counter 100
-- |     FiberRef.get counter   -- 100 (child's view)
-- |   childResult <- join fib
-- |   parentValue <- FiberRef.get counter   -- 0 (parent untouched)
-- |   pure { childResult, parentValue }
-- | ```
module RIO.FiberRef
  ( FiberRef
  , FiberRefs
  , newFiberRefs
  , newFiberRefsEffect
  , make
  , get
  , set
  , update
  , forkFiber
  , forkFiberScoped
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

import RIO.Concurrency (Fiber(..), mkFiber)
import RIO.Env (ask) as Env
import RIO.Internal (RIO(..), unRIO)
import RIO.Resource (Scope, addFinalizer)

-- | A type-erased `Ref` cell, recovered by the carrying
-- | `FiberRef`'s witness at read time.
foreign import data AnyRef :: Type

eraseRef :: forall a. Ref a -> AnyRef
eraseRef = unsafeCoerce

reifyRef :: forall a. AnyRef -> Ref a
reifyRef = unsafeCoerce

-- | The per-fiber storage map. One copy per fiber; carried in the
-- | environment row as the `fiberRefs` service so every read /
-- | write knows which fiber's view to consult.
newtype FiberRefs = FiberRefs (Ref (Map Int AnyRef))

-- | Allocate a fresh, empty `FiberRefs`. Use this once per
-- | top-level `RIO` program (typically in `main`) before
-- | `provide`ing it as the `fiberRefs` service.
newFiberRefs :: forall r e. RIO r e FiberRefs
newFiberRefs = RIO \_ -> do
  ref <- liftEffect (Ref.new (Map.empty :: Map Int AnyRef))
  pure (Right (FiberRefs ref))

-- | `Effect`-typed variant for callers building the environment
-- | outside an `RIO` action (e.g. at the top of `main`).
newFiberRefsEffect :: Effect FiberRefs
newFiberRefsEffect = FiberRefs <$> Ref.new (Map.empty :: Map Int AnyRef)

-- | A typed cell identified by an `Int` key.
newtype FiberRef a = FiberRef { key :: Int, default :: a }

-- | Module-local counter for minting unique keys. The cell holds
-- | a monotonically bumped `Int`; there is no ordering hazard
-- | because no `Int` value is ever observed under another key.
keySource :: Ref Int
keySource = unsafePerformEffect (Ref.new 0)

nextKey :: Effect Int
nextKey = Ref.modify (_ + 1) keySource

-- | Allocate a fresh `FiberRef` with the given initial value.
-- |
-- | The cell starts present in the *calling* fiber's storage.
-- | Other fibers (in particular fibers forked *before* this
-- | `make` call) do not see the cell; reads from those fibers
-- | return the `default`.
make
  :: forall r e a
   . a
  -> RIO (fiberRefs :: FiberRefs | r) e (FiberRef a)
make initial = do
  FiberRefs storage <- Env.ask (Proxy :: Proxy "fiberRefs")
  RIO \_ -> do
    key <- liftEffect nextKey
    ref <- liftEffect (Ref.new initial)
    liftEffect (Ref.modify_ (Map.insert key (eraseRef ref)) storage)
    pure (Right (FiberRef { key, default: initial }))

-- | Read the calling fiber's value of the cell.
get
  :: forall r e a
   . FiberRef a
  -> RIO (fiberRefs :: FiberRefs | r) e a
get (FiberRef { key, default }) = do
  FiberRefs storage <- Env.ask (Proxy :: Proxy "fiberRefs")
  RIO \_ -> do
    m <- liftEffect (Ref.read storage)
    case Map.lookup key m of
      Just anyRef -> do
        v <- liftEffect (Ref.read (reifyRef anyRef))
        pure (Right v)
      Nothing -> pure (Right default)

-- | Overwrite the calling fiber's value of the cell. If the cell
-- | does not yet have an entry in this fiber's storage (because
-- | the fiber inherited a snapshot that did not include this
-- | cell, or no `set`/`make` has touched it here), one is
-- | allocated.
set
  :: forall r e a
   . FiberRef a
  -> a
  -> RIO (fiberRefs :: FiberRefs | r) e Unit
set (FiberRef { key }) value = do
  FiberRefs storage <- Env.ask (Proxy :: Proxy "fiberRefs")
  RIO \_ -> do
    m <- liftEffect (Ref.read storage)
    case Map.lookup key m of
      Just anyRef -> do
        liftEffect (Ref.write value (reifyRef anyRef))
        pure (Right unit)
      Nothing -> do
        ref <- liftEffect (Ref.new value)
        liftEffect (Ref.modify_ (Map.insert key (eraseRef ref)) storage)
        pure (Right unit)

-- | Apply a pure function to the calling fiber's value and store
-- | the result. Equivalent to `get` followed by `set`.
update
  :: forall r e a
   . FiberRef a
  -> (a -> a)
  -> RIO (fiberRefs :: FiberRefs | r) e Unit
update fr f = do
  v <- get fr
  set fr (f v)

-- | Fork a child fiber with a snapshot copy of the parent's
-- | `FiberRefs`. Every cell present in the parent's storage at
-- | the fork moment is cloned into a fresh `Ref` in the child's
-- | storage; subsequent writes on either side stay local to
-- | their fiber.
-- |
-- | The child runs with the same environment record as the
-- | parent except for the `fiberRefs` slot, which is replaced
-- | with the snapshot.
forkFiber
  :: forall r e e' a
   . RIO (fiberRefs :: FiberRefs | r) e a
  -> RIO (fiberRefs :: FiberRefs | r) e' (Fiber e a)
forkFiber inner = do
  FiberRefs parent <- Env.ask (Proxy :: Proxy "fiberRefs")
  RIO \r -> do
    childStorage <- liftEffect (snapshotStorage parent)
    let childEnv = unsafeSet "fiberRefs" (FiberRefs childStorage) r
    fib <- mkFiber (unRIO inner childEnv)
    pure (Right fib)

-- | Scope-bounded variant of `forkFiber`: the child's lifetime is
-- | bounded by the supplied `Scope`, just like
-- | `RIO.Concurrency.forkScoped`.
forkFiberScoped
  :: forall r e e' a
   . Scope
  -> RIO (fiberRefs :: FiberRefs | r) e a
  -> RIO (fiberRefs :: FiberRefs | r) e' (Fiber e a)
forkFiberScoped scope inner = do
  FiberRefs parent <- Env.ask (Proxy :: Proxy "fiberRefs")
  RIO \r -> do
    childStorage <- liftEffect (snapshotStorage parent)
    let childEnv = unsafeSet "fiberRefs" (FiberRefs childStorage) r
    fib@(Fiber f) <- mkFiber (unRIO inner childEnv)
    let
      cleanup = Aff.killFiber
        (Aff.error "RIO.forkFiberScoped: scope exit")
        f.underlying
    _ <- unRIO (addFinalizer scope cleanup) r
    pure (Right fib)

-- | Clone every entry in the source storage into a fresh `Ref`
-- | holding the same value. The clone is per-entry; reads and
-- | writes on the result are independent of the source.
snapshotStorage
  :: Ref (Map Int AnyRef)
  -> Effect (Ref (Map Int AnyRef))
snapshotStorage src = do
  parentMap <- Ref.read src
  let entries = Map.toUnfoldable parentMap :: Array (Tuple Int AnyRef)
  cloned <- traverse cloneEntry entries
  Ref.new (Map.fromFoldable cloned)

-- | Clone a single erased `Ref` entry. The contents are treated
-- | as an opaque value; `Ref.read` + `Ref.new` preserves whatever
-- | type round-trips through the erase / reify pair.
cloneEntry :: Tuple Int AnyRef -> Effect (Tuple Int AnyRef)
cloneEntry (Tuple key anyRef) = do
  let (src :: Ref Int) = reifyRef anyRef
  v <- Ref.read src
  fresh <- Ref.new v
  pure (Tuple key (eraseRef fresh))
