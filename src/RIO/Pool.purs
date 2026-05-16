-- | A generic resource pool.
-- |
-- | `Pool a` holds a bounded set of reusable resources of type
-- | `a` (database connections, HTTP clients, file handles, etc.).
-- | `withResource` borrows a resource for the dynamic extent of
-- | its body and returns it to the pool when the body exits.
-- |
-- | A pool caps simultaneous borrowers at its configured
-- | `maxSize` via an internal `Semaphore`. When every permit is
-- | in use, additional `withResource` calls block until a permit
-- | is returned. Idle resources accumulate in an internal stack
-- | and are reused on subsequent borrows; only when the stack is
-- | empty does a borrow trigger `acquire` to mint a new resource.
-- |
-- | ## Lifecycle: tied to a `Scope`
-- |
-- | Pools are created inside a `Scope`. When the scope exits
-- | (success, typed failure, defect, or fiber kill), the pool is
-- | shut down: every idle resource has `release` called on it,
-- | and any subsequent `withResource` call rejects with a defect.
-- | Resources still in flight when the scope exits are released
-- | when their borrowing block finishes, since the post-use
-- | "return to pool" step observes the shutdown flag and calls
-- | `release` instead of returning the resource to the idle
-- | stack.
-- |
-- | ```purescript
-- | -- a pool of 8 DB connections; close them all when `program`
-- | -- exits, no matter how
-- | program = scoped do
-- |   scope <- ask (Proxy :: Proxy "scope")
-- |   pool <- Pool.make scope
-- |     { acquire: openConnection
-- |     , release: closeConnection
-- |     , maxSize: 8
-- |     }
-- |   parTraverse (Pool.withResource pool runQuery) tasks
-- | ```
-- |
-- | ## Semantics: stack, not FIFO
-- |
-- | Returned resources are pushed onto the idle stack and the
-- | next borrow pops from the top. This is LIFO ("MRU-first") on
-- | purpose: a hot connection that was just returned is likely
-- | to still be warm at the next borrow. If you need fairness or
-- | FIFO eviction, build it as a wrapper around `withResource`
-- | (e.g., using a `Queue` instead of a `Ref (Array a)` for the
-- | idle store).
module RIO.Pool
  ( Pool
  , PoolConfig
  , make
  , withResource
  , size
  , idle
  , maxSize
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, finally)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Effect.Ref as ERef

import RIO.Internal (RIO(..), unsafeUnRIO)
import RIO.Resource (Scope, addFinalizer)
import RIO.Semaphore (Semaphore)
import RIO.Semaphore as Semaphore

-- | Configuration for `make`.
-- |
-- | * `acquire` mints a new resource. Runs in `Aff` so it can
-- |   block on network/file I/O.
-- | * `release` returns a resource to the underlying system
-- |   (close the socket, close the file). It runs uninterruptibly
-- |   on scope exit and on per-borrow eviction.
-- | * `maxSize` caps the number of resources alive at once.
type PoolConfig a =
  { acquire :: Aff a
  , release :: a -> Aff Unit
  , maxSize :: Int
  }

-- | An opaque resource pool. Construct with `make`.
newtype Pool a = Pool
  { sem :: Semaphore
  , idleRef :: ERef.Ref (Array a)
  , totalRef :: ERef.Ref Int
  , acquire :: Aff a
  , release :: a -> Aff Unit
  , shutdownRef :: ERef.Ref Boolean
  , capacity :: Int
  }

-- | Construct a pool tied to `scope`. When `scope` exits, every
-- | idle resource is released; resources in flight at that
-- | moment are released when their `withResource` body returns.
-- |
-- | The error row is left polymorphic; pool construction itself
-- | does not signal typed failures.
make
  :: forall r e a
   . Scope
  -> PoolConfig a
  -> RIO r e (Pool a)
make scope config = do
  pool <- RIO \_ -> do
    sem <- liftEffect (Semaphore.make config.maxSize)
    idleRef <- liftEffect (ERef.new [])
    totalRef <- liftEffect (ERef.new 0)
    shutdownRef <- liftEffect (ERef.new false)
    pure
      ( Pool
          { sem
          , idleRef
          , totalRef
          , acquire: config.acquire
          , release: config.release
          , shutdownRef
          , capacity: max 0 config.maxSize
          }
      )
  addFinalizer scope (drainAll pool)
  pure pool

-- | The pool's configured `maxSize`.
maxSize :: forall a. Pool a -> Int
maxSize (Pool p) = p.capacity

-- | The number of resources currently alive (idle + in-flight).
-- | Advisory; the value can change concurrently.
size :: forall a. Pool a -> Effect Int
size (Pool p) = ERef.read p.totalRef

-- | The number of resources currently idle (returned to the
-- | pool, ready to be borrowed). Advisory.
idle :: forall a. Pool a -> Effect Int
idle (Pool p) = Array.length <$> ERef.read p.idleRef

-- | Borrow a resource for the dynamic extent of `use`. The
-- | resource is returned to the pool on every termination path
-- | of `use` (success, typed failure, defect, fiber kill). If
-- | the pool is shut down between borrow and return, the
-- | resource is released via `config.release` instead.
-- |
-- | Borrows block when every permit is in flight; the wait
-- | itself is interruptible.
withResource
  :: forall r e a b
   . Pool a
  -> (a -> RIO r e b)
  -> RIO r e b
withResource pool@(Pool p) use = Semaphore.withPermit p.sem do
  -- Check shutdown before allocating.
  shut <- RIO \_ -> liftEffect (ERef.read p.shutdownRef)
  when shut do
    RIO \_ -> liftAff
      (liftEffect (throwException (error "RIO.Pool: pool is shut down")))
  resource <- borrowOne pool
  RIO \r -> do
    let
      returnIt = do
        nowShut <- liftEffect (ERef.read p.shutdownRef)
        if nowShut then do
          p.release resource
          liftEffect (ERef.modify_ (_ - 1) p.totalRef)
        else
          liftEffect
            (ERef.modify_ (\xs -> Array.snoc xs resource) p.idleRef)
    finally returnIt (unsafeUnRIO (use resource) r)

-- Pop an idle resource if available; otherwise acquire a fresh
-- one. Increments `totalRef` when a fresh resource is minted.
borrowOne :: forall r e a. Pool a -> RIO r e a
borrowOne (Pool p) = RIO \_ -> do
  idleNow <- liftEffect (ERef.read p.idleRef)
  case Array.unsnoc idleNow of
    Just { init, last } -> do
      liftEffect (ERef.write init p.idleRef)
      pure last
    Nothing -> do
      a <- p.acquire
      liftEffect (ERef.modify_ (_ + 1) p.totalRef)
      pure a

-- Drain every idle resource, releasing it. Set the shutdown
-- flag so in-flight resources release on return rather than
-- re-entering the idle stack. Decrement the total counter so
-- `size` reflects the drained idle resources.
drainAll :: forall a. Pool a -> Aff Unit
drainAll (Pool p) = do
  liftEffect (ERef.write true p.shutdownRef)
  drained <- liftEffect do
    xs <- ERef.read p.idleRef
    ERef.write [] p.idleRef
    pure xs
  releaseEach drained
  liftEffect (ERef.modify_ (\n -> n - Array.length drained) p.totalRef)
  where
  releaseEach :: Array a -> Aff Unit
  releaseEach xs = case Array.uncons xs of
    Nothing -> pure unit
    Just { head, tail } -> do
      p.release head
      releaseEach tail
