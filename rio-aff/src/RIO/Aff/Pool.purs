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
-- | `shutdown` exposes the same drain action as an explicit
-- | call. Useful for "shut this pool down now, before the scope
-- | exits" patterns (e.g., a reload that swaps the pool out).
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
-- | ## Idle TTL
-- |
-- | `makeWithTTL` extends `make` with a per-resource idle TTL.
-- | An idle resource whose age exceeds the TTL is destroyed
-- | (rather than handed out) on the next borrow attempt, so a
-- | quiet pool self-recycles instead of holding stale handles
-- | indefinitely. The TTL clock is read from `RIO.Aff.Clock`, so
-- | a virtual clock makes the timing deterministic in tests.
-- |
-- | ## Per-borrow invalidation
-- |
-- | `withResource'` is the same as `withResource` but exposes a
-- | per-borrow `invalidate` action; calling it inside the body
-- | marks the resource as bad so the pool runs `release` on it
-- | when the body exits instead of returning it to the idle
-- | stack. Useful for "discard the connection if the call
-- | failed" patterns.
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
module RIO.Aff.Pool
  ( Pool
  , PoolConfig
  , PoolConfigWithTTL
  , make
  , makeWithTTL
  , makeWithTimeSource
  , withResource
  , withResource'
  , shutdown
  , size
  , idle
  , maxSize
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, finally)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Effect.Ref as ERef
import Type.Proxy (Proxy(..))

import RIO.Aff.Clock (Clock)
import RIO.Aff.Core (ask)
import RIO.Aff.Internal (RIO, mkEffectRIO, mkRIO, unsafeUnRIO)
import RIO.Aff.Resource (Scope, addFinalizer)
import RIO.Aff.Semaphore (Semaphore)
import RIO.Aff.Semaphore as Semaphore

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

-- | Configuration for `makeWithTTL`. Same shape as `PoolConfig`
-- | with an extra `timeToLive`: idle resources older than this
-- | are destroyed on the next borrow attempt instead of being
-- | recycled.
type PoolConfigWithTTL a =
  { acquire :: Aff a
  , release :: a -> Aff Unit
  , maxSize :: Int
  , timeToLive :: Milliseconds
  }

-- | An idle entry: the resource itself plus the wall-clock time
-- | at which it was returned to the pool. The timestamp is only
-- | consulted when the pool has a TTL configured.
type Entry a =
  { item :: a
  , addedAt :: Milliseconds
  }

-- | An opaque resource pool. Construct with `make` or
-- | `makeWithTTL`.
newtype Pool a = Pool
  { sem :: Semaphore
  , idleRef :: ERef.Ref (Array (Entry a))
  , totalRef :: ERef.Ref Int
  , acquire :: Aff a
  , release :: a -> Aff Unit
  , shutdownRef :: ERef.Ref Boolean
  , capacity :: Int
  , ttl :: Maybe Milliseconds
  , nowSource :: Aff Milliseconds
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
  pool <- mkEffectRIO \_ -> do
    sem <- Semaphore.make config.maxSize
    idleRef <- ERef.new []
    totalRef <- ERef.new 0
    shutdownRef <- ERef.new false
    pure
      ( Pool
          { sem
          , idleRef
          , totalRef
          , acquire: config.acquire
          , release: config.release
          , shutdownRef
          , capacity: max 0 config.maxSize
          , ttl: Nothing
          , nowSource: pure (Milliseconds 0.0)
          }
      )
  addFinalizer scope (drainAll pool)
  pure pool

-- | Like `make`, but idle resources whose age exceeds
-- | `timeToLive` are destroyed (and a fresh one minted) on the
-- | next borrow attempt. The wall-clock comparison reads from
-- | the active `Clock` service, so a virtual clock makes the
-- | timing deterministic in tests.
makeWithTTL
  :: forall r e a
   . Scope
  -> PoolConfigWithTTL a
  -> RIO (clock :: Clock | r) e (Pool a)
makeWithTTL scope config = do
  clock <- ask (Proxy :: Proxy "clock")
  makeWithTimeSource scope
    { acquire: config.acquire
    , release: config.release
    , maxSize: config.maxSize
    , timeToLive: config.timeToLive
    , nowSource: clock.now
    }

-- | Lower-level variant of `makeWithTTL` that takes the wall-clock
-- | source as an explicit `Aff` action instead of pulling the
-- | `Clock` service out of the row. Useful when a wrapper (e.g.
-- | `KeyedPool.makeWithTTL`) has already resolved the clock once
-- | and wants to feed every per-key pool the same time source
-- | without re-requiring `Clock` in the row at each borrow site.
makeWithTimeSource
  :: forall r e a
   . Scope
  -> { acquire :: Aff a
     , release :: a -> Aff Unit
     , maxSize :: Int
     , timeToLive :: Milliseconds
     , nowSource :: Aff Milliseconds
     }
  -> RIO r e (Pool a)
makeWithTimeSource scope config = do
  pool <- mkEffectRIO \_ -> do
    sem <- Semaphore.make config.maxSize
    idleRef <- ERef.new []
    totalRef <- ERef.new 0
    shutdownRef <- ERef.new false
    pure
      ( Pool
          { sem
          , idleRef
          , totalRef
          , acquire: config.acquire
          , release: config.release
          , shutdownRef
          , capacity: max 0 config.maxSize
          , ttl: Just config.timeToLive
          , nowSource: config.nowSource
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
withResource pool use = withResource' pool (\r _ -> use r)

-- | Like `withResource`, but the body also receives an
-- | `invalidate` action. Calling it marks the resource as bad:
-- | when the body returns, the pool runs `release` on the
-- | resource instead of pushing it back onto the idle stack.
-- | The next borrow has to mint a fresh one.
-- |
-- | Useful for "discard the connection on a failed call":
-- |
-- | ```purescript
-- | withResource' pool \conn invalidate -> do
-- |   r <- runQuery conn `catchAll` \e -> invalidate *> rethrow e
-- |   pure r
-- | ```
withResource'
  :: forall r e a b
   . Pool a
  -> (a -> RIO r e Unit -> RIO r e b)
  -> RIO r e b
withResource' pool@(Pool p) use = Semaphore.withPermit p.sem do
  shut <- mkEffectRIO \_ -> ERef.read p.shutdownRef
  when shut do
    mkRIO \_ -> liftAff
      (liftEffect (throwException (error "RIO.Aff.Pool: pool is shut down")))
  resource <- borrowOne pool
  invalidRef <- mkEffectRIO \_ -> ERef.new false
  let
    invalidate :: forall r' e'. RIO r' e' Unit
    invalidate = mkEffectRIO \_ -> ERef.write true invalidRef
  mkRIO \r -> do
    let
      returnIt = do
        invalidated <- liftEffect (ERef.read invalidRef)
        nowShut <- liftEffect (ERef.read p.shutdownRef)
        if invalidated || nowShut then do
          p.release resource
          liftEffect (ERef.modify_ (_ - 1) p.totalRef)
        else do
          addedAt <- p.nowSource
          liftEffect
            ( ERef.modify_
                (\xs -> Array.snoc xs { item: resource, addedAt })
                p.idleRef
            )
    finally returnIt (unsafeUnRIO (use resource invalidate) r)

-- | Explicit, eager shutdown. Releases every currently idle
-- | resource and flips the shutdown flag so subsequent
-- | `withResource` calls reject with a defect. Borrowers in
-- | flight keep their resource until they return it; that
-- | return then releases instead of recycling.
-- |
-- | Calling `shutdown` more than once is safe; later calls
-- | see an empty idle stack and the flag already set.
-- |
-- | `shutdown` runs anyway when the owning scope exits; this
-- | function just exposes the same drain action for callers
-- | that need it sooner.
shutdown :: forall r e a. Pool a -> RIO r e Unit
shutdown pool = mkRIO \_ -> drainAll pool

-- Pop the most-recently-returned idle entry; honour the TTL by
-- discarding expired entries (releasing each one) until either
-- a fresh-enough entry is found or the idle stack is empty.
-- When the stack runs dry, mint a fresh resource via `acquire`.
borrowOne :: forall r e a. Pool a -> RIO r e a
borrowOne (Pool p) = case p.ttl of
  Nothing -> mkRIO \_ -> do
    idleNow <- liftEffect (ERef.read p.idleRef)
    case Array.unsnoc idleNow of
      Just { init, last } -> do
        liftEffect (ERef.write init p.idleRef)
        pure last.item
      Nothing -> do
        a <- p.acquire
        liftEffect (ERef.modify_ (_ + 1) p.totalRef)
        pure a
  Just (Milliseconds ttlMs) -> mkRIO \_ -> do
    Milliseconds nowMs <- p.nowSource
    let
      loop = do
        idleNow <- liftEffect (ERef.read p.idleRef)
        case Array.unsnoc idleNow of
          Just { init, last } -> do
            liftEffect (ERef.write init p.idleRef)
            let Milliseconds added = last.addedAt
            if (nowMs - added) <= ttlMs then pure last.item
            else do
              p.release last.item
              liftEffect (ERef.modify_ (_ - 1) p.totalRef)
              loop
          Nothing -> do
            a <- p.acquire
            liftEffect (ERef.modify_ (_ + 1) p.totalRef)
            pure a
    loop

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
  releaseEach :: Array (Entry a) -> Aff Unit
  releaseEach xs = case Array.uncons xs of
    Nothing -> pure unit
    Just { head, tail } -> do
      p.release head.item
      releaseEach tail
