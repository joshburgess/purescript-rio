-- | A scope-managed bag of running fibers with graceful drain.
-- |
-- | `FiberSet e a` is the many-fibers counterpart to `FiberHandle`:
-- | every call to `run` forks a fiber and adds it to the set. When
-- | the owning scope closes, every fiber still in the set is
-- | interrupted. Fibers that complete on their own are removed
-- | automatically. `awaitEmpty` suspends until every tracked fiber
-- | has finished, which gives a clean "drain my workers" handoff
-- | without manual `traverse join` over a hand-kept list.
-- |
-- | Use this when several short-lived background fibers share a
-- | lifetime (e.g. one fiber per incoming request, one per work
-- | item) and you need them all gone before the next phase starts.
-- | It replaces the manual `Ref (Array (Fiber e a))` + add-on-fork +
-- | remove-on-complete + interrupt-all-on-scope-close boilerplate.
module RIO.Aff.FiberSet
  ( FiberSet
  , awaitEmpty
  , interruptAll
  , make
  , run
  , size
  ) where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Aff (error, killFiber) as Aff
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

import RIO.Aff.Concurrency
  ( Fiber(..)
  , asyncInterrupt
  , await
  , fork
  , forkUntracked
  , interrupt
  , uninterruptible
  )
import RIO.Aff.Core (RIO)
import RIO.Aff.Resource (Scope, addFinalizer)

type Waiter = { id :: Int, fire :: Effect Unit }

type State e a =
  { fibers :: Map Int (Fiber e a)
  , nextId :: Int
  , waiters :: Array Waiter
  , nextWaiterId :: Int
  }

-- | A scope-managed bag of fibers. Auto-removes completed fibers;
-- | interrupts everything still alive when the scope closes.
newtype FiberSet e a = FiberSet (Ref (State e a))

-- | Allocate a fresh set owned by `scope`. When the scope closes,
-- | every tracked fiber is interrupted.
make :: forall r e e' a. Scope -> RIO r e' (FiberSet e a)
make scope = do
  ref <- liftEffect $ Ref.new
    { fibers: Map.empty
    , nextId: 0
    , waiters: []
    , nextWaiterId: 0
    }
  addFinalizer scope do
    st <- liftEffect (Ref.read ref)
    for_ st.fibers \(Fiber f) ->
      Aff.killFiber (Aff.error "FiberSet: scope closing") f.underlying
  pure (FiberSet ref)

-- | Fork `action` and add the fiber to the set. The fiber removes
-- | itself from the set on completion; if its removal empties the
-- | set, any `awaitEmpty` waiters resume.
-- |
-- | The fork + install runs in an uninterruptible region so an
-- | interrupt cannot strand the fresh fiber outside the set.
run
  :: forall r e a
   . FiberSet e a
  -> RIO r e a
  -> RIO r e (Fiber e a)
run (FiberSet ref) action = uninterruptible do
  fib <- fork action
  id <- liftEffect do
    st <- Ref.read ref
    let
      i = st.nextId
      st' = st
        { fibers = Map.insert i fib st.fibers
        , nextId = i + 1
        }
    Ref.write st' ref
    pure i
  _ <- forkUntracked do
    _ <- await fib
    liftEffect do
      st0 <- Ref.read ref
      let
        fibers' = Map.delete id st0.fibers
      if Map.isEmpty fibers' then do
        Ref.write
          (st0 { fibers = fibers', waiters = [] })
          ref
        for_ st0.waiters \w -> w.fire
      else
        Ref.write (st0 { fibers = fibers' }) ref
  pure fib

-- | The current number of tracked fibers.
size :: forall r e e' a. FiberSet e a -> RIO r e' Int
size (FiberSet ref) = liftEffect (Map.size <<< _.fibers <$> Ref.read ref)

-- | Suspend until the set is empty. Resumes immediately if the set
-- | is already empty at the time of the call. Multiple concurrent
-- | waiters are all resumed when the set drains.
awaitEmpty :: forall r e e' a. FiberSet e a -> RIO r e' Unit
awaitEmpty (FiberSet ref) = asyncInterrupt \cb -> do
  st <- Ref.read ref
  if Map.isEmpty st.fibers then do
    cb (Right unit)
    pure (pure unit)
  else do
    let
      id = st.nextWaiterId
      waiter = { id, fire: cb (Right unit) }
    Ref.write
      ( st
          { waiters = st.waiters <> [ waiter ]
          , nextWaiterId = id + 1
          }
      )
      ref
    pure
      ( Ref.modify_
          ( \s -> s
              { waiters = filter (\w -> w.id /= id) s.waiters }
          )
          ref
      )

-- | Interrupt every fiber currently in the set. Returns the count
-- | of fibers interrupted. The set itself stays open; subsequent
-- | `run` calls still attach.
interruptAll :: forall r e e' a. FiberSet e a -> RIO r e' Int
interruptAll (FiberSet ref) = do
  st <- liftEffect (Ref.read ref)
  for_ st.fibers interrupt
  pure (Map.size st.fibers)
