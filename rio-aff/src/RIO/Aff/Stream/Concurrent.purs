-- | Dynamic broadcast: one source, subscribe-on-demand consumers.
-- |
-- | `RIO.Aff.Stream.Par.broadcast` is the static fan-out shape: declare
-- | `n` consumers up front and receive `n` consumer streams. Every
-- | element flows through every consumer's bounded queue, and the
-- | slowest consumer applies backpressure to the producer.
-- |
-- | This module adds the dual shape, modelled on ZIO
-- | `ZStream.broadcastDynamic` / Effect-TS `Stream.broadcastDynamic`:
-- | the source is drained once into a hub, and a `subscribe` action
-- | is handed back to the caller. Every call to `subscribe` returns
-- | a fresh consumer stream that observes every value published
-- | while it is alive. Subscribers can come and go independently;
-- | the producer never knows how many of them are listening at a
-- | given moment.
-- |
-- | Trade-off vs `RIO.Aff.Stream.Par.broadcast`: subscribers each get an
-- | unbounded queue (the same model `RIO.Aff.Hub` uses), so a slow
-- | subscriber does not slow the producer or any sibling subscriber
-- | down, but a slow subscriber can fall arbitrarily far behind.
-- |
-- | The producer is scope-bound: it runs on a fiber forked into the
-- | enclosing `Scope`, so it cannot outlive the surrounding `scoped`
-- | block. Late subscribers (after the source has finished) see an
-- | empty stream that terminates immediately rather than blocking
-- | forever.
-- |
-- | ```purescript
-- | -- one source, two independent consumers, each running in its
-- | -- own fiber
-- | program = scoped do
-- |   subscribe <- broadcastDynamic source
-- |   c1 <- subscribe
-- |   c2 <- subscribe
-- |   Tuple xs ys <- zipPar (runCollect c1) (runCollect c2)
-- |   logInfo (show xs <> " / " <> show ys)
-- | ```
module RIO.Aff.Stream.Concurrent
  ( broadcastDynamic
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))

import RIO.Aff.Concurrency (forkScoped)
import RIO.Aff.Core (RIO)
import RIO.Aff.Env (ask)
import RIO.Aff.Error (rethrow)
import RIO.Aff.Error as Error
import RIO.Aff.Hub as Hub
import RIO.Aff.Queue as Queue
import RIO.Aff.Resource (Scope)
import RIO.Aff.Stream (Step(..), Stream(..), intoHub)

-- | Share a single source stream across many on-demand subscribers.
-- |
-- | Spawns one forked fiber that drains `source` into a fresh hub
-- | and shuts the hub down when the source is exhausted. The
-- | returned action subscribes to that hub: every call gives back a
-- | fresh `Stream` that observes every value published while it is
-- | alive.
-- |
-- | A subscriber attached after the source has already finished
-- | sees an empty, immediately-terminated stream rather than
-- | blocking forever. A subscriber that attaches partway through
-- | only sees values published after its subscription; earlier
-- | values are not replayed.
-- |
-- | If the source raises a typed failure, the failure is recorded
-- | and the hub is shut down. Every live subscriber observes the
-- | partial output through its queue first, and the next pull past
-- | the queue tail raises the recorded failure on the subscriber's
-- | row. Subscribers attached after the failure see the failure on
-- | their first pull.
-- |
-- | The publisher fiber is bound to the enclosing `Scope`, so it
-- | cannot outlive the surrounding `scoped` block.
broadcastDynamic
  :: forall r e e' a
   . Stream (scope :: Scope | r) e a
  -> RIO (scope :: Scope | r) e'
       (RIO (scope :: Scope | r) e' (Stream (scope :: Scope | r) e a))
broadcastDynamic source = do
  scope <- ask (Proxy :: Proxy "scope")
  hub <- liftEffect Hub.make
  errRef <- liftEffect (Ref.new Nothing)
  let
    publisher =
      Error.catchAll
        ( \v -> do
            liftEffect (Ref.write (Just v) errRef)
            Hub.shutdown hub
        )
        (intoHub hub source)
        *> Hub.shutdown hub
  _ <- forkScoped scope publisher
  pure do
    sub <- Hub.subscribe hub
    pure (subscribed sub.queue errRef)
  where
  subscribed q errRef = Stream do
    m <- Queue.take q
    case m of
      Just a -> pure (Yield a (subscribed q errRef))
      Nothing -> do
        err <- liftEffect (Ref.read errRef)
        case err of
          Just v -> rethrow v
          Nothing -> pure Done
