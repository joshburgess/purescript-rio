-- | A multi-producer / single-consumer drop-box. A `Mailbox` is a
-- | bounded `Queue` of values paired with a producer reference count.
-- |
-- | Producers `offer` values. When a producer is finished, it calls
-- | `done`. Once every producer has called `done`, the consumer's
-- | next `take` (after draining any remaining buffered values) yields
-- | `Nothing`, signalling end of stream.
-- |
-- | This is a focused replacement for the ad-hoc `Queue (Maybe a)`
-- | sentinel pattern that shows up wherever multiple fibers fan
-- | results into a single consumer (`Stream.merge`, `mergeAll`,
-- | `mapRIOPar`, etc.). Centralising the producer-count bookkeeping
-- | here keeps those call sites from re-implementing it.
-- |
-- | A Mailbox has a fixed declared producer count at construction;
-- | the consumer side trusts the count and the `done` signals from
-- | producers.
module RIO.Fiber.Mailbox
  ( Mailbox
  , make
  , unbounded
  , offer
  , done
  , take
  , size
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q

-- | A buffered fan-in channel. Carries values of type `a`; closes
-- | (returning `Nothing` from `take`) once every declared producer
-- | has called `done`.
newtype Mailbox a = Mailbox
  { queue :: Queue (Maybe a)
  , remaining :: Ref Int
  }

-- | Allocate a bounded mailbox. `capacity` is the underlying queue
-- | capacity (offers suspend on a full buffer); `producers` is the
-- | declared number of fibers that will call `done` to signal their
-- | individual completion. Both are clamped to at least `1`.
make :: forall a. Int -> Int -> Effect (Mailbox a)
make capacity producers = do
  queue <- Q.make capacity
  remaining <- Ref.new (max 1 producers)
  pure (Mailbox { queue, remaining })

-- | Like `make` but with an unbounded underlying queue: `offer` never
-- | suspends. Useful when the producers can't safely block (e.g. they
-- | are running on a single dispatcher fiber).
unbounded :: forall a. Int -> Effect (Mailbox a)
unbounded producers = do
  queue <- Q.unbounded
  remaining <- Ref.new (max 1 producers)
  pure (Mailbox { queue, remaining })

-- | Enqueue a value. Suspends if the underlying queue is at capacity
-- | (with `make`) or returns immediately (with `unbounded`).
offer :: forall r e a. Mailbox a -> a -> RIO r e Unit
offer (Mailbox m) a = Q.offer m.queue (Just a)

-- | Signal that one producer has finished. When the last producer
-- | calls `done`, a terminal `Nothing` is enqueued so the consumer
-- | eventually observes end-of-stream after draining buffered values.
-- | Extra `done` calls past the declared producer count are no-ops.
done :: forall r e a. Mailbox a -> RIO r e Unit
done (Mailbox m) = do
  remaining <- liftEffect (Ref.modify (\n -> n - 1) m.remaining)
  if remaining <= 0 then Q.offer m.queue Nothing
  else pure unit

-- | Take the next value, or `Nothing` if the mailbox has been closed
-- | by all producers and the buffer has been drained. After the first
-- | `Nothing`, every subsequent `take` also returns `Nothing`.
take :: forall r e a. Mailbox a -> RIO r e (Maybe a)
take (Mailbox m) = do
  msg <- Q.take m.queue
  case msg of
    Just a -> pure (Just a)
    Nothing -> do
      -- Re-post the terminal so any further takes (or a second
      -- consumer) also observe end-of-stream.
      Q.offer m.queue Nothing
      pure Nothing

-- | Current number of buffered values (does not count the terminal
-- | `Nothing` once posted).
size :: forall r e a. Mailbox a -> RIO r e Int
size (Mailbox m) = Q.size m.queue
