-- | Pull-based asynchronous streams.
-- |
-- | A `Stream r e a` is built around a single primitive: pull the
-- | next step. Each pull runs in `RIO r e` and either yields the
-- | head plus the continuation, or signals end-of-stream. This keeps
-- | the implementation small while supporting both bounded and
-- | unbounded producers, backpressure (each pull is explicit), and
-- | failure (the pull RIO can raise typed errors or defects).
-- |
-- | The MVP ships construction helpers (`empty`, `emit`, `fromArray`,
-- | `repeatRIO`, `fromQueue`), the obvious transforms (`map`,
-- | `filter`, `take`), and three terminations (`run`, `runCollect`,
-- | `fold`). Concurrency-shaped operators (merge, zipPar, channel)
-- | are left to later phases.
module RIO.Fiber.Stream
  ( Stream(..)
  , Step(..)
  , empty
  , emit
  , fromArray
  , repeatRIO
  , fromQueue
  , map
  , filter
  , take
  , fold
  , forEach
  , run
  , runCollect
  ) where

import Prelude hiding (map)

import Data.Array (snoc, uncons)
import Data.Maybe (Maybe(..))
import RIO.Fiber.Core (RIO)
import RIO.Fiber.Queue (Queue)
import RIO.Fiber.Queue as Q

-- | A stream is a producer of `Step`s. Each pull is an `RIO` that
-- | yields either the next element + continuation or `Done`.
newtype Stream r e a = Stream (RIO r e (Step r e a))

-- | A single step in the pull protocol.
data Step r e a
  = Done
  | Yield a (Stream r e a)

-- | The empty stream. The first pull immediately signals `Done`.
empty :: forall r e a. Stream r e a
empty = Stream (pure Done)

-- | A stream that yields exactly one element, then halts.
emit :: forall r e a. a -> Stream r e a
emit a = Stream (pure (Yield a empty))

-- | A stream that yields each element of the array in order, then halts.
fromArray :: forall r e a. Array a -> Stream r e a
fromArray xs = Stream
  ( pure case uncons xs of
      Nothing -> Done
      Just { head, tail } -> Yield head (fromArray tail)
  )

-- | A stream that yields the result of running `action` on every
-- | pull. Infinite: bound it with `take` or short-circuit via
-- | interruption.
repeatRIO :: forall r e a. RIO r e a -> Stream r e a
repeatRIO action = Stream do
  a <- action
  pure (Yield a (repeatRIO action))

-- | A stream that pulls from the given queue. Each pull suspends
-- | until an element is available. Infinite: queues have no close
-- | signal, so bound externally.
fromQueue :: forall r e a. Queue a -> Stream r e a
fromQueue q = Stream do
  a <- Q.take q
  pure (Yield a (fromQueue q))

-- | Transform every element with `f`. Lazy: the function runs as
-- | elements are pulled.
map :: forall r e a b. (a -> b) -> Stream r e a -> Stream r e b
map f (Stream pull) = Stream do
  s <- pull
  pure case s of
    Done -> Done
    Yield a rest -> Yield (f a) (map f rest)

-- | Keep only elements satisfying `p`. Pulls the upstream stream
-- | until a matching element appears or it ends.
filter :: forall r e a. (a -> Boolean) -> Stream r e a -> Stream r e a
filter p (Stream pull) = Stream (loop pull)
  where
  loop next = do
    s <- next
    case s of
      Done -> pure Done
      Yield a (Stream rest)
        | p a -> pure (Yield a (filter p (Stream rest)))
        | otherwise -> loop rest

-- | Take at most `n` elements from the stream. Non-positive `n`
-- | becomes the empty stream.
take :: forall r e a. Int -> Stream r e a -> Stream r e a
take n s
  | n <= 0 = empty
  | otherwise = case s of
      Stream pull -> Stream do
        step <- pull
        pure case step of
          Done -> Done
          Yield a rest -> Yield a (take (n - 1) rest)

-- | Reduce the stream with `step`, starting from `seed`. Pulls
-- | until the stream signals `Done`.
fold :: forall r e a b. (b -> a -> b) -> b -> Stream r e a -> RIO r e b
fold step seed (Stream pull) = do
  s <- pull
  case s of
    Done -> pure seed
    Yield a rest -> fold step (step seed a) rest

-- | Run an `RIO` action for every element. Returns when the stream
-- | is exhausted.
forEach :: forall r e a. (a -> RIO r e Unit) -> Stream r e a -> RIO r e Unit
forEach f (Stream pull) = do
  s <- pull
  case s of
    Done -> pure unit
    Yield a rest -> do
      f a
      forEach f rest

-- | Drain the stream, discarding every element. Useful for streams
-- | run for their side effects.
run :: forall r e a. Stream r e a -> RIO r e Unit
run = forEach (\_ -> pure unit)

-- | Pull every element into an array. Use only on bounded streams.
runCollect :: forall r e a. Stream r e a -> RIO r e (Array a)
runCollect = fold snoc []
