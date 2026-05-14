-- | A minimal pull-based stream.
-- |
-- | A `Stream r e a` is a `RIO r e`-flavoured iterator: each step
-- | either yields a value and a continuation, or signals
-- | end-of-stream. All operations are written as direct
-- | recursions on the step function rather than going through a
-- | free encoding, so traces stay readable and there is no fusion
-- | machinery to reason about.
-- |
-- | This is intentionally narrower than ZStream / Effect Stream:
-- | one input channel, no sinks, no parallel combinators, no
-- | resource-safe finalization beyond what the underlying `RIO r e`
-- | actions already give you. It is enough to demonstrate that
-- | streaming fits the framework cleanly, and to use in places
-- | like "pull rows from a Postgres cursor and write JSON to
-- | stdout" without rewriting `for_` over an `Array`.
-- |
-- | ```purescript
-- | -- pull every row out of a query result, transform, drain
-- | runDrain
-- |   ( fromArray [1, 2, 3, 4, 5]
-- |       # filter (\n -> n `mod` 2 == 0)
-- |       # map (_ * 10)
-- |       # mapM (\n -> logInfo (show n) *> pure n)
-- |   )
-- | ```
module RIO.Stream
  ( Stream(..)
  , Step(..)
  , unStream
  , concat
  , drop
  , empty
  , filter
  , flatMap
  , fromArray
  , map
  , mapM
  , repeatM
  , runCollect
  , runDrain
  , runFold
  , runFoldM
  , single
  , take
  , unfoldM
  ) where

import Prelude hiding (map)

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import RIO.Core (RIO)

-- | One step of a stream: either a value paired with the rest of
-- | the stream, or end-of-stream.
data Step r e a
  = Yield a (Stream r e a)
  | Done

-- | A pull-based stream. Run the inner `RIO` to get the next step.
-- |
-- | The constructor and `unStream` are exposed so that companion
-- | modules (e.g. `RIO.Stream.Par`) can build new combinators that
-- | need to step the underlying `RIO`. End-user code should reach
-- | for the combinators in this module rather than peeling the
-- | newtype directly.
newtype Stream :: Row Type -> Row Type -> Type -> Type
newtype Stream r e a = Stream (RIO r e (Step r e a))

unStream :: forall r e a. Stream r e a -> RIO r e (Step r e a)
unStream (Stream s) = s

-- | The empty stream.
empty :: forall r e a. Stream r e a
empty = Stream (pure Done)

-- | A single-element stream.
single :: forall r e a. a -> Stream r e a
single a = Stream (pure (Yield a empty))

-- | A stream of the elements of an array, in input order.
fromArray :: forall r e a. Array a -> Stream r e a
fromArray xs = case Array.uncons xs of
  Nothing -> empty
  Just { head, tail } -> Stream (pure (Yield head (fromArray tail)))

-- | Build a stream by repeatedly stepping a seed state. Each step
-- | either returns a new value plus the next seed, or `Nothing` to
-- | end the stream.
unfoldM
  :: forall r e s a
   . s
  -> (s -> RIO r e (Maybe (Tuple a s)))
  -> Stream r e a
unfoldM seed step = Stream do
  m <- step seed
  case m of
    Nothing -> pure Done
    Just (Tuple a seed') -> pure (Yield a (unfoldM seed' step))

-- | An infinite stream of values produced by a `RIO` action.
repeatM :: forall r e a. RIO r e a -> Stream r e a
repeatM ra = Stream do
  a <- ra
  pure (Yield a (repeatM ra))

-- | Map a pure function over every element.
map :: forall r e a b. (a -> b) -> Stream r e a -> Stream r e b
map f s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> pure (Yield (f a) (map f rest))

-- | Map an effectful function over every element.
mapM
  :: forall r e a b
   . (a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
mapM f s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> do
      b <- f a
      pure (Yield b (mapM f rest))

-- | Keep elements for which the predicate is true.
filter
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> Stream r e a
filter p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest ->
      if p a then pure (Yield a (filter p rest))
      else unStream (filter p rest)

-- | Concatenate two streams: drain the first, then drain the
-- | second.
concat :: forall r e a. Stream r e a -> Stream r e a -> Stream r e a
concat l r = Stream do
  step <- unStream l
  case step of
    Done -> unStream r
    Yield a rest -> pure (Yield a (concat rest r))

-- | Replace each element with a stream and concatenate.
flatMap
  :: forall r e a b
   . Stream r e a
  -> (a -> Stream r e b)
  -> Stream r e b
flatMap s f = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> unStream (concat (f a) (flatMap rest f))

-- | Take the first `n` elements.
take :: forall r e a. Int -> Stream r e a -> Stream r e a
take n s
  | n <= 0 = empty
  | otherwise = Stream do
      step <- unStream s
      case step of
        Done -> pure Done
        Yield a rest -> pure (Yield a (take (n - 1) rest))

-- | Drop the first `n` elements.
drop :: forall r e a. Int -> Stream r e a -> Stream r e a
drop n s
  | n <= 0 = s
  | otherwise = Stream do
      step <- unStream s
      case step of
        Done -> pure Done
        Yield _ rest -> unStream (drop (n - 1) rest)

-- | Drain the stream, discarding every value.
runDrain :: forall r e a. Stream r e a -> RIO r e Unit
runDrain s = do
  step <- unStream s
  case step of
    Done -> pure unit
    Yield _ rest -> runDrain rest

-- | Drain the stream and collect every value into an array.
runCollect :: forall r e a. Stream r e a -> RIO r e (Array a)
runCollect = go []
  where
  go :: Array a -> Stream r e a -> RIO r e (Array a)
  go acc s = do
    step <- unStream s
    case step of
      Done -> pure acc
      Yield a rest -> go (Array.snoc acc a) rest

-- | Left fold over the stream with a pure accumulator step.
runFold
  :: forall r e a b
   . b
  -> (b -> a -> b)
  -> Stream r e a
  -> RIO r e b
runFold seed step s = do
  st <- unStream s
  case st of
    Done -> pure seed
    Yield a rest -> runFold (step seed a) step rest

-- | Left fold with an effectful accumulator step.
runFoldM
  :: forall r e a b
   . b
  -> (b -> a -> RIO r e b)
  -> Stream r e a
  -> RIO r e b
runFoldM seed step s = do
  st <- unStream s
  case st of
    Done -> pure seed
    Yield a rest -> do
      seed' <- step seed a
      runFoldM seed' step rest

