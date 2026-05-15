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
  , chunk
  , concat
  , distinct
  , drop
  , dropWhile
  , empty
  , filter
  , flatMap
  , flatten
  , fromArray
  , intersperse
  , map
  , mapM
  , repeatM
  , runCollect
  , runDrain
  , runFold
  , runFoldM
  , scan
  , scanM
  , single
  , take
  , takeWhile
  , unfoldM
  , zip
  , zipWith
  , zipWithIndex
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

-- | Pair two streams elementwise. Ends when either input ends, so
-- | the output length is the minimum of the two inputs.
-- |
-- | Both streams are stepped on each output, in left-then-right
-- | order. There is no buffering: this is a strict zip on a pull
-- | stream.
zip
  :: forall r e a b
   . Stream r e a
  -> Stream r e b
  -> Stream r e (Tuple a b)
zip = zipWith Tuple

-- | Combine two streams elementwise with a function. Ends when
-- | either input ends.
zipWith
  :: forall r e a b c
   . (a -> b -> c)
  -> Stream r e a
  -> Stream r e b
  -> Stream r e c
zipWith f l r = Stream do
  stepL <- unStream l
  case stepL of
    Done -> pure Done
    Yield a restL -> do
      stepR <- unStream r
      case stepR of
        Done -> pure Done
        Yield b restR -> pure (Yield (f a b) (zipWith f restL restR))

-- | Pair each element with its 0-based position in the stream.
zipWithIndex
  :: forall r e a
   . Stream r e a
  -> Stream r e (Tuple Int a)
zipWithIndex = go 0
  where
  go :: Int -> Stream r e a -> Stream r e (Tuple Int a)
  go i s = Stream do
    step <- unStream s
    case step of
      Done -> pure Done
      Yield a rest -> pure (Yield (Tuple i a) (go (i + 1) rest))

-- | Running prefix fold. Emits the seed first, then one element
-- | per input element with the accumulator applied. The output is
-- | always one element longer than the input.
-- |
-- | ```purescript
-- | -- prefix sums of [1, 2, 3] starting at 0 yields [0, 1, 3, 6]
-- | runCollect (scan 0 (+) (fromArray [1, 2, 3]))
-- | ```
scan
  :: forall r e a b
   . b
  -> (b -> a -> b)
  -> Stream r e a
  -> Stream r e b
scan seed step s = Stream do
  pure (Yield seed (scanRest seed step s))
  where
  scanRest :: b -> (b -> a -> b) -> Stream r e a -> Stream r e b
  scanRest acc f t = Stream do
    st <- unStream t
    case st of
      Done -> pure Done
      Yield a rest ->
        let
          acc' = f acc a
        in
          pure (Yield acc' (scanRest acc' f rest))

-- | Effectful variant of `scan`. Each step yields the updated
-- | accumulator and runs in `RIO r e`.
scanM
  :: forall r e a b
   . b
  -> (b -> a -> RIO r e b)
  -> Stream r e a
  -> Stream r e b
scanM seed step s = Stream do
  pure (Yield seed (go seed step s))
  where
  go :: b -> (b -> a -> RIO r e b) -> Stream r e a -> Stream r e b
  go acc f t = Stream do
    st <- unStream t
    case st of
      Done -> pure Done
      Yield a rest -> do
        acc' <- f acc a
        pure (Yield acc' (go acc' f rest))

-- | Group consecutive elements into arrays of size `n`. The final
-- | chunk may be shorter when the input length is not a multiple
-- | of `n`. `n <= 0` produces an empty stream.
chunk :: forall r e a. Int -> Stream r e a -> Stream r e (Array a)
chunk n s
  | n <= 0 = empty
  | otherwise = chunkGo n [] s

chunkGo :: forall r e a. Int -> Array a -> Stream r e a -> Stream r e (Array a)
chunkGo n acc t = Stream do
  step <- unStream t
  case step of
    Done ->
      if Array.null acc then pure Done
      else pure (Yield acc empty)
    Yield a rest ->
      let
        acc' = Array.snoc acc a
      in
        if Array.length acc' >= n then pure (Yield acc' (chunkGo n [] rest))
        else unStream (chunkGo n acc' rest)

-- | Take elements while the predicate holds. Stops at (and does
-- | not emit) the first element for which `p` is false.
takeWhile
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> Stream r e a
takeWhile p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest ->
      if p a then pure (Yield a (takeWhile p rest))
      else pure Done

-- | Drop elements while the predicate holds, then yield the rest
-- | unchanged. The first failing element is included in the
-- | output.
dropWhile
  :: forall r e a
   . (a -> Boolean)
  -> Stream r e a
  -> Stream r e a
dropWhile p s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest ->
      if p a then unStream (dropWhile p rest)
      else pure (Yield a rest)

-- | Insert `sep` between consecutive elements. The output has
-- | `2n - 1` elements when the input has `n` elements; an empty
-- | input stays empty.
intersperse :: forall r e a. a -> Stream r e a -> Stream r e a
intersperse sep s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> pure (Yield a (prefixed rest))
  where
  prefixed :: Stream r e a -> Stream r e a
  prefixed t = Stream do
    step <- unStream t
    case step of
      Done -> pure Done
      Yield a rest -> pure (Yield sep (Stream (pure (Yield a (prefixed rest)))))

-- | Flatten a stream of streams into one stream. Each inner stream
-- | is drained in full before the next is stepped.
flatten :: forall r e a. Stream r e (Stream r e a) -> Stream r e a
flatten ss = flatMap ss identity

-- | Drop consecutive duplicate elements (keeps the first of each
-- | run). Compares with `Eq`.
distinct :: forall r e a. Eq a => Stream r e a -> Stream r e a
distinct s = Stream do
  step <- unStream s
  case step of
    Done -> pure Done
    Yield a rest -> pure (Yield a (go a rest))
  where
  go :: a -> Stream r e a -> Stream r e a
  go prev t = Stream do
    step <- unStream t
    case step of
      Done -> pure Done
      Yield a rest ->
        if a == prev then unStream (go prev rest)
        else pure (Yield a (go a rest))

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

