-- | Sinks: composable consumers of `Stream` output.
-- |
-- | A `Sink r e i o` folds a stream of `i` values into a single `o`,
-- | possibly performing effects in `RIO r e` along the way. A sink
-- | is the natural dual of `Stream`: where a `Stream` produces values
-- | on each pull, a `Sink` accepts them and decides whether to keep
-- | going or stop early with a final answer.
-- |
-- | The shape is a single allocation step (`Sink (RIO r e SinkLoop)`)
-- | that returns a record of two callbacks:
-- |
-- |   * `step :: i -> RIO r e (Maybe o)` consumes one element. Return
-- |     `Nothing` to ask for another, `Just o` to terminate early.
-- |   * `done :: RIO r e o` is called when the upstream signals
-- |     end-of-stream before the sink terminated itself; it
-- |     synthesises a final result from whatever was accumulated.
-- |
-- | Sinks are run against streams with `Stream.runSink`.
module RIO.Fiber.Sink
  ( Sink(..)
  , SinkLoop
  , mkSink
  , map
  , contramap
  , contramapRIO
  , contramapM
  , filterIn
  , count
  , sum
  , product
  , minimum
  , maximum
  , mconcat
  , collectAll
  , head
  , last
  , drain
  , foreach
  , fold
  , foldRIO
  , foldUntil
  , foldRIOUntil
  , takeN
  , dropN
  , takeWhile
  , dropWhile
  , mkString
  , find
  , findRIO
  , any
  , all
  , andThen
  , race
  , zipPar
  , zipWithPar
  ) where

import Prelude hiding (map)
import Prelude as Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Ref as Ref
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Core as F

-- | A sink. The outer `RIO` allocates whatever per-run state the sink
-- | needs (typically a `Ref`); the returned record exposes the
-- | step / done callbacks that drive the consumption loop.
newtype Sink r e i o = Sink (RIO r e (SinkLoop r e i o))

-- | The callbacks a sink hands back from its initial allocation.
-- |
-- |   * `step` consumes one element. `Nothing` asks for another;
-- |     `Just o` terminates the run immediately with `o`.
-- |   * `done` synthesises a result when the stream ends before
-- |     `step` opted out.
type SinkLoop r e i o =
  { step :: i -> RIO r e (Maybe o)
  , done :: RIO r e o
  }

-- | Build a sink from an allocation step. The allocator runs once per
-- | run, so per-run mutable state (e.g. `Ref`) stays local to that
-- | run rather than leaking between calls.
mkSink :: forall r e i o. RIO r e (SinkLoop r e i o) -> Sink r e i o
mkSink = Sink

-- | Post-process a sink's output. Behaves like `Functor` on the
-- | output type; the input side is untouched.
map :: forall r e i a b. (a -> b) -> Sink r e i a -> Sink r e i b
map f (Sink mk) = Sink do
  loop <- mk
  pure
    { step: \i -> Prelude.map (Prelude.map f) (loop.step i)
    , done: Prelude.map f loop.done
    }

-- | Pre-process each element with `f` before it reaches the sink.
-- | Behaves like `Contravariant` on the input type.
contramap :: forall r e i j o. (j -> i) -> Sink r e i o -> Sink r e j o
contramap f (Sink mk) = Sink do
  loop <- mk
  pure
    { step: \j -> loop.step (f j)
    , done: loop.done
    }

-- | Effectful contravariant input map. Each input is first transformed
-- | by `f` (which runs in `RIO` and may consult services or fail)
-- | before being fed to the underlying sink.
contramapRIO
  :: forall r e i j o
   . (j -> RIO r e i)
  -> Sink r e i o
  -> Sink r e j o
contramapRIO f (Sink mk) = Sink do
  loop <- mk
  pure
    { step: \j -> do
        i <- f j
        loop.step i
    , done: loop.done
    }

-- | Alias for `contramapRIO`, named to mirror rio-aff.
contramapM
  :: forall r e i j o
   . (j -> RIO r e i)
  -> Sink r e i o
  -> Sink r e j o
contramapM = contramapRIO

-- | Drop inputs for which the predicate is false before feeding them
-- | to the underlying sink. The underlying sink only ever observes
-- | elements that pass `p`, and its `done` is used unchanged on
-- | end-of-stream.
filterIn :: forall r e i o. (i -> Boolean) -> Sink r e i o -> Sink r e i o
filterIn p (Sink mk) = Sink do
  loop <- mk
  pure
    { step: \i -> if p i then loop.step i else pure Nothing
    , done: loop.done
    }

-- | Count every element the sink sees. Never terminates early; the
-- | result is the total count when the stream ends.
count :: forall r e i. Sink r e i Int
count = Sink do
  ref <- liftEffect (Ref.new 0)
  pure
    { step: \_ -> liftEffect (Ref.modify_ (_ + 1) ref) *> pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Sum every element via its `Semiring` instance. Starts from `zero`.
sum :: forall r e a. Semiring a => Sink r e a a
sum = Sink do
  ref <- liftEffect (Ref.new zero)
  pure
    { step: \a -> liftEffect (Ref.modify_ (_ + a) ref) *> pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Multiply every element via its `Semiring` instance. Starts from
-- | `one`, so an empty stream yields `one`.
product :: forall r e a. Semiring a => Sink r e a a
product = Sink do
  ref <- liftEffect (Ref.new one)
  pure
    { step: \a -> liftEffect (Ref.modify_ (_ * a) ref) *> pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | The minimum element under the `Ord` instance, or `Nothing` if the
-- | stream was empty.
minimum :: forall r e a. Ord a => Sink r e a (Maybe a)
minimum = Sink do
  ref <- liftEffect (Ref.new (Nothing :: Maybe _))
  pure
    { step: \a -> do
        liftEffect
          ( Ref.modify_
              ( case _ of
                  Nothing -> Just a
                  Just curr -> Just (min curr a)
              )
              ref
          )
        pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | The maximum element under the `Ord` instance, or `Nothing` if the
-- | stream was empty.
maximum :: forall r e a. Ord a => Sink r e a (Maybe a)
maximum = Sink do
  ref <- liftEffect (Ref.new (Nothing :: Maybe _))
  pure
    { step: \a -> do
        liftEffect
          ( Ref.modify_
              ( case _ of
                  Nothing -> Just a
                  Just curr -> Just (max curr a)
              )
              ref
          )
        pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Concatenate every element under its `Monoid`. Empty stream yields
-- | `mempty`.
mconcat :: forall r e a. Monoid a => Sink r e a a
mconcat = Sink do
  ref <- liftEffect (Ref.new mempty)
  pure
    { step: \a -> liftEffect (Ref.modify_ (_ <> a) ref) *> pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Collect every element into an array. Use only with bounded
-- | streams; an unbounded stream would grow the array without limit.
collectAll :: forall r e i. Sink r e i (Array i)
collectAll = Sink do
  ref <- liftEffect (Ref.new [])
  pure
    { step: \i ->
        liftEffect (Ref.modify_ (\xs -> Array.snoc xs i) ref) *> pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | The first element, or `Nothing` if the stream was empty. This
-- | sink terminates as soon as it sees a single value, so the
-- | upstream is short-circuited after one pull.
head :: forall r e i. Sink r e i (Maybe i)
head = Sink
  ( pure
      { step: \i -> pure (Just (Just i))
      , done: pure Nothing
      }
  )

-- | The last element, or `Nothing` if the stream was empty. Pulls
-- | the entire stream.
last :: forall r e i. Sink r e i (Maybe i)
last = Sink do
  ref <- liftEffect (Ref.new Nothing)
  pure
    { step: \i -> liftEffect (Ref.write (Just i) ref) *> pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Discard every element. Returns `unit` when the stream ends.
drain :: forall r e i. Sink r e i Unit
drain = Sink (pure { step: \_ -> pure Nothing, done: pure unit })

-- | Run an effectful action for each element. The action's failures
-- | propagate; success is discarded.
foreach :: forall r e i. (i -> RIO r e Unit) -> Sink r e i Unit
foreach f = Sink (pure { step: \i -> f i *> pure Nothing, done: pure unit })

-- | Strict left fold, lifted to `Sink`. The step function is pure;
-- | for an effectful step, use `foldRIO`.
fold :: forall r e a b. (b -> a -> b) -> b -> Sink r e a b
fold step seed = Sink do
  ref <- liftEffect (Ref.new seed)
  pure
    { step: \a ->
        liftEffect (Ref.modify_ (\b -> step b a) ref) *> pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Strict left fold with an effectful step. The step's failures
-- | propagate and short-circuit the run.
foldRIO :: forall r e a b. (b -> a -> RIO r e b) -> b -> Sink r e a b
foldRIO step seed = Sink do
  ref <- liftEffect (Ref.new seed)
  pure
    { step: \a -> do
        b <- liftEffect (Ref.read ref)
        b' <- step b a
        liftEffect (Ref.write b' ref)
        pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Fold until the accumulator satisfies `stop`, then terminate
-- | early with that accumulator. If the stream ends first, the
-- | final accumulator is returned regardless.
foldUntil
  :: forall r e a b. (b -> Boolean) -> (b -> a -> b) -> b -> Sink r e a b
foldUntil stop step seed = Sink do
  ref <- liftEffect (Ref.new seed)
  pure
    { step: \a -> do
        b' <- liftEffect (Ref.modify (\b -> step b a) ref)
        pure (if stop b' then Just b' else Nothing)
    , done: liftEffect (Ref.read ref)
    }

-- | Effectful variant of `foldUntil`. The step runs in `RIO`, so it
-- | can fail or observe services. Termination still happens when
-- | `stop` returns `true` for the resulting accumulator.
foldRIOUntil
  :: forall r e a b
   . (b -> Boolean)
  -> (b -> a -> RIO r e b)
  -> b
  -> Sink r e a b
foldRIOUntil stop step seed = Sink do
  ref <- liftEffect (Ref.new seed)
  pure
    { step: \a -> do
        b <- liftEffect (Ref.read ref)
        b' <- step b a
        liftEffect (Ref.write b' ref)
        pure (if stop b' then Just b' else Nothing)
    , done: liftEffect (Ref.read ref)
    }

-- | Collect the first `n` elements and terminate. If the stream is
-- | shorter than `n`, returns whatever was seen.
takeN :: forall r e i. Int -> Sink r e i (Array i)
takeN n
  | n <= 0 = Sink (pure { step: \_ -> pure (Just []), done: pure [] })
  | otherwise = Sink do
      ref <- liftEffect (Ref.new ([] :: Array _))
      pure
        { step: \i -> do
            xs <- liftEffect (Ref.modify (\x -> Array.snoc x i) ref)
            pure (if Array.length xs >= n then Just xs else Nothing)
        , done: liftEffect (Ref.read ref)
        }

-- | Skip the first `n` elements and collect the rest into an array.
-- | A non-positive `n` collects the full stream. Mirrors `takeN`.
dropN :: forall r e i. Int -> Sink r e i (Array i)
dropN n = Sink do
  remaining <- liftEffect (Ref.new (max 0 n))
  ref <- liftEffect (Ref.new ([] :: Array _))
  pure
    { step: \i -> do
        left <- liftEffect (Ref.read remaining)
        if left > 0 then do
          liftEffect (Ref.write (left - 1) remaining)
          pure Nothing
        else do
          liftEffect (Ref.modify_ (\xs -> Array.snoc xs i) ref)
          pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Collect leading elements while `p` holds, then terminate with
-- | the accumulated array. The first element that fails the
-- | predicate stops the run and is not included. If the stream ends
-- | before `p` fails, every element seen is returned.
takeWhile :: forall r e i. (i -> Boolean) -> Sink r e i (Array i)
takeWhile p = Sink do
  ref <- liftEffect (Ref.new ([] :: Array _))
  pure
    { step: \i ->
        if p i then do
          liftEffect (Ref.modify_ (\xs -> Array.snoc xs i) ref)
          pure Nothing
        else do
          xs <- liftEffect (Ref.read ref)
          pure (Just xs)
    , done: liftEffect (Ref.read ref)
    }

-- | Skip leading elements while `p` holds, then collect the rest
-- | (including the first element that failed `p`) into an array.
-- | Returns the empty array if every element matched `p`.
dropWhile :: forall r e i. (i -> Boolean) -> Sink r e i (Array i)
dropWhile p = Sink do
  dropping <- liftEffect (Ref.new true)
  ref <- liftEffect (Ref.new ([] :: Array _))
  pure
    { step: \i -> do
        stillDropping <- liftEffect (Ref.read dropping)
        if stillDropping && p i then pure Nothing
        else do
          liftEffect do
            Ref.write false dropping
            Ref.modify_ (\xs -> Array.snoc xs i) ref
          pure Nothing
    , done: liftEffect (Ref.read ref)
    }

-- | Concatenate every element into a single string, joined by
-- | `sep`. The separator only appears between elements: zero inputs
-- | yield `""`, one input yields the input unchanged.
mkString :: forall r e. String -> Sink r e String String
mkString sep = Sink do
  ref <- liftEffect (Ref.new (Nothing :: Maybe String))
  pure
    { step: \s -> do
        liftEffect (Ref.modify_ (append s) ref)
        pure Nothing
    , done: do
        m <- liftEffect (Ref.read ref)
        pure (case m of
          Nothing -> ""
          Just out -> out)
    }
  where
  -- Append `s` to the accumulator, inserting `sep` between every
  -- pair of inputs. `Nothing` marks "no input seen yet."
  append :: String -> Maybe String -> Maybe String
  append s Nothing = Just s
  append s (Just acc) = Just (acc <> sep <> s)

-- | Find the first element matching the pure predicate, terminating
-- | early. Returns `Nothing` if the stream ends without a match.
find :: forall r e i. (i -> Boolean) -> Sink r e i (Maybe i)
find p = Sink
  ( pure
      { step: \i -> pure (if p i then Just (Just i) else Nothing)
      , done: pure Nothing
      }
  )

-- | Find the first element for which the effectful predicate
-- | returns `true`, terminating early. Returns `Nothing` if the
-- | stream ends without a match. Predicate failures propagate.
findRIO :: forall r e i. (i -> RIO r e Boolean) -> Sink r e i (Maybe i)
findRIO p = Sink
  ( pure
      { step: \i -> do
          ok <- p i
          pure (if ok then Just (Just i) else Nothing)
      , done: pure Nothing
      }
  )

-- | Whether any element matches `p`. Short-circuits on the first
-- | match, returning `true`; if the stream ends without a match,
-- | returns `false`.
any :: forall r e i. (i -> Boolean) -> Sink r e i Boolean
any p = Sink
  ( pure
      { step: \i -> pure (if p i then Just true else Nothing)
      , done: pure false
      }
  )

-- | Whether every element matches `p`. Short-circuits on the first
-- | non-match, returning `false`; if the stream ends with every
-- | element matching (including the empty stream), returns `true`.
all :: forall r e i. (i -> Boolean) -> Sink r e i Boolean
all p = Sink
  ( pure
      { step: \i -> pure (if p i then Nothing else Just false)
      , done: pure true
      }
  )

-- | Sequence two sinks: run the first against the stream, then resume
-- | with the second (built from the first's result) from the next
-- | input. The element that triggered the first sink's termination is
-- | consumed by the first sink; the second sink starts on the element
-- | after that.
-- |
-- | If the stream ends while the first sink is still consuming, the
-- | first sink's `done` runs, the result is fed into `k`, and the
-- | resulting second sink's `done` runs immediately (so it never sees
-- | any input).
andThen
  :: forall r e i a b
   . Sink r e i a
  -> (a -> Sink r e i b)
  -> Sink r e i b
andThen (Sink mkA) k = Sink do
  loopA <- mkA
  phaseRef <- liftEffect (Ref.new (Nothing :: Maybe (SinkLoop r e i b)))
  let
    stepFn i = do
      mLoopB <- liftEffect (Ref.read phaseRef)
      case mLoopB of
        Just loopB -> loopB.step i
        Nothing -> do
          mResultA <- loopA.step i
          case mResultA of
            Nothing -> pure Nothing
            Just a -> do
              let Sink mkB = k a
              loopB <- mkB
              liftEffect (Ref.write (Just loopB) phaseRef)
              pure Nothing
    doneFn = do
      mLoopB <- liftEffect (Ref.read phaseRef)
      case mLoopB of
        Just loopB -> loopB.done
        Nothing -> do
          a <- loopA.done
          let Sink mkB = k a
          loopB <- mkB
          loopB.done
  pure { step: stepFn, done: doneFn }

-- | Run two sinks against the same input stream, with each element
-- | fed to both step functions in parallel. Whichever side terminates
-- | early wins and its result is returned. If neither terminates and
-- | the stream ends, the left sink's `done` is used as the tiebreaker
-- | (matching the "left wins" convention elsewhere in the runtime).
race :: forall r e i o. Sink r e i o -> Sink r e i o -> Sink r e i o
race (Sink mkA) (Sink mkB) = Sink do
  loopA <- mkA
  loopB <- mkB
  pure
    { step: \i -> do
        Tuple ma mb <- F.zipPar (loopA.step i) (loopB.step i)
        case ma, mb of
          Just a, _ -> pure (Just a)
          _, Just b -> pure (Just b)
          _, _ -> pure Nothing
    , done: loopA.done
    }

-- | Run two sinks against the same input stream in parallel and pair
-- | their results. Each element is fed to both step functions via
-- | `RIO.zipPar`. A sink that terminates early stops accepting input
-- | (its stored result is reused) while the other continues. The
-- | combined sink terminates as soon as both sides have results, or
-- | on upstream end, whichever comes first; in the latter case any
-- | side that hadn't terminated has its `done` invoked.
zipPar
  :: forall r e i a b
   . Sink r e i a
  -> Sink r e i b
  -> Sink r e i (Tuple a b)
zipPar = zipWithPar Tuple

-- | Like `zipPar`, but combine the two results with `f`.
zipWithPar
  :: forall r e i a b c
   . (a -> b -> c)
  -> Sink r e i a
  -> Sink r e i b
  -> Sink r e i c
zipWithPar f (Sink mkA) (Sink mkB) = Sink do
  loopA <- mkA
  loopB <- mkB
  resA <- liftEffect (Ref.new (Nothing :: Maybe a))
  resB <- liftEffect (Ref.new (Nothing :: Maybe b))
  let
    stepA i = do
      stored <- liftEffect (Ref.read resA)
      case stored of
        Just _ -> pure unit
        Nothing -> do
          r <- loopA.step i
          case r of
            Just a -> liftEffect (Ref.write (Just a) resA)
            Nothing -> pure unit
    stepB i = do
      stored <- liftEffect (Ref.read resB)
      case stored of
        Just _ -> pure unit
        Nothing -> do
          r <- loopB.step i
          case r of
            Just b -> liftEffect (Ref.write (Just b) resB)
            Nothing -> pure unit
  pure
    { step: \i -> do
        _ <- F.zipPar (stepA i) (stepB i)
        ma <- liftEffect (Ref.read resA)
        mb <- liftEffect (Ref.read resB)
        case ma, mb of
          Just a, Just b -> pure (Just (f a b))
          _, _ -> pure Nothing
    , done: do
        ma <- liftEffect (Ref.read resA)
        a <- case ma of
          Just a -> pure a
          Nothing -> loopA.done
        mb <- liftEffect (Ref.read resB)
        b <- case mb of
          Just b -> pure b
          Nothing -> loopB.done
        pure (f a b)
    }
