-- | Pipes: composable stream-to-stream transducers.
-- |
-- | A `Pipe r e i o` consumes a stream of `i` and emits a stream of
-- | `o`. It is the natural middle ground between `Stream` (a
-- | producer) and `Sink` (a one-shot consumer): each input can emit
-- | zero or more outputs, and on end-of-stream the pipe gets a
-- | chance to emit any accumulated tail.
-- |
-- | The shape is the same allocator + callbacks pattern used by
-- | `Sink`: `mkPipe` (or one of the prebuilts) returns an `RIO` that
-- | allocates per-run state and hands back two callbacks:
-- |
-- |   * `onInput :: i -> RIO r e (PipeStep o)` consumes one
-- |     upstream element. The emitted array is pushed downstream;
-- |     `more = false` signals "no more upstream, flush and stop".
-- |   * `onDone :: RIO r e (Array o)` is called when upstream ends
-- |     before the pipe terminated itself; the array it returns is
-- |     the trailing emission (e.g. the last partial chunk).
-- |
-- | Pipes splice into streams via `Stream.via`. Pipes compose with
-- | `andThen`, building pipelines as values before applying them.
module RIO.Aff.Pipe
  ( Pipe(..)
  , PipeLoop
  , PipeStep
  , mkPipe
  , identity
  , map
  , filter
  , mapAccum
  , take
  , chunked
  , andThen
  ) where

import Prelude hiding (identity, map)

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Aff.Core (RIO)

-- | A stream-to-stream transducer.
newtype Pipe r e i o = Pipe (RIO r e (PipeLoop r e i o))

-- | The callbacks a pipe exposes after allocation.
type PipeLoop r e i o =
  { onInput :: i -> RIO r e (PipeStep o)
  , onDone :: RIO r e (Array o)
  }

-- | One step's emission. `more = false` means "I am done after
-- | these emissions, do not pull any more upstream".
type PipeStep o =
  { emit :: Array o
  , more :: Boolean
  }

-- | Build a pipe from a custom allocator. Most users want one of
-- | the prebuilts (`map`, `filter`, `chunked`, `take`, `mapAccum`).
mkPipe :: forall r e i o. RIO r e (PipeLoop r e i o) -> Pipe r e i o
mkPipe = Pipe

-- | The pipe that emits every input unchanged.
identity :: forall r e a. Pipe r e a a
identity = Pipe
  ( pure
      { onInput: \a -> pure { emit: [ a ], more: true }
      , onDone: pure []
      }
  )

-- | Emit `f a` for every input `a`. The pipe never terminates
-- | itself.
map :: forall r e a b. (a -> b) -> Pipe r e a b
map f = Pipe
  ( pure
      { onInput: \a -> pure { emit: [ f a ], more: true }
      , onDone: pure []
      }
  )

-- | Pass through inputs that satisfy the predicate; drop the rest.
filter :: forall r e a. (a -> Boolean) -> Pipe r e a a
filter p = Pipe
  ( pure
      { onInput: \a -> pure
          { emit: if p a then [ a ] else [], more: true }
      , onDone: pure []
      }
  )

-- | Stateful map: the step sees the previous state and the current
-- | input and returns the new state plus the emission. Useful for
-- | running-totals, indexing, deduplication, etc.
mapAccum
  :: forall r e s a b
   . s
  -> (s -> a -> Tuple s b)
  -> Pipe r e a b
mapAccum seed step = Pipe do
  ref <- liftEffect (Ref.new seed)
  pure
    { onInput: \a -> do
        s <- liftEffect (Ref.read ref)
        let Tuple s' b = step s a
        liftEffect (Ref.write s' ref)
        pure { emit: [ b ], more: true }
    , onDone: pure []
    }

-- | Pass through the first `n` inputs and stop. Negative or zero
-- | `n` emits nothing.
take :: forall r e a. Int -> Pipe r e a a
take n
  | n <= 0 = Pipe
      ( pure
          { onInput: \_ -> pure { emit: [], more: false }
          , onDone: pure []
          }
      )
  | otherwise = Pipe do
      ref <- liftEffect (Ref.new 0)
      pure
        { onInput: \a -> do
            seen <- liftEffect (Ref.modify (_ + 1) ref)
            pure { emit: [ a ], more: seen < n }
        , onDone: pure []
        }

-- | Group inputs into fixed-size arrays. The trailing chunk emitted
-- | by `onDone` may be shorter than `n`. Non-positive `n` is
-- | treated as 1.
chunked :: forall r e a. Int -> Pipe r e a (Array a)
chunked n0 = Pipe do
  let n = if n0 < 1 then 1 else n0
  bufRef <- liftEffect (Ref.new ([] :: Array a))
  pure
    { onInput: \a -> do
        buf' <- liftEffect (Ref.modify (\xs -> Array.snoc xs a) bufRef)
        if Array.length buf' >= n then do
          liftEffect (Ref.write [] bufRef)
          pure { emit: [ buf' ], more: true }
        else
          pure { emit: [], more: true }
    , onDone: do
        buf <- liftEffect (Ref.read bufRef)
        pure (if Array.null buf then [] else [ buf ])
    }

-- | Compose two pipes end-to-end: every output of the first pipe
-- | is fed as input to the second. If either pipe terminates with
-- | `more = false`, the combined pipe drains its tail and signals
-- | `more = false` after the final emissions.
andThen
  :: forall r e a b c. Pipe r e a b -> Pipe r e b c -> Pipe r e a c
andThen (Pipe mkP) (Pipe mkQ) = Pipe do
  p <- mkP
  q <- mkQ
  finishedRef <- liftEffect (Ref.new false)
  let
    runQ :: Array b -> RIO r e (PipeStep c)
    runQ bs = goQ bs { emit: [], more: true }

    goQ :: Array b -> PipeStep c -> RIO r e (PipeStep c)
    goQ bs acc = case Array.uncons bs of
      Nothing -> pure acc
      Just { head, tail } -> do
        if not acc.more then
          pure acc
        else do
          step <- q.onInput head
          goQ tail { emit: acc.emit <> step.emit, more: step.more }

    onInput :: a -> RIO r e (PipeStep c)
    onInput a = do
      done <- liftEffect (Ref.read finishedRef)
      if done then pure { emit: [], more: false }
      else do
        pStep <- p.onInput a
        qStep <- runQ pStep.emit
        let combinedMore = pStep.more && qStep.more
        if combinedMore then
          pure { emit: qStep.emit, more: true }
        else do
          tailB <- p.onDone
          qTail <- goQ tailB { emit: qStep.emit, more: qStep.more }
          qFinalDone <- q.onDone
          liftEffect (Ref.write true finishedRef)
          pure { emit: qTail.emit <> qFinalDone, more: false }

    onDone :: RIO r e (Array c)
    onDone = do
      done <- liftEffect (Ref.read finishedRef)
      if done then pure []
      else do
        tailB <- p.onDone
        qTail <- runQ tailB
        qFinalDone <- q.onDone
        liftEffect (Ref.write true finishedRef)
        pure (qTail.emit <> qFinalDone)
  pure { onInput, onDone }
