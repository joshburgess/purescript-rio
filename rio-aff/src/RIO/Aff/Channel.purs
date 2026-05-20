-- | A minimal pull-based `Channel r e i o d` primitive.
-- |
-- | `Channel` is the unified shape that both `Stream` and `Sink`
-- | specialise: it can request inputs of type `i`, emit outputs of
-- | type `o`, fail with a typed error in row `e`, and terminate
-- | with a done value of type `d`. Streams are channels that
-- | never read (so `i` is unused); sinks are channels that never
-- | emit (so `o` is unused); a closed pipeline is a channel that
-- | neither reads nor emits and just produces a `d`.
-- |
-- | This module is deliberately scoped to demonstrate that the
-- | bedrock primitive from Effect-TS and ZIO ports cleanly to
-- | PureScript: it provides the type, the obvious constructors,
-- | interop with `RIO.Aff.Stream` and `RIO.Aff.Sink`, and `pipe` /
-- | `run`. The full Channel API (broadcasters, halt-when,
-- | resource-safe finalisation, fan-out) lives on `Stream` and
-- | `Sink` directly; users who reach for those reach for the
-- | concrete types.
-- |
-- | ```purescript
-- | -- a closed pipeline: stream piped into a sink, run to its
-- | -- terminal value
-- | result <- run
-- |   (pipe (fromStream (Stream.fromArray [1, 2, 3]))
-- |         (fromSink (Sink.collect)))
-- | -- result :: Array Int = [1, 2, 3]
-- | ```
module RIO.Aff.Channel
  ( Channel(..)
  , ChStep(..)
  , unChannel
  , done
  , emit
  , read_
  , fromStream
  , fromSink
  , pipe
  , run
  ) where

import Prelude

import RIO.Aff.Internal (RIO)
import RIO.Aff.Sink (Sink, Step(..), unSink) as Sink
import RIO.Aff.Stream (Stream, Step(..), unStream) as Stream

-- | One step of a channel.
-- |
-- |   * `ChDone d` terminates the channel with value `d`.
-- |   * `ChOut o k` emits `o` and continues with `k`.
-- |   * `ChIn k eof` asks the outside for an `i`. On a value,
-- |     resumes with `k i`; on end-of-input, falls through to
-- |     `eof` (a channel that produces the terminal `d`
-- |     without further reads).
data ChStep :: Row Type -> Row Type -> Type -> Type -> Type -> Type
data ChStep r e i o d
  = ChDone d
  | ChOut o (Channel r e i o d)
  | ChIn (i -> Channel r e i o d) (Channel r e i o d)

-- | The channel itself: an effectful `RIO` that, when run,
-- | produces one step.
newtype Channel :: Row Type -> Row Type -> Type -> Type -> Type -> Type
newtype Channel r e i o d = Channel (RIO r e (ChStep r e i o d))

-- | Project a channel back into its underlying step action.
-- | Companion modules can use this to implement custom operators
-- | without peeling the newtype each time.
unChannel :: forall r e i o d. Channel r e i o d -> RIO r e (ChStep r e i o d)
unChannel (Channel m) = m

-- | A channel that immediately terminates with the given done
-- | value. No inputs are read; no outputs are emitted.
done :: forall r e i o d. d -> Channel r e i o d
done d = Channel (pure (ChDone d))

-- | Emit a single output, then continue with `next`.
emit :: forall r e i o d. o -> Channel r e i o d -> Channel r e i o d
emit o next = Channel (pure (ChOut o next))

-- | Request a single input. On an input value, continue with
-- | `k i`; on end-of-input, fall through to `eof`.
read_
  :: forall r e i o d
   . (i -> Channel r e i o d)
  -> Channel r e i o d
  -> Channel r e i o d
read_ k eof = Channel (pure (ChIn k eof))

-- | Lift a `RIO.Aff.Stream.Stream r e a` into a channel: a source
-- | that never reads, emits each `a`, and terminates with
-- | `unit` once the underlying stream is exhausted.
-- |
-- | The input and done parameters are polymorphic because a
-- | source channel does not constrain them.
fromStream :: forall r e i a. Stream.Stream r e a -> Channel r e i a Unit
fromStream s = Channel do
  step <- Stream.unStream s
  case step of
    Stream.Done -> pure (ChDone unit)
    Stream.Yield a rest -> pure (ChOut a (fromStream rest))

-- | Lift a `RIO.Aff.Sink.Sink r e i a` into a channel: a sink that
-- | never emits, reads `i`s, and terminates with the sink's
-- | result of type `a`.
-- |
-- | The output parameter is polymorphic because a sink channel
-- | never emits.
fromSink :: forall r e i o a. Sink.Sink r e i a -> Channel r e i o a
fromSink sk = Channel do
  step <- Sink.unSink sk
  case step of
    Sink.Halt a -> pure (ChDone a)
    Sink.Need k finish ->
      pure
        ( ChIn
            (\i -> fromSink (k i))
            (Channel (ChDone <$> finish))
        )

-- | Compose two channels end-to-end: outputs of the upstream
-- | become inputs of the downstream. The done type is the
-- | downstream's; the upstream's done value is discarded when
-- | downstream finishes first.
-- |
-- | If the upstream finishes before the downstream has read
-- | everything it wanted, the downstream's `eof` branch runs and
-- | produces the terminal done value.
pipe
  :: forall r e i mid o x d
   . Channel r e i mid x
  -> Channel r e mid o d
  -> Channel r e i o d
pipe up downCh = Channel do
  ds <- unChannel downCh
  case ds of
    ChDone d -> pure (ChDone d)
    ChOut o k -> pure (ChOut o (pipe up k))
    ChIn k eof -> stepUpstream up k eof
  where
  stepUpstream
    :: Channel r e i mid x
    -> (mid -> Channel r e mid o d)
    -> Channel r e mid o d
    -> RIO r e (ChStep r e i o d)
  stepUpstream upCh k eof = do
    us <- unChannel upCh
    case us of
      ChDone _ -> unChannel (pipeEof eof)
      ChOut o up' -> unChannel (pipe up' (k o))
      ChIn ki upEof -> do
        let downRest = Channel (pure (ChIn k eof))
        pure
          ( ChIn
              (\i -> pipe (ki i) downRest)
              (pipe upEof downRest)
          )

  -- When the upstream has finished, the downstream's eof branch
  -- becomes the new pipeline. It still has its own input row
  -- `mid` but should never read because upstream is gone; we
  -- coerce the input through `read_` (returning eof immediately).
  pipeEof
    :: Channel r e mid o d
    -> Channel r e i o d
  pipeEof eof = Channel do
    s <- unChannel eof
    case s of
      ChDone d -> pure (ChDone d)
      ChOut o next -> pure (ChOut o (pipeEof next))
      ChIn _ inner -> unChannel (pipeEof inner)

-- | Run a closed channel (one that neither reads nor emits) and
-- | return its terminal done value.
-- |
-- | If the channel tries to read, the read returns the EOF
-- | branch immediately; if it tries to emit, the output is
-- | discarded. A truly closed pipeline (a source piped into a
-- | sink) reaches `ChDone` without invoking either.
run :: forall r e d. Channel r e Void Void d -> RIO r e d
run = loop
  where
  loop ch = do
    s <- unChannel ch
    case s of
      ChDone d -> pure d
      ChOut _ k -> loop k
      ChIn _ eof -> loop eof
