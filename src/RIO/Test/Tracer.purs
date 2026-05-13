-- | An in-memory `Tracer` for tests.
-- |
-- | `newRecordingTracer` returns a `Tracer` whose `startSpan`,
-- | `endSpan`, and `addAttribute` record every operation into
-- | in-memory `Ref`s, plus a `snapshot` action that returns the
-- | recorded spans in start order.
-- |
-- | Time is virtual: each operation increments a counter and uses
-- | the resulting integer as the span's `startMs` / `endMs`. This
-- | keeps the recorder fully deterministic; tests assert on the
-- | order of opens and closes, not on wall-clock durations.
module RIO.Test.Tracer
  ( RecordingTracer
  , newRecordingTracer
  ) where

import Prelude

import Data.Array (mapMaybe, snoc) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.Tracer (Span, SpanId(..), SpanStatus(..), Tracer)

-- | A `Tracer` bundled with the controller that reads back what it
-- | recorded. The test holds the whole record, provides `tracer` to
-- | the program under test as the `tracer` service, and calls
-- | `snapshot` from the test thread to inspect what was captured.
type RecordingTracer =
  { tracer :: Tracer
  , snapshot :: Effect (Array Span)
  }

-- | Allocate a fresh recording tracer. Virtual time starts at 0;
-- | each `startSpan` / `endSpan` advances it by 1.
newRecordingTracer :: Aff RecordingTracer
newRecordingTracer = liftEffect do
  nextIdRef <- Ref.new 0
  currentRef <- Ref.new (Nothing :: Maybe SpanId)
  spansRef <- Ref.new (Map.empty :: Map SpanId Span)
  orderRef <- Ref.new ([] :: Array SpanId)
  tickRef <- Ref.new 0.0
  let
    nextTick :: Effect Number
    nextTick = Ref.modify (_ + 1.0) tickRef

    startSpan :: String -> Effect SpanId
    startSpan name = do
      n <- Ref.modify (_ + 1) nextIdRef
      let sid = SpanId n
      parent <- Ref.read currentRef
      tick <- nextTick
      let
        span :: Span
        span =
          { id: sid
          , parent
          , name
          , startMs: tick
          , endMs: Nothing
          , status: SpanOk
          , attributes: []
          }
      Ref.modify_ (Map.insert sid span) spansRef
      Ref.modify_ (\xs -> Array.snoc xs sid) orderRef
      Ref.write (Just sid) currentRef
      pure sid

    endSpan :: SpanId -> SpanStatus -> Effect Unit
    endSpan sid status = do
      m <- Ref.read spansRef
      case Map.lookup sid m of
        Nothing -> pure unit
        Just span -> case span.endMs of
          Just _ -> pure unit
          Nothing -> do
            tick <- nextTick
            let updated = span { endMs = Just tick, status = status }
            Ref.modify_ (Map.insert sid updated) spansRef
            cur <- Ref.read currentRef
            when (cur == Just sid) (Ref.write span.parent currentRef)

    addAttribute :: SpanId -> String -> String -> Effect Unit
    addAttribute sid key value = do
      m <- Ref.read spansRef
      case Map.lookup sid m of
        Nothing -> pure unit
        Just span -> do
          let
            updated = span
              { attributes = Array.snoc span.attributes (Tuple key value)
              }
          Ref.modify_ (Map.insert sid updated) spansRef

    currentSpan :: Effect (Maybe SpanId)
    currentSpan = Ref.read currentRef

    snapshot :: Effect (Array Span)
    snapshot = do
      m <- Ref.read spansRef
      order <- Ref.read orderRef
      pure (Array.mapMaybe (\sid -> Map.lookup sid m) order)

    tracer :: Tracer
    tracer =
      { startSpan
      , endSpan
      , addAttribute
      , currentSpan
      }
  pure { tracer, snapshot }
