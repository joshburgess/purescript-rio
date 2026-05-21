-- | An in-memory `Tracer` for tests.
-- |
-- | `newRecordingTracer` returns a `Tracer` whose `startSpan`
-- | callback hands back a `Span` whose closures mutate an
-- | internal log, plus a `snapshot` action that returns every
-- | recorded span in start order.
-- |
-- | Time is virtual: each `startSpan` / `finish` increments a
-- | counter and uses the resulting `Number` as the span's
-- | `startTick` / `endTick`. Tests assert on the order of opens
-- | and closes, not on wall-clock durations.
-- |
-- | Span identity comes from `RIO.Fiber.Tracer.SpanId`. The
-- | recorder assigns each span its sequence number formatted as
-- | decimal (`"1"`, `"2"`, ...); the surrounding tracer code
-- | (`StartSpanRequest.parent`, `Span.addLink`) carries the
-- | `SpanId` straight through, so the recorded `parent` and
-- | `links` fields reflect the real graph.
module RIO.Fiber.Test.Tracer
  ( RecordedSpan
  , RecordingTracer
  , newRecordingTracer
  ) where

import Prelude

import Data.Array (mapMaybe, snoc) as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref

import RIO.Fiber.Tracer
  ( Attr
  , Span(..)
  , SpanId(..)
  , SpanKind
  , SpanStatus(..)
  , Tracer(..)
  , spanId
  )

-- | The recorded shape of a span. `endTick` is `Nothing` while
-- | the span is still open. `events` carries name + attributes
-- | entries in the order they were added; `links` carries the
-- | target `SpanId` of every `addLink` call against this span.
type RecordedSpan =
  { id :: SpanId
  , parent :: Maybe SpanId
  , name :: String
  , kind :: SpanKind
  , startTick :: Number
  , endTick :: Maybe Number
  , attributes :: Array (Tuple String String)
  , events :: Array { name :: String, attributes :: Array Attr }
  , links :: Array SpanId
  , status :: SpanStatus
  }

-- | A `Tracer` bundled with the controllers used to drive and
-- | inspect it.
type RecordingTracer =
  { tracer :: Tracer
  , snapshot :: Effect (Array RecordedSpan)
  }

-- | Allocate a fresh recording tracer. Virtual time starts at
-- | `0`; each `startSpan` / `finish` advances it by `1`.
newRecordingTracer :: Effect RecordingTracer
newRecordingTracer = do
  nextIdRef <- Ref.new 0
  spansRef <- Ref.new (Map.empty :: Map SpanId RecordedSpan)
  orderRef <- Ref.new ([] :: Array SpanId)
  tickRef <- Ref.new 0.0
  let
    nextTick :: Effect Number
    nextTick = Ref.modify (_ + 1.0) tickRef

    startSpan
      :: { name :: String
         , attributes :: Array Attr
         , parent :: Maybe Span
         , kind :: SpanKind
         }
      -> Effect Span
    startSpan req = do
      n <- Ref.modify (_ + 1) nextIdRef
      let sid = SpanId (show n)
      tick <- nextTick
      let
        rec :: RecordedSpan
        rec =
          { id: sid
          , parent: map spanId req.parent
          , name: req.name
          , kind: req.kind
          , startTick: tick
          , endTick: Nothing
          , attributes: map (\a -> Tuple a.key a.value) req.attributes
          , events: []
          , links: []
          , status: StatusUnset
          }
      Ref.modify_ (Map.insert sid rec) spansRef
      Ref.modify_ (\xs -> Array.snoc xs sid) orderRef
      pure (Span (spanClosures sid))

    spanClosures
      :: SpanId
      -> { spanId :: SpanId
         , addAttribute :: String -> String -> Effect Unit
         , addEvent :: String -> Array Attr -> Effect Unit
         , addLink :: Span -> Effect Unit
         , setStatus :: SpanStatus -> Effect Unit
         , finish :: Effect Unit
         }
    spanClosures sid =
      { spanId: sid
      , addAttribute: \k v ->
          Ref.modify_
            ( Map.update
                ( \s -> Just s
                    { attributes = Array.snoc s.attributes (Tuple k v) }
                )
                sid
            )
            spansRef
      , addEvent: \name attrs ->
          Ref.modify_
            ( Map.update
                ( \s -> Just s
                    { events = Array.snoc s.events { name, attributes: attrs } }
                )
                sid
            )
            spansRef
      , addLink: \target ->
          Ref.modify_
            ( Map.update
                ( \s -> Just s
                    { links = Array.snoc s.links (spanId target) }
                )
                sid
            )
            spansRef
      , setStatus: \status ->
          Ref.modify_
            (Map.update (\s -> Just s { status = status }) sid)
            spansRef
      , finish: do
          tick <- nextTick
          Ref.modify_
            ( Map.update
                ( \s -> case s.endTick of
                    Just _ -> Just s
                    Nothing -> Just s { endTick = Just tick }
                )
                sid
            )
            spansRef
      }

    snapshot :: Effect (Array RecordedSpan)
    snapshot = do
      m <- Ref.read spansRef
      order <- Ref.read orderRef
      pure (Array.mapMaybe (\sid -> Map.lookup sid m) order)

    tracer :: Tracer
    tracer = Tracer { startSpan }

  pure { tracer, snapshot }
