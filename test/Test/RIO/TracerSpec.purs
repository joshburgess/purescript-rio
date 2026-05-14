module Test.RIO.TracerSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, fail, fork, join, provideAll, runRIO, runRIO')
import RIO.Test.Tracer (newRecordingTracer)
import RIO.Tracer
  ( SpanId
  , SpanStatus(..)
  , Tracer
  , addAttribute
  , currentSpan
  , noopTracer
  , withSpan
  )

spec :: Spec Unit
spec = describe "RIO.Tracer" do
  describe "SpanStatus instances" do
    -- The docstring distinguishes three terminal outcomes:
    -- SpanOk (happy path), SpanFailed (typed failure inside
    -- the action), and SpanInterrupted (fiber killed before
    -- the action completed). Pin the Show instance renders
    -- each constructor by its name so any log / OTel exporter
    -- that relies on `show status` to label a status code
    -- can't be silently broken.
    it "Show renders each status by its constructor name" do
      show SpanOk `shouldEqual` "SpanOk"
      show SpanFailed `shouldEqual` "SpanFailed"
      show SpanInterrupted `shouldEqual` "SpanInterrupted"

  it "withSpan opens and closes a span around a successful action" do
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) () Unit
      program = withSpan "outer" (pure unit)
    _ <- runRIO (provideAll { tracer: rec.tracer } program)
    spans <- liftEffect rec.snapshot
    Array.length spans `shouldEqual` 1
    case spans of
      [ s ] -> do
        s.name `shouldEqual` "outer"
        s.status `shouldEqual` SpanOk
        s.parent `shouldEqual` Nothing
        case s.endMs of
          Just _ -> pure unit
          Nothing -> 1 `shouldEqual` 0
      _ -> 1 `shouldEqual` 0

  it "snapshot taken mid-flight surfaces an open span with endMs = Nothing" do
    -- Docstring promise on the `Span` type: "`endMs` is `Nothing`
    -- while the span is still open; a recording backend may surface
    -- mid-flight spans this way for inspection." Every other
    -- TracerSpec test snapshots after the `withSpan` block has
    -- closed, so the `endMs` field is observed only on closed spans
    -- (always `Just _`). A regression that flipped the recorder to
    -- write `Just 0.0` at `startSpan` time (or eagerly close a span
    -- when its parent finishes) would still pass every other test
    -- because they never look at the recording while a span is open.
    -- Pin the open-span surface directly by snapshotting from inside
    -- the body of a `withSpan` and asserting the in-flight span has
    -- `endMs = Nothing` while a not-yet-opened sibling is absent.
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) () (Array { name :: String, endMs :: Maybe Number })
      program = withSpan "outer" do
        midFlight <- liftEffect rec.snapshot
        pure (map (\s -> { name: s.name, endMs: s.endMs }) midFlight)
    result <- runRIO (provideAll { tracer: rec.tracer } program)
    case result of
      Right rows ->
        rows `shouldEqual` [ { name: "outer", endMs: Nothing } ]
      Left _ -> 1 `shouldEqual` 0

  it "nested withSpan records parent/child correctly" do
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) () Unit
      program = withSpan "outer" do
        withSpan "inner" (pure unit)
    _ <- runRIO (provideAll { tracer: rec.tracer } program)
    spans <- liftEffect rec.snapshot
    case spans of
      [ outer, inner ] -> do
        outer.name `shouldEqual` "outer"
        inner.name `shouldEqual` "inner"
        outer.parent `shouldEqual` Nothing
        inner.parent `shouldEqual` Just outer.id
      _ -> 1 `shouldEqual` Array.length spans

  it "recording backend assigns deterministic monotonic ticks to startMs/endMs" do
    -- `RIO.Test.Tracer` module docstring promises: "Each
    -- operation increments a counter and uses the resulting
    -- integer as the span's `startMs` / `endMs`. This keeps
    -- the recorder fully deterministic." Existing TracerSpec
    -- tests only check `endMs` is `Just _` for closed spans
    -- and never inspect the timestamps. A regression that
    -- wired the recorder to a wall-clock source (or swapped
    -- `Ref.modify` for `Ref.write` in `nextTick`) would still
    -- mark every closed span with `Just _` but silently break
    -- the deterministic-ordering contract downstream tests
    -- rely on for log/replay assertions. Pin the four-step
    -- monotone sequence directly for a single nested span:
    -- outer opens at 1, inner opens at 2, inner closes at 3,
    -- outer closes at 4.
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) () Unit
      program = withSpan "outer" do
        withSpan "inner" (pure unit)
    _ <- runRIO (provideAll { tracer: rec.tracer } program)
    spans <- liftEffect rec.snapshot
    case spans of
      [ outer, inner ] -> do
        outer.startMs `shouldEqual` 1.0
        inner.startMs `shouldEqual` 2.0
        inner.endMs `shouldEqual` Just 3.0
        outer.endMs `shouldEqual` Just 4.0
      _ -> 1 `shouldEqual` Array.length spans

  it "currentSpan restores to the parent after the inner span ends" do
    rec <- liftAff newRecordingTracer
    let
      program
        :: RIO (tracer :: Tracer) ()
             { afterInner :: Maybe SpanId, afterOuter :: Maybe SpanId }
      program = withSpan "outer" do
        outerId <- currentSpan
        withSpan "inner" (pure unit)
        afterInner <- currentSpan
        pure { afterInner, afterOuter: outerId }
    result <- runRIO (provideAll { tracer: rec.tracer } program)
    case result of
      Right { afterInner, afterOuter } -> do
        afterInner `shouldEqual` afterOuter
      Left _ -> 1 `shouldEqual` 0

  it "marks a span SpanFailed when the action raises a typed failure" do
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) (boom :: Unit) Unit
      program = withSpan "outer" do
        fail (Proxy :: Proxy "boom") unit
    _ <- runRIO (provideAll { tracer: rec.tracer } program)
    spans <- liftEffect rec.snapshot
    case spans of
      [ s ] -> s.status `shouldEqual` SpanFailed
      _ -> 1 `shouldEqual` Array.length spans

  it "addAttribute attaches a key/value to the currently-active span" do
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) () Unit
      program = withSpan "outer" do
        addAttribute "request.id" "abc-123"
        addAttribute "user" "alice"
    _ <- runRIO (provideAll { tracer: rec.tracer } program)
    spans <- liftEffect rec.snapshot
    case spans of
      [ s ] ->
        s.attributes `shouldEqual`
          [ Tuple "request.id" "abc-123"
          , Tuple "user" "alice"
          ]
      _ -> 1 `shouldEqual` Array.length spans

  it "no active span: addAttribute is a no-op (does not crash)" do
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) () Unit
      program = addAttribute "stray" "value"
    _ <- runRIO (provideAll { tracer: rec.tracer } program)
    spans <- liftEffect rec.snapshot
    Array.length spans `shouldEqual` 0

  it "marks a span SpanInterrupted when the fiber is killed mid-action" do
    -- Docstring promise: "SpanInterrupted means the fiber was
    -- killed before the action completed". Pin this by forking
    -- a withSpan that delays, killing the fiber before the delay
    -- elapses, and asserting the recorded span closes with
    -- SpanInterrupted.
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) () Unit
      program = withSpan "outer" do
        liftAff (delay (Milliseconds 50.0))
    f <- forkAff (runRIO' (provideAll { tracer: rec.tracer } program))
    liftAff (delay (Milliseconds 5.0))
    killFiber (error "test-cancel") f
    liftAff (delay (Milliseconds 5.0))
    spans <- liftEffect rec.snapshot
    case spans of
      [ s ] -> do
        s.status `shouldEqual` SpanInterrupted
        case s.endMs of
          Just _ -> pure unit
          Nothing -> 1 `shouldEqual` 0
      _ -> 1 `shouldEqual` Array.length spans

  it "sibling spans share the same parent" do
    rec <- liftAff newRecordingTracer
    let
      program :: RIO (tracer :: Tracer) () Unit
      program = withSpan "outer" do
        withSpan "first" (pure unit)
        withSpan "second" (pure unit)
    _ <- runRIO' (provideAll { tracer: rec.tracer } program)
    spans <- liftEffect rec.snapshot
    case spans of
      [ outer, first, second ] -> do
        first.parent `shouldEqual` Just outer.id
        second.parent `shouldEqual` Just outer.id
        first.name `shouldEqual` "first"
        second.name `shouldEqual` "second"
      _ -> 1 `shouldEqual` Array.length spans

  describe "fork inheritance (implicit-context semantics)" do
    -- The module docstring promises: "A `fork`ed fiber inherits
    -- whichever span was current at the point of fork (because
    -- the same Tracer is shared by the underlying `Effect.Ref`s);
    -- after the fork, the parent fiber's subsequent spans land
    -- under the same parent until the parent's `endSpan` runs."
    -- Pin both halves of that contract.
    it "a forked fiber inherits the parent's current span at fork time" do
      rec <- liftAff newRecordingTracer
      let
        program :: RIO (tracer :: Tracer) () Unit
        program = withSpan "outer" do
          child <- fork (withSpan "from-child" (pure unit))
          join child
      _ <- runRIO (provideAll { tracer: rec.tracer } program)
      spans <- liftEffect rec.snapshot
      case spans of
        [ outer, child ] -> do
          outer.name `shouldEqual` "outer"
          child.name `shouldEqual` "from-child"
          child.parent `shouldEqual` Just outer.id
        _ -> 1 `shouldEqual` Array.length spans

    it "after fork, the parent's subsequent spans still land under the outer span" do
      rec <- liftAff newRecordingTracer
      let
        program :: RIO (tracer :: Tracer) () Unit
        program = withSpan "outer" do
          child <- fork (withSpan "from-child" (pure unit))
          join child
          withSpan "after-fork" (pure unit)
      _ <- runRIO (provideAll { tracer: rec.tracer } program)
      spans <- liftEffect rec.snapshot
      case spans of
        [ outer, child, afterFork ] -> do
          outer.name `shouldEqual` "outer"
          child.parent `shouldEqual` Just outer.id
          afterFork.parent `shouldEqual` Just outer.id
        _ -> 1 `shouldEqual` Array.length spans

  describe "noopTracer" do
    it "runs every span operation without crashing and records nothing" do
      let
        program :: RIO (tracer :: Tracer) () Unit
        program = withSpan "outer" do
          addAttribute "k" "v"
          withSpan "inner" (addAttribute "k2" "v2")
      result <- runRIO (provideAll { tracer: noopTracer } program)
      case result of
        Right _ -> pure unit
        Left _ -> 1 `shouldEqual` 0

    it "reports currentSpan as Nothing even inside a withSpan block" do
      -- noopTracer's startSpan returns SpanId 0 and currentSpan
      -- always returns Nothing, so a program reading currentSpan
      -- inside a span sees no active span. Pin this so any future
      -- change to noopTracer's bookkeeping is caught.
      let
        program :: RIO (tracer :: Tracer) () (Maybe SpanId)
        program = withSpan "outer" currentSpan
      result <- runRIO (provideAll { tracer: noopTracer } program)
      case result of
        Right inner -> inner `shouldEqual` Nothing
        Left _ -> 1 `shouldEqual` 0

    it "lets a typed failure inside withSpan surface unchanged" do
      -- noopTracer does no bookkeeping, but withSpan still wires
      -- startSpan/endSpan around the action through Aff.finally.
      -- Pin that a typed failure raised inside the body propagates
      -- on the parent's row.
      let
        program :: RIO (tracer :: Tracer) (boom :: Unit) Unit
        program = withSpan "outer" (fail (Proxy :: Proxy "boom") unit)
      result <- runRIO (provideAll { tracer: noopTracer } program)
      case result of
        Left _ -> pure unit
        Right _ -> 1 `shouldEqual` 0
