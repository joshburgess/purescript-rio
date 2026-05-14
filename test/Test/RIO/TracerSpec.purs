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

import RIO.Core (RIO, fail, provideAll, runRIO, runRIO')
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
