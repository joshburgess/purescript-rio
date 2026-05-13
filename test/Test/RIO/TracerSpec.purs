module Test.RIO.TracerSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
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
