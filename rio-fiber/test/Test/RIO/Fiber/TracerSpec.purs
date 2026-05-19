module Test.RIO.Fiber.TracerSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Type.Proxy (Proxy(..))
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Tracer (Span(..), Tracer(..))
import RIO.Fiber.Tracer as Tracer
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

type Recorded =
  { name :: String
  , attrs :: Array { key :: String, value :: String }
  , finished :: Boolean
  }

-- A recording tracer for tests: every started span lands in `ref`.
recordingTracer :: Ref.Ref (Array Recorded) -> Tracer
recordingTracer ref = Tracer
  { startSpan: \req -> do
      attrsRef <- Ref.new req.attributes
      idx <- do
        xs <- Ref.read ref
        let i = Array.length xs
        Ref.write
          (Array.snoc xs
            { name: req.name, attrs: req.attributes, finished: false })
          ref
        pure i
      pure
        ( Span
            { addAttribute: \k v -> do
                Ref.modify_ (\as -> Array.snoc as { key: k, value: v }) attrsRef
                attrs <- Ref.read attrsRef
                Ref.modify_
                  ( \xs -> case Array.modifyAt idx (\r -> r { attrs = attrs }) xs of
                      Just xs' -> xs'
                      _ -> xs
                  )
                  ref
            , finish: do
                Ref.modify_
                  ( \xs -> case Array.modifyAt idx (\r -> r { finished = true }) xs of
                      Just xs' -> xs'
                      _ -> xs
                  )
                  ref
            }
        )
  }

spec :: Spec Unit
spec = describe "rio-fiber: Tracer" do
  it "withSpan starts and finishes a span around the body" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    let
      prog :: F.RIO () () Int
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        Tracer.withSpan "outer" [] \_ -> pure 42
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)
    recorded <- liftEffect (Ref.read recordedRef)
    Array.length recorded `shouldEqual` 1
    case Array.head recorded of
      Just r -> do
        r.name `shouldEqual` "outer"
        r.finished `shouldEqual` true
      _ -> fail "expected at least one recorded span"

  it "addAttribute attaches to the active span" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    let
      prog :: F.RIO () () Unit
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        Tracer.withSpan "labeled" [] \span -> do
          Tracer.addAttribute span "k1" "v1"
          Tracer.addAttribute span "k2" "v2"
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    recorded <- liftEffect (Ref.read recordedRef)
    case Array.head recorded of
      Just r -> do
        Array.length r.attrs `shouldEqual` 2
        (map _.key r.attrs) `shouldEqual` [ "k1", "k2" ]
      _ -> fail "expected at least one recorded span"

  it "finishes the span even when the body fails" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    let
      prog :: F.RIO () () Int
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        F.catchAll (\_ -> pure 0) do
          Tracer.withSpan "doomed" [] \_ ->
            F.fail (Variant.inj (Proxy :: _ "boom") "kaboom")
    out <- runAff (prog :: F.RIO () () Int) {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    recorded <- liftEffect (Ref.read recordedRef)
    case Array.head recorded of
      Just r -> r.finished `shouldEqual` true
      _ -> fail "expected at least one recorded span"

  it "nested withSpan sees the outer span as its parent" do
    tracker <- liftEffect (Ref.new ([] :: Array (Maybe String)))
    let
      tracer = Tracer
        { startSpan: \req -> do
            parentName <- case req.parent of
              Nothing -> pure Nothing
              Just (Span p) -> do
                -- distinguish "has parent" from "no parent" by
                -- recording the request name + Just unit-ish marker
                p.addAttribute "child" req.name
                pure (Just req.name)
            Ref.modify_ (\xs -> Array.snoc xs parentName) tracker
            pure
              ( Span
                  { addAttribute: \_ _ -> pure unit
                  , finish: pure unit
                  }
              )
        }

      prog :: F.RIO () () Unit
      prog = Tracer.withTracer tracer do
        Tracer.withSpan "outer" [] \_ ->
          Tracer.withSpan "inner" [] \_ -> pure unit
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    seen <- liftEffect (Ref.read tracker)
    -- outer has no parent; inner sees outer
    seen `shouldEqual` [ Nothing, Just "inner" ]

  it "currentSpan reports the active span inside withSpan" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    observed <- liftEffect (Ref.new (false :: Boolean))
    let
      prog :: F.RIO () () Unit
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        Tracer.withSpan "active" [] \_ -> do
          ms <- Tracer.currentSpan
          F.liftEffect (Ref.write
            (case ms of
              Just _ -> true
              Nothing -> false)
            observed)
    _ <- runAff prog {}
    inside <- liftEffect (Ref.read observed)
    inside `shouldEqual` true

  it "currentSpan is Nothing outside any withSpan" do
    observed <- liftEffect (Ref.new (true :: Boolean))
    let
      prog :: F.RIO () () Unit
      prog = do
        ms <- Tracer.currentSpan
        F.liftEffect (Ref.write
          (case ms of
            Just _ -> true
            Nothing -> false)
          observed)
    _ <- runAff prog {}
    outside <- liftEffect (Ref.read observed)
    outside `shouldEqual` false

  it "forked fibers inherit the parent span" do
    seen <- liftEffect (Ref.new ([] :: Array (Maybe String)))
    let
      tracer = Tracer
        { startSpan: \req -> do
            Ref.modify_ (\xs -> Array.snoc xs (map (\_ -> req.name) req.parent)) seen
            pure
              ( Span
                  { addAttribute: \_ _ -> pure unit
                  , finish: pure unit
                  }
              )
        }

      prog :: F.RIO () () Unit
      prog = Tracer.withTracer tracer do
        Tracer.withSpan "parent" [] \_ -> do
          child <- F.fork do
            -- give the parent body a moment to settle before opening
            F.sleep (Milliseconds 1.0)
            Tracer.withSpan "child" [] \_ -> pure unit
          F.join child
    _ <- runAff prog {}
    xs <- liftEffect (Ref.read seen)
    -- parent has no parent; child sees the parent span
    xs `shouldEqual` [ Nothing, Just "child" ]

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
