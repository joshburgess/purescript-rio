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
import RIO.Fiber.Tracer
  ( Span(..)
  , SpanId(..)
  , SpanKind(..)
  , SpanStatus(..)
  , Tracer(..)
  )
import RIO.Fiber.Tracer as Tracer
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

type RecordedEvent =
  { name :: String
  , attrs :: Array { key :: String, value :: String }
  }

type Recorded =
  { name :: String
  , kind :: SpanKind
  , attrs :: Array { key :: String, value :: String }
  , events :: Array RecordedEvent
  , linkCount :: Int
  , status :: SpanStatus
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
            { name: req.name
            , kind: req.kind
            , attrs: req.attributes
            , events: []
            , linkCount: 0
            , status: StatusUnset
            , finished: false
            })
          ref
        pure i
      let
        updateAt f =
          Ref.modify_
            ( \xs -> case Array.modifyAt idx f xs of
                Just xs' -> xs'
                _ -> xs
            )
            ref
      pure
        ( Span
            { spanId: SpanId (show idx)
            , addAttribute: \k v -> do
                Ref.modify_ (\as -> Array.snoc as { key: k, value: v }) attrsRef
                attrs <- Ref.read attrsRef
                updateAt (\r -> r { attrs = attrs })
            , addEvent: \evName evAttrs ->
                updateAt (\r -> r { events = Array.snoc r.events { name: evName, attrs: evAttrs } })
            , addLink: \_ ->
                updateAt (\r -> r { linkCount = r.linkCount + 1 })
            , setStatus: \status ->
                updateAt (\r -> r { status = status })
            , finish:
                updateAt (\r -> r { finished = true })
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
                  { spanId: SpanId req.name
                  , addAttribute: \_ _ -> pure unit
                  , addEvent: \_ _ -> pure unit
                  , addLink: \_ -> pure unit
                  , setStatus: \_ -> pure unit
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
                  { spanId: SpanId req.name
                  , addAttribute: \_ _ -> pure unit
                  , addEvent: \_ _ -> pure unit
                  , addLink: \_ -> pure unit
                  , setStatus: \_ -> pure unit
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

  it "addEvent records a timestamped event with attributes on the active span" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    let
      prog :: F.RIO () () Unit
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        Tracer.withSpan "event-holder" [] \span -> do
          Tracer.addEvent span "cache.miss" [ { key: "key", value: "user:42" } ]
          Tracer.addEvent span "retry" []
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    recorded <- liftEffect (Ref.read recordedRef)
    case Array.head recorded of
      Just r -> do
        (map _.name r.events) `shouldEqual` [ "cache.miss", "retry" ]
        case Array.head r.events of
          Just first -> (map _.key first.attrs) `shouldEqual` [ "key" ]
          _ -> fail "expected at least one event"
      _ -> fail "expected at least one recorded span"

  it "addLink attaches a non-parent reference between spans" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    let
      prog :: F.RIO () () Unit
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        Tracer.withSpan "linker" [] \lhs ->
          Tracer.withSpan "linked" [] \rhs ->
            Tracer.addLink lhs rhs
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    recorded <- liftEffect (Ref.read recordedRef)
    case Array.find (\r -> r.name == "linker") recorded of
      Just r -> r.linkCount `shouldEqual` 1
      _ -> fail "expected a recorded 'linker' span"

  it "setStatus updates the recorded span status" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    let
      prog :: F.RIO () () Unit
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        Tracer.withSpan "ok-span" [] \s ->
          Tracer.setStatus s StatusOk
        Tracer.withSpan "err-span" [] \s ->
          Tracer.setStatus s (StatusError "boom")
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)
    recorded <- liftEffect (Ref.read recordedRef)
    case Array.find (\r -> r.name == "ok-span") recorded of
      Just r -> r.status `shouldEqual` StatusOk
      _ -> fail "expected an 'ok-span' record"
    case Array.find (\r -> r.name == "err-span") recorded of
      Just r -> r.status `shouldEqual` StatusError "boom"
      _ -> fail "expected an 'err-span' record"

  it "default withSpan opens a span with Internal kind" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    let
      prog :: F.RIO () () Unit
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        Tracer.withSpan "default" [] \_ -> pure unit
    _ <- runAff prog {}
    recorded <- liftEffect (Ref.read recordedRef)
    case Array.head recorded of
      Just r -> r.kind `shouldEqual` Internal
      _ -> fail "expected at least one recorded span"

  it "withSpanWith forwards the requested SpanKind to the tracer" do
    recordedRef <- liftEffect (Ref.new ([] :: Array Recorded))
    let
      prog :: F.RIO () () Unit
      prog = Tracer.withTracer (recordingTracer recordedRef) do
        Tracer.withSpanWith
          { name: "inbound", attributes: [], kind: Server }
          \_ -> Tracer.withSpanWith
            { name: "outbound", attributes: [], kind: Client }
            \_ -> pure unit
    _ <- runAff prog {}
    recorded <- liftEffect (Ref.read recordedRef)
    (map (\r -> r.kind) recorded) `shouldEqual` [ Server, Client ]

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
