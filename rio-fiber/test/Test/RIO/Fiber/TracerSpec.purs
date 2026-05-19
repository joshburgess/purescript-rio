module Test.RIO.Fiber.TracerSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
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
  { startSpan: \name initialAttrs -> do
      attrsRef <- Ref.new initialAttrs
      finishedRef <- Ref.new false
      -- find the slot we'll grow as the span runs
      idx <- do
        xs <- Ref.read ref
        let i = Array.length xs
        Ref.write
          (Array.snoc xs { name, attrs: initialAttrs, finished: false })
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
                Ref.write true finishedRef
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

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
