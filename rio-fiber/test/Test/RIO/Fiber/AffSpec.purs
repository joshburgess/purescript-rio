module Test.RIO.Fiber.AffSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Aff (Milliseconds(..), delay, error, throwError, try)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effect.Ref as Ref
import RIO.Fiber.Aff (fromAff, runAff, runAffEither, runAffThrow)
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

type Boom = (boom :: String)

spec :: Spec Unit
spec = describe "rio-fiber: Aff bridge" do
  describe "fromAff" do
    it "delivers an Aff success on the success channel" do
      let
        prog :: F.RIO () () Int
        prog = fromAff (pure 42)
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 42
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "surfaces an Aff throwError as a defect" do
      let
        prog :: F.RIO () () Int
        prog = fromAff (throwError (error "kaboom"))
      out <- runAff prog {}
      case out of
        Die e -> message e `shouldEqual` "kaboom"
        other -> fail ("expected Die, got " <> describeOutcome other)

    it "kills the embedded Aff when the surrounding RIO is cancelled" do
      ref <- liftEffect (Ref.new "running")
      let
        slow = do
          delay (Milliseconds 200.0)
          liftEffect (Ref.write "finished" ref)

        prog :: F.RIO () () Unit
        prog = fromAff slow

      cancel <- liftEffect
        (F.runRIOCallback prog {} (\_ -> pure unit))
      delay (Milliseconds 50.0)
      liftEffect cancel
      delay (Milliseconds 300.0)
      seen <- liftEffect (Ref.read ref)
      seen `shouldEqual` "running"

  describe "runAff" do
    it "returns Success on a pure program" do
      out <- runAff (pure 7 :: F.RIO () () Int) {}
      case out of
        Success n -> n `shouldEqual` 7
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "returns Fail on a typed failure" do
      let
        prog :: F.RIO () Boom Int
        prog = F.fail (Variant.inj (Proxy :: Proxy "boom") "nope")
      out <- runAff prog {}
      case out of
        Fail v ->
          Variant.match { boom: \s -> s `shouldEqual` "nope" } v
        other -> fail ("expected Fail, got " <> describeOutcome other)

    it "returns Die on a synchronous defect" do
      let
        prog :: F.RIO () () Int
        prog = F.die (error "boom")
      out <- runAff prog {}
      case out of
        Die e -> message e `shouldEqual` "boom"
        other -> fail ("expected Die, got " <> describeOutcome other)

  describe "runAffEither" do
    it "projects Success onto Right" do
      r <- runAffEither (pure 1 :: F.RIO () () Int) {}
      r `shouldEqual` Right 1

    it "projects Fail onto Left" do
      let
        prog :: F.RIO () Boom Int
        prog = F.fail (Variant.inj (Proxy :: Proxy "boom") "x")
      r <- runAffEither prog {}
      case r of
        Left v ->
          Variant.match { boom: \s -> s `shouldEqual` "x" } v
        Right _ -> fail "expected Left"

    it "raises a defect on Aff's error channel" do
      let
        prog :: F.RIO () () Int
        prog = F.die (error "defect")
      attempt <- try (runAffEither prog {})
      case attempt of
        Left e -> message e `shouldEqual` "defect"
        Right _ -> fail "expected an Aff exception"

  describe "runAffThrow" do
    it "unwraps Success to the bare value" do
      n <- runAffThrow (pure 99 :: F.RIO () () Int)
      n `shouldEqual` 99

    it "raises a defect on Aff's error channel" do
      let
        prog :: F.RIO () () Int
        prog = F.die (error "thrown")
      attempt <- try (runAffThrow prog)
      case attempt of
        Left e -> message e `shouldEqual` "thrown"
        Right _ -> fail "expected an Aff exception"

  describe "round-trip" do
    it "Aff -> RIO -> Aff preserves a pure value" do
      n <- runAffThrow (fromAff (pure 13 :: _ Int))
      n `shouldEqual` 13

    it "threads a Ref write across the bridge" do
      ref <- liftEffect (Ref.new 0)
      runAffThrow (fromAff (liftEffect (Ref.write 5 ref)))
      v <- liftEffect (Ref.read ref)
      v `shouldEqual` 5

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
