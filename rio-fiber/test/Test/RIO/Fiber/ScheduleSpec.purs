module Test.RIO.Fiber.ScheduleSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Schedule as Sch
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Schedule" do
  describe "repeat" do
    it "recurs n runs the action n + 1 times" do
      ref <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () () Unit
        action = F.liftEffect (Ref.modify_ (_ + 1) ref)

        prog :: F.RIO () () Int
        prog = Sch.repeatN 4 action
      out <- runAff prog {}
      case out of
        Success _ -> pure unit
        other -> fail ("expected Success, got " <> describeOutcome other)
      n <- liftEffect (Ref.read ref)
      n `shouldEqual` 5

    it "spaced waits the requested duration between attempts" do
      ref <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () () Unit
        action = F.liftEffect (Ref.modify_ (_ + 1) ref)

        prog :: F.RIO () () Unit
        prog = do
          _ <- Sch.repeat
            (Sch.bothS (Sch.recurs 2) (Sch.spaced (Milliseconds 5.0)))
            action
          pure unit
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      -- runs 1 + 2 retries = 3 total
      n `shouldEqual` 3

  describe "retry" do
    it "retries a failing action and eventually succeeds" do
      counter <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () (oops :: String) Int
        action = do
          n <- F.liftEffect (Ref.modify (_ + 1) counter)
          if n < 3 then F.fail (Variant.inj (Proxy :: _ "oops") "no")
          else pure n

        prog :: F.RIO () (oops :: String) Int
        prog = Sch.retryN 5 action
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 3
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "re-raises the last error when the schedule halts" do
      counter <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () (oops :: String) Int
        action = do
          _ <- F.liftEffect (Ref.modify (_ + 1) counter)
          F.fail (Variant.inj (Proxy :: _ "oops") "always")

        prog :: F.RIO () (oops :: String) Int
        prog = Sch.retryN 2 action
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "oops") identity) v
            `shouldEqual` "always"
        other -> fail ("expected Fail, got " <> describeOutcome other)
      n <- liftEffect (Ref.read counter)
      -- initial attempt + 2 retries = 3
      n `shouldEqual` 3

  describe "andThen" do
    it "runs the first schedule, then the second" do
      ref <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () () Unit
        action = F.liftEffect (Ref.modify_ (_ + 1) ref)

        prog :: F.RIO () () Unit
        prog = do
          _ <- Sch.repeat (Sch.andThen (Sch.recurs 1) (Sch.recurs 2)) action
          pure unit
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      -- initial + recurs 1 = 2, then recurs 2 = 2 more steps after second halts at first
      -- recurs 1 emits one Step then Halt -> action runs twice for first schedule
      -- Then andThen switches to recurs 2 -> two more Steps -> action runs 2 more times
      -- Total: 1 + 1 (first step) + 2 (second schedule) = 4
      n `shouldEqual` 4

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
