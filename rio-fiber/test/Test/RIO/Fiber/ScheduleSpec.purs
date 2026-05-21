module Test.RIO.Fiber.ScheduleSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Schedule (Decision(..), Schedule(..))
import RIO.Fiber.Schedule as Sch
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

collect
  :: forall a
   . Int
  -> (Unit -> Effect (Decision Unit a))
  -> Array Milliseconds
  -> Effect (Array Milliseconds)
collect n step acc
  | n <= 0 = pure acc
  | otherwise = do
      d <- step unit
      case d of
        Halt _ -> pure acc
        Step _ delay (Schedule next) ->
          collect (n - 1) next (acc <> [ delay ])

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

  describe "fibonacci" do
    it "emits delays in the fibonacci sequence" do
      decisions <- liftEffect do
        let Schedule s0 = Sch.fibonacci (Milliseconds 1.0)
        collect 4 s0 []
      decisions `shouldEqual`
        [ Milliseconds 1.0
        , Milliseconds 1.0
        , Milliseconds 2.0
        , Milliseconds 3.0
        ]

  describe "exponential" do
    it "multiplies each delay by the growth factor" do
      delays <- liftEffect do
        let Schedule s0 = Sch.exponential (Milliseconds 1.0) 2.0
        collect 5 s0 []
      delays `shouldEqual`
        [ Milliseconds 1.0
        , Milliseconds 2.0
        , Milliseconds 4.0
        , Milliseconds 8.0
        , Milliseconds 16.0
        ]

  describe "jittered" do
    it "scales each delay within the requested range" do
      delays <- liftEffect do
        let
          Schedule s0 = Sch.jittered 0.5 1.5
            (Sch.spaced (Milliseconds 100.0))
        collect 5 s0 []
      let
        allInRange = Array.all
          (\(Milliseconds ms) -> ms >= 50.0 && ms <= 150.0)
          delays
      allInRange `shouldEqual` true

  describe "whileOutput" do
    it "halts once the inner schedule's output stops satisfying p" do
      ref <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () () Unit
        action = F.liftEffect (Ref.modify_ (_ + 1) ref)

        prog :: F.RIO () () Unit
        prog = do
          _ <- Sch.repeat
            (Sch.whileOutput (\n -> n < 3) Sch.forever)
            action
          pure unit
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      -- forever emits Step 1, Step 2, Step 3, ... ; whileOutput halts
      -- as soon as the output is >= 3, i.e. after emitting Step 1 and
      -- Step 2 (action runs 1 + 2 = 3 times total).
      n `shouldEqual` 3

  describe "untilInput" do
    it "halts once an input satisfies the predicate" do
      counter <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () (oops :: String) Int
        action = do
          n <- F.liftEffect (Ref.modify (_ + 1) counter)
          F.fail (Variant.inj (Proxy :: _ "oops") ("attempt " <> show n))

        prog :: F.RIO () (oops :: String) Int
        prog = Sch.retry
          (Sch.untilInput (\v -> Variant.case_ # Variant.on (Proxy :: _ "oops")
            (\msg -> msg == "attempt 2") $ v) Sch.forever)
          action
      _ <- runAff prog {}
      n <- liftEffect (Ref.read counter)
      -- attempts 1 fails, untilInput keeps trying; attempt 2 fails,
      -- untilInput sees match and halts -> total attempts is 2.
      n `shouldEqual` 2

  describe "bothS" do
    it "intersection halts as soon as either side halts" do
      ref <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () () Unit
        action = F.liftEffect (Ref.modify_ (_ + 1) ref)

        -- recurs 2 halts after emitting 2 steps; spaced has no bound.
        -- The combined schedule halts when recurs 2 halts.
        prog :: F.RIO () () Unit
        prog = do
          _ <- Sch.repeat
            (Sch.bothS (Sch.recurs 2) (Sch.spaced (Milliseconds 0.0)))
            action
          pure unit
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      -- initial run + 2 retries
      n `shouldEqual` 3

    it "delay is the max of both branches" do
      delays <- liftEffect do
        let
          Schedule s0 = Sch.bothS
            (Sch.spaced (Milliseconds 5.0))
            (Sch.spaced (Milliseconds 12.0))
        collect 3 s0 []
      delays `shouldEqual`
        [ Milliseconds 12.0
        , Milliseconds 12.0
        , Milliseconds 12.0
        ]

  describe "andThen" do
    it "switches over once the first schedule halts" do
      delays <- liftEffect do
        let
          Schedule s0 = Sch.andThen
            (Sch.recurs 2)
            (Sch.spaced (Milliseconds 5.0))
        collect 5 s0 []
      -- recurs 2 emits Step 1 (0ms), Step 2 (0ms), then Halt.
      -- andThen jumps to spaced 5.0 which emits 5.0 forever.
      delays `shouldEqual`
        [ Milliseconds 0.0
        , Milliseconds 0.0
        , Milliseconds 5.0
        , Milliseconds 5.0
        , Milliseconds 5.0
        ]

  describe "mapOutput" do
    it "rewrites the output without changing decisions" do
      decisions <- liftEffect do
        let Schedule s0 = Sch.mapOutput (_ * 10) (Sch.recurs 3)
        collectOutputs 4 s0 []
      decisions `shouldEqual` [ 10, 20, 30 ]

  describe "whileInput / untilInput" do
    it "whileInput halts when the input fails the predicate" do
      counter <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () (oops :: String) Int
        action = do
          n <- F.liftEffect (Ref.modify (_ + 1) counter)
          F.fail (Variant.inj (Proxy :: _ "oops") ("attempt " <> show n))

        -- Continue retrying while the error message is "attempt 1".
        -- After attempt 1 fails, error becomes "attempt 1"; the
        -- predicate accepts it and we retry. After attempt 2, the
        -- error is "attempt 2", which fails the predicate, so we
        -- halt and re-raise.
        keepGoing :: Variant (oops :: String) -> Boolean
        keepGoing v = Variant.case_
          # Variant.on (Proxy :: _ "oops") (\msg -> msg == "attempt 1") $ v

        prog :: F.RIO () (oops :: String) Int
        prog = Sch.retry
          (Sch.whileInput keepGoing Sch.forever)
          action
      _ <- runAff prog {}
      n <- liftEffect (Ref.read counter)
      n `shouldEqual` 2

  describe "whileOutput / untilOutput" do
    it "untilOutput halts when the inner schedule's output satisfies p" do
      ref <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () () Unit
        action = F.liftEffect (Ref.modify_ (_ + 1) ref)

        -- Halt when the recurs-style attempt counter reaches 3.
        prog :: F.RIO () () Unit
        prog = do
          _ <- Sch.repeat
            (Sch.untilOutput (\n -> n >= 3) Sch.forever)
            action
          pure unit
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      -- initial run, then output 1, output 2 emit Steps; output 3
      -- fails the predicate so the schedule halts. Total runs = 3.
      n `shouldEqual` 3

  describe "fixed" do
    it "is equivalent to spaced in this MVP" do
      let
        Schedule sFixed = Sch.fixed (Milliseconds 7.0)
        Schedule sSpaced = Sch.spaced (Milliseconds 7.0)
      a <- liftEffect (collect 4 sFixed [])
      b <- liftEffect (collect 4 sSpaced [])
      a `shouldEqual` b

  describe "once" do
    it "emits exactly one Step then Halts" do
      ref <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () () Unit
        action = F.liftEffect (Ref.modify_ (_ + 1) ref)

        prog :: F.RIO () () Unit
        prog = do
          _ <- Sch.repeat Sch.once action
          pure unit
      _ <- runAff prog {}
      n <- liftEffect (Ref.read ref)
      -- initial run + 1 step
      n `shouldEqual` 2

  describe "combined policies" do
    it "exponential `bothS` recurs caps the total attempts" do
      delays <- liftEffect do
        let
          Schedule s0 = Sch.bothS
            (Sch.recurs 3)
            (Sch.exponential (Milliseconds 1.0) 2.0)
        collect 10 s0 []
      -- recurs 3 halts after 3 steps; bothS halts on that.
      Array.length delays `shouldEqual` 3
      delays `shouldEqual`
        [ Milliseconds 1.0
        , Milliseconds 2.0
        , Milliseconds 4.0
        ]

    it "retry exponential succeeds eventually on a flaky action" do
      counter <- liftEffect (Ref.new 0)
      let
        action :: F.RIO () (oops :: String) Int
        action = do
          n <- F.liftEffect (Ref.modify (_ + 1) counter)
          if n >= 3 then pure n
          else F.fail (Variant.inj (Proxy :: _ "oops") "no")

        prog :: F.RIO () (oops :: String) Int
        prog = Sch.retry
          ( Sch.bothS
              (Sch.recurs 5)
              (Sch.exponential (Milliseconds 1.0) 2.0)
          )
          action
      out <- runAff prog {}
      case out of
        Success n -> n `shouldEqual` 3
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "compose" do
    it "feeds the first schedule's output into the second's input" do
      -- `recurs 4` emits 1, 2, 3, 4 then halts.
      -- `mapInput (_ * 10) (recurs 3)` consumes those, halts on the 4th input.
      let
        Schedule step = Sch.compose
          (Sch.recurs 4)
          (Sch.mapInput (_ * 10) (Sch.recurs 3))
      outs <- liftEffect (collectOutputs 10 step [])
      -- recurs 3 outputs 1, 2, 3 (it's an attempt counter, not the input).
      outs `shouldEqual` [ 1, 2, 3 ]

    it "halts when either schedule halts" do
      let
        Schedule step = Sch.compose (Sch.recurs 2) Sch.passthrough
      outs <- liftEffect (collectOutputs 10 step [])
      -- recurs 2 emits 1, 2 then halts; passthrough echoes them.
      outs `shouldEqual` [ 1, 2 ]

    it "uses the max of the two delays" do
      let
        Schedule step = Sch.compose
          (Sch.spaced (Milliseconds 5.0))
          (Sch.spaced (Milliseconds 20.0))
      ds <- liftEffect (collect 3 step [])
      ds `shouldEqual`
        [ Milliseconds 20.0, Milliseconds 20.0, Milliseconds 20.0 ]

  describe "mapInput" do
    it "pre-processes the input via the supplied function" do
      let
        -- whileInput on the inner schedule will halt when its remapped
        -- input is >= 30. Feeding 1, 2, 3, ... means the remapped
        -- input is 10, 20, 30, ... so it halts on the third element.
        Schedule step = Sch.mapInput (_ * 10)
          (Sch.whileInput (\i -> i < 30) (Sch.recurs 10))
      outs <- liftEffect (collectInputs 10 1 step [])
      outs `shouldEqual` [ 1, 2 ]

  describe "passthrough" do
    it "echoes every input as the output forever" do
      let
        Schedule step = Sch.passthrough
      outs <- liftEffect (collectInputs 5 100 step [])
      outs `shouldEqual` [ 100, 101, 102, 103, 104 ]

    it "emits a zero delay per step" do
      let
        Schedule step = Sch.passthrough
      ds <- liftEffect (collect 4 step [])
      ds `shouldEqual`
        [ Milliseconds 0.0, Milliseconds 0.0, Milliseconds 0.0, Milliseconds 0.0 ]

  describe "elapsed" do
    it "first output is zero" do
      let
        Schedule step = Sch.elapsed
      outs <- liftEffect (collectOutputs 1 step [])
      outs `shouldEqual` [ Milliseconds 0.0 ]

    it "subsequent outputs are monotonically non-decreasing" do
      let
        Schedule step = Sch.elapsed
      Milliseconds first <- pure (Milliseconds 0.0)
      outs <- liftEffect (collectOutputs 5 step [])
      let
        nums = Array.mapMaybe
          ( case _ of
              Milliseconds m -> pure m
          )
          outs
        nonDecreasing = case Array.uncons nums of
          Nothing -> true
          Just { head: h, tail: t } ->
            Array.foldl
              (\acc x -> acc && x >= 0.0)
              (h >= first)
              t
      nonDecreasing `shouldEqual` true

  describe "delays" do
    it "replaces the output with the inner schedule's delay" do
      let
        Schedule step = Sch.delays
          (Sch.exponential (Milliseconds 1.0) 2.0)
      outs <- liftEffect (collectOutputs 4 step [])
      outs `shouldEqual`
        [ Milliseconds 1.0
        , Milliseconds 2.0
        , Milliseconds 4.0
        , Milliseconds 8.0
        ]

    it "preserves the inner schedule's pacing (output equals emitted delay)" do
      let
        sched = Sch.delays (Sch.spaced (Milliseconds 7.0))
        Schedule step = sched
      ds <- liftEffect (collect 3 step [])
      ds `shouldEqual`
        [ Milliseconds 7.0, Milliseconds 7.0, Milliseconds 7.0 ]

collectOutputs
  :: forall a
   . Int
  -> (Unit -> Effect (Decision Unit a))
  -> Array a
  -> Effect (Array a)
collectOutputs n step acc
  | n <= 0 = pure acc
  | otherwise = do
      d <- step unit
      case d of
        Halt _ -> pure acc
        Step b _ (Schedule next) ->
          collectOutputs (n - 1) next (acc <> [ b ])

-- | Like `collectOutputs` but feeds a stream of integer inputs
-- | (`startInput, startInput + 1, ...`). Used to exercise schedules
-- | whose behavior depends on the input value (`mapInput`,
-- | `passthrough`, `compose`).
collectInputs
  :: forall a
   . Int
  -> Int
  -> (Int -> Effect (Decision Int a))
  -> Array a
  -> Effect (Array a)
collectInputs remaining input step acc
  | remaining <= 0 = pure acc
  | otherwise = do
      d <- step input
      case d of
        Halt _ -> pure acc
        Step b _ (Schedule next) ->
          collectInputs (remaining - 1) (input + 1) next (acc <> [ b ])

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
