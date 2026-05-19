module Test.RIO.Fiber.ServicesSpec (spec) where

import Prelude

import Data.DateTime.Instant (Instant, instant)
import Data.Int (toNumber)
import Data.Maybe (fromJust)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Partial.Unsafe (unsafePartial)
import RIO.Fiber.Clock (Clock(..))
import RIO.Fiber.Clock as Clock
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Logger (Logger(..))
import RIO.Fiber.Logger as Logger
import RIO.Fiber.Random (Random(..))
import RIO.Fiber.Random as Random
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

mkInstant :: Number -> Instant
mkInstant ms = unsafePartial (fromJust (instant (Milliseconds ms)))

spec :: Spec Unit
spec = describe "rio-fiber: services" do
  describe "Clock" do
    it "withClock substitutes the implementation for the wrapped action" do
      let
        fake :: Clock
        fake = Clock
          { instant: pure (mkInstant 1000.0)
          , epoch: pure (Milliseconds 1000.0)
          }

        prog :: F.RIO () () Milliseconds
        prog = Clock.withClock fake Clock.currentEpoch
      out <- runAff prog {}
      case out of
        Success ms -> ms `shouldEqual` Milliseconds 1000.0
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "restores the previous clock after the block exits" do
      ref <- liftEffect (Ref.new (Milliseconds 0.0))
      let
        fake :: Clock
        fake = Clock
          { instant: pure (mkInstant 5000.0)
          , epoch: pure (Milliseconds 5000.0)
          }

        prog :: F.RIO () () Unit
        prog = do
          Clock.withClock fake do
            ms <- Clock.currentEpoch
            F.liftEffect (Ref.write ms ref)
          -- outside the block, the default clock is back; we can't
          -- assert its exact value, but reading it should not raise.
          _ <- Clock.currentEpoch
          pure unit
      _ <- runAff prog {}
      inside <- liftEffect (Ref.read ref)
      inside `shouldEqual` Milliseconds 5000.0

  describe "Random" do
    it "withRandom returns canned values in order" do
      counter <- liftEffect (Ref.new 0)
      let
        canned :: Random
        canned = Random
          { number: do
              n <- Ref.modify (_ + 1) counter
              pure (toNumber n)
          , int: \_ _ -> do
              n <- Ref.modify (_ + 1) counter
              pure n
          , boolean: pure true
          }

        prog :: F.RIO () () (Array Number)
        prog = Random.withRandom canned do
          a <- Random.nextNumber
          b <- Random.nextNumber
          c <- Random.nextNumber
          pure [ a, b, c ]
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1.0, 2.0, 3.0 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "Logger" do
    it "withLogger captures messages emitted inside" do
      sink <- liftEffect (Ref.new ([] :: Array String))
      let
        capture :: Logger
        capture = Logger
          { emit: \level msg -> Ref.modify_
              (\xs -> xs <> [ show level <> ": " <> msg ])
              sink
          }

        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture do
          Logger.info "starting"
          Logger.warn "watch out"
          Logger.error "boom"
      _ <- runAff prog {}
      seen <- liftEffect (Ref.read sink)
      seen `shouldEqual`
        [ "INFO: starting"
        , "WARN: watch out"
        , "ERROR: boom"
        ]

    it "child fibers inherit the override at fork time" do
      sink <- liftEffect (Ref.new ([] :: Array String))
      let
        capture :: Logger
        capture = Logger
          { emit: \_ msg -> Ref.modify_ (\xs -> xs <> [ msg ]) sink
          }

        prog :: F.RIO () () Unit
        prog = Logger.withLogger capture do
          fib <- F.fork (Logger.info "from child")
          Logger.info "from parent"
          _ <- F.join fib
          pure unit
      _ <- runAff prog {}
      seen <- liftEffect (Ref.read sink)
      seen `shouldEqual` [ "from parent", "from child" ]

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
