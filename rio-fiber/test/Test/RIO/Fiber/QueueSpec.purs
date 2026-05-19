module Test.RIO.Fiber.QueueSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Queue as Q
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Queue" do
  it "offer then take round-trips" do
    q <- liftEffect (Q.make 4 :: _ (Q.Queue Int))
    let
      prog :: F.RIO () () Int
      prog = do
        Q.offer q 7
        Q.take q
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 7
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "preserves FIFO order" do
    q <- liftEffect (Q.make 8 :: _ (Q.Queue Int))
    let
      prog :: F.RIO () () (Array Int)
      prog = do
        Q.offer q 1
        Q.offer q 2
        Q.offer q 3
        traverse (\_ -> Q.take q) [ unit, unit, unit ]
    out <- runAff prog {}
    case out of
      Success xs -> xs `shouldEqual` [ 1, 2, 3 ]
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "take suspends until a value is offered" do
    q <- liftEffect (Q.make 1 :: _ (Q.Queue Int))
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: String -> F.RIO () () Unit
      record m = F.liftEffect (Ref.modify_ (\xs -> xs <> [ m ]) log)

      prog :: F.RIO () () Int
      prog = do
        fib <- F.fork do
          n <- Q.take q
          record ("got " <> show n)
          pure n
        F.sleep (Milliseconds 10.0)
        record "offering"
        Q.offer q 42
        F.join fib
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual` [ "offering", "got 42" ]

  it "offer suspends when full and resumes when a take frees space" do
    q <- liftEffect (Q.make 2 :: _ (Q.Queue Int))
    let
      prog :: F.RIO () () Int
      prog = do
        Q.offer q 1
        Q.offer q 2
        fib <- F.fork (Q.offer q 3)
        F.sleep (Milliseconds 5.0)
        a <- Q.take q
        _ <- F.join fib
        b <- Q.take q
        c <- Q.take q
        pure (a * 100 + b * 10 + c)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 123
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryOffer returns false on a full queue with no takers" do
    q <- liftEffect (Q.make 1 :: _ (Q.Queue Int))
    let
      prog :: F.RIO () () { a :: Boolean, b :: Boolean }
      prog = do
        a <- Q.tryOffer q 1
        b <- Q.tryOffer q 2
        pure { a, b }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.a `shouldEqual` true
        r.b `shouldEqual` false
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "tryTake returns Nothing on an empty queue" do
    q <- liftEffect (Q.make 4 :: _ (Q.Queue Int))
    let
      prog :: F.RIO () () (Maybe Int)
      prog = Q.tryTake q
    out <- runAff prog {}
    case out of
      Success r -> r `shouldEqual` Nothing
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "delivers an offer directly to a waiting taker" do
    q <- liftEffect (Q.make 1 :: _ (Q.Queue Int))
    let
      prog :: F.RIO () () Int
      prog = do
        fib <- F.fork (Q.take q)
        F.sleep (Milliseconds 5.0)
        Q.offer q 99
        F.join fib
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 99
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "interrupting a waiting take does not strand the next offer" do
    q <- liftEffect (Q.make 1 :: _ (Q.Queue Int))
    let
      prog :: F.RIO () () Int
      prog = do
        ghost <- F.fork (Q.take q)
        F.sleep (Milliseconds 5.0)
        F.interrupt ghost
        _ <- F.causeOf (F.join ghost)
        -- the offer should land in the queue, not into the ghost
        Q.offer q 42
        Q.take q
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
