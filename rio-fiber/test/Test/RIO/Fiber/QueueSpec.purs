module Test.RIO.Fiber.QueueSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Foldable (traverse_)
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

  describe "unbounded" do
    it "accepts every offer without suspending" do
      q <- liftEffect (Q.unbounded :: _ (Q.Queue Int))
      let
        prog :: F.RIO () () (Array Int)
        prog = do
          traverse_ (Q.offer q) [ 1, 2, 3, 4, 5 ]
          traverse (\_ -> Q.take q) [ 1, 2, 3, 4, 5 ]
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2, 3, 4, 5 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "tryOffer always returns true" do
      q <- liftEffect (Q.unbounded :: _ (Q.Queue Int))
      let
        prog :: F.RIO () () (Array Boolean)
        prog = traverse (Q.tryOffer q) [ 1, 2, 3 ]
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ true, true, true ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "dropping" do
    it "drops the offered element when full" do
      q <- liftEffect (Q.dropping 2 :: _ (Q.Queue Int))
      let
        prog :: F.RIO () () (Array Int)
        prog = do
          Q.offer q 1
          Q.offer q 2
          Q.offer q 3 -- dropped
          Q.offer q 4 -- dropped
          a <- Q.take q
          b <- Q.take q
          pure [ a, b ]
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 1, 2 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "tryOffer returns false when dropping" do
      q <- liftEffect (Q.dropping 1 :: _ (Q.Queue Int))
      let
        prog :: F.RIO () () (Array Boolean)
        prog = do
          ok1 <- Q.tryOffer q 1
          ok2 <- Q.tryOffer q 2 -- full, dropped
          pure [ ok1, ok2 ]
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ true, false ]
        other -> fail ("expected Success, got " <> describeOutcome other)

  describe "sliding" do
    it "drops the oldest stored element to make room" do
      q <- liftEffect (Q.sliding 2 :: _ (Q.Queue Int))
      let
        prog :: F.RIO () () (Array Int)
        prog = do
          Q.offer q 1
          Q.offer q 2
          Q.offer q 3 -- drops 1, keeps [2,3]
          Q.offer q 4 -- drops 2, keeps [3,4]
          a <- Q.take q
          b <- Q.take q
          pure [ a, b ]
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ 3, 4 ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "tryOffer always returns true even when full" do
      q <- liftEffect (Q.sliding 1 :: _ (Q.Queue Int))
      let
        prog :: F.RIO () () (Array Boolean)
        prog = do
          ok1 <- Q.tryOffer q 1
          ok2 <- Q.tryOffer q 2 -- evicts 1
          pure [ ok1, ok2 ]
      out <- runAff prog {}
      case out of
        Success xs -> xs `shouldEqual` [ true, true ]
        other -> fail ("expected Success, got " <> describeOutcome other)

    it "delivers directly to a waiting taker before sliding" do
      q <- liftEffect (Q.sliding 1 :: _ (Q.Queue Int))
      log <- liftEffect (Ref.new (Nothing :: Maybe Int))
      let
        prog :: F.RIO () () Unit
        prog = do
          waiter <- F.fork do
            n <- Q.take q
            F.liftEffect (Ref.write (Just n) log)
          F.sleep (Milliseconds 5.0)
          Q.offer q 7
          F.join waiter
      out <- runAff prog {}
      case out of
        Success _ -> do
          got <- liftEffect (Ref.read log)
          got `shouldEqual` Just 7
        other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
