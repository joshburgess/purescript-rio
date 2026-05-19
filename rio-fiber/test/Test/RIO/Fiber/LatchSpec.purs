module Test.RIO.Fiber.LatchSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Latch as Latch
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: Latch" do
  it "await returns immediately when the latch starts at zero" do
    l <- liftEffect (Latch.make 0)
    let
      prog :: F.RIO () () Unit
      prog = Latch.await l
    out <- runAff prog {}
    case out of
      Success _ -> pure unit
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "await suspends until count reaches zero" do
    l <- liftEffect (Latch.make 3)
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: String -> F.RIO () () Unit
      record m = F.liftEffect (Ref.modify_ (\xs -> xs <> [ m ]) log)

      prog :: F.RIO () () Unit
      prog = do
        waiter <- F.fork do
          Latch.await l
          record "released"
        F.sleep (Milliseconds 5.0)
        record "before-1"
        Latch.countDown l
        record "before-2"
        Latch.countDown l
        record "before-3"
        Latch.countDown l
        _ <- F.join waiter
        pure unit
    _ <- runAff prog {}
    seen <- liftEffect (Ref.read log)
    seen `shouldEqual`
      [ "before-1", "before-2", "before-3", "released" ]

  it "count reports remaining and isOpen flips at zero" do
    l <- liftEffect (Latch.make 2)
    let
      prog :: F.RIO () () { c0 :: Int, open0 :: Boolean, c1 :: Int, c2 :: Int, open2 :: Boolean }
      prog = do
        c0 <- Latch.count l
        open0 <- Latch.isOpen l
        Latch.countDown l
        c1 <- Latch.count l
        Latch.countDown l
        c2 <- Latch.count l
        open2 <- Latch.isOpen l
        pure { c0, open0, c1, c2, open2 }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.c0 `shouldEqual` 2
        r.open0 `shouldEqual` false
        r.c1 `shouldEqual` 1
        r.c2 `shouldEqual` 0
        r.open2 `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "countDown past zero is a no-op" do
    l <- liftEffect (Latch.make 1)
    let
      prog :: F.RIO () () Int
      prog = do
        Latch.countDown l
        Latch.countDown l
        Latch.countDown l
        Latch.count l
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "many concurrent waiters all fire on the final countDown" do
    l <- liftEffect (Latch.make 1)
    fires <- liftEffect (Ref.new 0)
    let
      waiter :: F.RIO () () Unit
      waiter = do
        Latch.await l
        F.liftEffect (Ref.modify_ (_ + 1) fires)

      prog :: F.RIO () () Unit
      prog = do
        f1 <- F.fork waiter
        f2 <- F.fork waiter
        f3 <- F.fork waiter
        F.sleep (Milliseconds 5.0)
        Latch.countDown l
        _ <- F.join f1
        _ <- F.join f2
        _ <- F.join f3
        pure unit
    _ <- runAff prog {}
    n <- liftEffect (Ref.read fires)
    n `shouldEqual` 3

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
