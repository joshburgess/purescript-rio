module Test.RIO.Fiber.AbortSignalSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.AbortSignal as Abort
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: AbortSignal" do
  it "delivers a value when the register callback resumes synchronously" do
    let
      prog :: F.RIO () () Int
      prog = Abort.asyncAbortable \_signal resume ->
        resume (Right 42)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "exposes a fresh signal that is not aborted at start" do
    abortedRef <- liftEffect (Ref.new true)
    let
      prog :: F.RIO () () Unit
      prog = Abort.asyncAbortable \signal resume -> do
        a <- Abort.isAborted signal
        Ref.write a abortedRef
        resume (Right unit)
    _ <- runAff prog {}
    final <- liftEffect (Ref.read abortedRef)
    final `shouldEqual` false

  it "aborts the signal when the fiber is interrupted" do
    signalRef <- liftEffect (Ref.new (Nothing :: _))
    let
      -- Stash the signal so we can inspect it after interruption,
      -- and never resume so the fiber stays parked.
      action :: F.RIO () () Unit
      action = Abort.asyncAbortable \signal _resume ->
        Ref.write (Just signal) signalRef

      prog :: F.RIO () () Boolean
      prog = do
        f <- F.fork action
        F.sleep (Milliseconds 5.0)
        F.interrupt f
        _ <- F.causeOf (F.join f)
        F.sleep (Milliseconds 5.0)
        F.liftEffect do
          m <- Ref.read signalRef
          case m of
            Just sig -> Abort.isAborted sig
            Nothing -> pure false
    out <- runAff prog {}
    case out of
      Success b -> b `shouldEqual` true
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "does not abort the signal on a normal successful resume" do
    abortedRef <- liftEffect (Ref.new false)
    let
      prog :: F.RIO () () Unit
      prog = do
        Abort.asyncAbortable \signal resume -> do
          -- Resume immediately; the signal must NOT abort because
          -- the fiber wasn't interrupted.
          resume (Right unit)
          -- Re-check synchronously: still false.
          a <- Abort.isAborted signal
          Ref.write a abortedRef
        F.sleep (Milliseconds 5.0)
    _ <- runAff prog {}
    final <- liftEffect (Ref.read abortedRef)
    final `shouldEqual` false

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
