module Test.RIO.Aff.AbortSignalSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay) as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.AbortSignal (AbortSignal)
import RIO.Aff.AbortSignal as Abort
import RIO.Aff.Cause (attemptCause)
import RIO.Aff.Concurrency (fork, interrupt)
import RIO.Aff.Core (RIO, runRIO')

spec :: Spec Unit
spec = describe "RIO.Aff.AbortSignal" do
  it "delivers a value when the register callback resumes synchronously" do
    let
      program :: RIO () () Int
      program = Abort.asyncAbortable \_signal resume ->
        resume (Right 42)
    n <- runRIO' program
    n `shouldEqual` 42

  it "exposes a fresh signal that is not aborted at start" do
    abortedRef <- liftEffect (Ref.new true)
    let
      program :: RIO () () Unit
      program = Abort.asyncAbortable \signal resume -> do
        a <- Abort.isAborted signal
        Ref.write a abortedRef
        resume (Right unit)
    _ <- runRIO' program
    final <- liftEffect (Ref.read abortedRef)
    final `shouldEqual` false

  it "aborts the signal when the fiber is interrupted" do
    signalRef <- liftEffect (Ref.new (Nothing :: Maybe AbortSignal))
    let
      action :: RIO () () Unit
      action = Abort.asyncAbortable \signal _resume ->
        Ref.write (Just signal) signalRef

      program :: RIO () () Boolean
      program = do
        f <- fork action
        liftAff (Aff.delay (Milliseconds 5.0))
        interrupt f
        liftAff (Aff.delay (Milliseconds 5.0))
        liftEffect do
          m <- Ref.read signalRef
          case m of
            Just sig -> Abort.isAborted sig
            Nothing -> pure false
    b <- runRIO' program
    b `shouldEqual` true

  it "does not abort the signal on a normal successful resume" do
    abortedRef <- liftEffect (Ref.new false)
    let
      program :: RIO () () Unit
      program = do
        Abort.asyncAbortable \signal resume -> do
          resume (Right unit)
          a <- Abort.isAborted signal
          Ref.write a abortedRef
        liftAff (Aff.delay (Milliseconds 5.0))
    _ <- runRIO' program
    final <- liftEffect (Ref.read abortedRef)
    final `shouldEqual` false
