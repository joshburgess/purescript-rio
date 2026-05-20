module Test.RIO.Aff.Concurrency.ParSpec (spec) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Variant as Variant
import Effect.Aff (Milliseconds(..), attempt, delay, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now) as Now
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions (fail) as Spec
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, asks, die, fail, provideAll, runRIO)
import RIO.Aff.Concurrency.Par as Par

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Concurrency.Par" do
    describe "Par.ado" do
      it "runs branches concurrently (3x 100ms in ~one slot)" do
        let
          slow ms label = do
            liftAff (delay (Milliseconds ms))
            pure label

          program :: RIO () () { a :: String, b :: String, c :: String }
          program = Par.ado
            a <- slow 100.0 "A"
            b <- slow 100.0 "B"
            c <- slow 100.0 "C"
            in { a, b, c }

        start <- liftEffect Now.now
        result <- runRIO program
        end <- liftEffect Now.now
        let elapsed = unwrap (unInstant end) - unwrap (unInstant start)
        result `shouldEqual` Right { a: "A", b: "B", c: "C" }
        -- generous upper bound: parallel should finish well under the
        -- 300ms a sequential version would take
        when (elapsed >= 250.0) do
          Spec.fail
            ( "expected parallel block to finish in well under 250ms, took "
                <> show elapsed
                <> "ms"
            )

      it "leftmost typed failure wins; later branches still run to completion" do
        let
          program :: RIO () (boom :: String) Int
          program = Par.ado
            a <- fail (Proxy :: Proxy "boom") "first"
            b <- (liftAff (delay (Milliseconds 10.0)) *> pure 2)
            in a + b

        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected typed failure"

      it "no short-circuit: right branch runs to completion even when left fails" do
        rightSteps <- liftEffect (Ref.new 0)
        let
          rightWork :: RIO () (boom :: Unit) Int
          rightWork = do
            liftEffect (Ref.modify_ (_ + 1) rightSteps)
            liftAff (delay (Milliseconds 10.0))
            liftEffect (Ref.modify_ (_ + 1) rightSteps)
            liftAff (delay (Milliseconds 10.0))
            liftEffect (Ref.modify_ (_ + 1) rightSteps)
            pure 7

          program :: RIO () (boom :: Unit) Int
          program = Par.ado
            a <- fail (Proxy :: Proxy "boom") unit
            b <- rightWork
            in a + b

        _ <- runRIO program
        steps <- liftEffect (Ref.read rightSteps)
        steps `shouldEqual` 3

      it "right typed failure surfaces when the left branch succeeds" do
        let
          program :: RIO () (right :: String) Int
          program = Par.ado
            a <- pure 10
            b <- fail (Proxy :: Proxy "right") "from-right"
            in a + b

        result <- runRIO program
        case result of
          Left v ->
            (Variant.case_ # Variant.on (Proxy :: Proxy "right") identity $ v)
              `shouldEqual` "from-right"
          Right _ -> Spec.fail "expected right-branch failure to surface"

      it "shared environment: every branch reads from the same record" do
        -- Docstring promise: "Shared environment. Each branch sees
        -- the same environment record. There is no per-branch
        -- override." Every other `Par.ado` test in this file runs
        -- against the empty `()` row, so none of them observes
        -- whether the same env record is threaded into every
        -- branch. A regression that mutated the env between branch
        -- invocations (e.g. `unRIO rf r *> unRIO ra (someEdit r)`)
        -- would still pass them all because no branch reaches into
        -- `r`. Pin the contract by reading the same service from
        -- two parallel branches and asserting they observe the
        -- identical value.
        let
          program :: RIO (config :: { port :: Int }) () { a :: Int, b :: Int }
          program = Par.ado
            a <- asks (Proxy :: Proxy "config") _.port
            b <- asks (Proxy :: Proxy "config") _.port
            in { a, b }
        result <- runRIO
          (provideAll { config: { port: 8080 } } program)
        result `shouldEqual` Right { a: 8080, b: 8080 }

      it "a defect in any branch propagates and interrupts the siblings" do
        -- Docstring promise: "A defect (Aff exception) in any
        -- branch propagates; the other branches are interrupted
        -- by the underlying ParAff runtime." Pin both halves:
        -- the defect raised by one branch surfaces as an Aff
        -- exception (visible through `attempt`), and the
        -- sibling's post-delay side effect never runs because
        -- ParAff cancels it.
        siblingFinished <- liftEffect (Ref.new false)
        let
          program :: RIO () () Int
          program = Par.ado
            a <-
              liftAff (delay (Milliseconds 50.0))
                *> liftEffect (Ref.write true siblingFinished)
                *> pure 1
            b <-
              liftAff (delay (Milliseconds 5.0))
                *> die (error "kaboom")
            in a + b

        outcome <- attempt (runRIO program)
        case outcome of
          Left _ -> pure unit
          Right _ -> Spec.fail "expected defect to propagate as an Aff exception"
        -- Wait past the sibling's original delay; if it had not
        -- been interrupted, the post-delay write would land in
        -- this window.
        delay (Milliseconds 100.0)
        finished <- liftEffect (Ref.read siblingFinished)
        finished `shouldEqual` false
