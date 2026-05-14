module Test.RIO.CauseSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.String (contains)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Exception (error) as Exception
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Cause
  ( Cause(..)
  , acquireReleaseCause
  , attemptCause
  , bothPar
  , concatParallel
  , concatSequential
  , fromOutcome
  , parSequenceCause
  , parTraverseCause
  , prettyCause
  , raceCause
  )
import RIO.Core (RIO, die, fail, runRIO)

type Errs = (boom :: String, oops :: Int)

renderErrs :: Variant Errs -> String
renderErrs = Variant.match
  { boom: \s -> "boom: " <> s
  , oops: \n -> "oops: " <> show n
  }

spec :: Spec Unit
spec = do
  describe "RIO.Cause" do

    describe "fromOutcome" do
      it "passes a successful outcome through" do
        let
          outcome :: Either _ (Either (Variant Errs) Int)
          outcome = Right (Right 7)
        case fromOutcome outcome of
          Right 7 -> pure unit
          _ -> shouldEqual "" "expected Right 7"

      it "wraps a typed failure as Fail" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "nope" :: Variant Errs

          outcome :: Either _ (Either (Variant Errs) Int)
          outcome = Right (Left v)
        case fromOutcome outcome of
          Left (Fail _) -> pure unit
          _ -> shouldEqual "" "expected Left (Fail _)"

      it "wraps a defect as Die" do
        let
          outcome :: Either _ (Either (Variant Errs) Int)
          outcome = Left (Exception.error "kaboom")
        case fromOutcome outcome of
          Left (Die err) ->
            (contains (Pattern "kaboom") (show err)) `shouldEqual` true
          _ -> shouldEqual "" "expected Left (Die _)"

    describe "bothPar" do
      it "returns Right (Tuple a b) when both sides succeed" do
        let
          program :: RIO () Errs (Either (Cause Errs) (Tuple Int String))
          program = bothPar (pure 1) (pure "ok")
        r <- runRIO program
        case r of
          Right (Right (Tuple 1 "ok")) -> pure unit
          _ -> shouldEqual "" "expected Right (Right (Tuple 1 \"ok\"))"

      it "returns Left (Fail v) when only the left side fails typed" do
        let
          left :: RIO () Errs Int
          left = fail (Proxy :: Proxy "boom") "left bad"

          program :: RIO () Errs (Either (Cause Errs) (Tuple Int String))
          program = bothPar left (pure "ok")
        r <- runRIO program
        case r of
          Right (Left (Fail _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Fail _))"

      it "returns Left (Fail v) when only the right side fails typed" do
        let
          right :: RIO () Errs String
          right = fail (Proxy :: Proxy "boom") "right bad"

          program :: RIO () Errs (Either (Cause Errs) (Tuple Int String))
          program = bothPar (pure 1) right
        r <- runRIO program
        case r of
          Right (Left (Fail _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Fail _))"

      it "returns Left (Die _) when only one side dies" do
        let
          left :: RIO () Errs Int
          left = die (Exception.error "boom!")

          program :: RIO () Errs (Either (Cause Errs) (Tuple Int String))
          program = bothPar left (pure "ok")
        r <- runRIO program
        case r of
          Right (Left (Die _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Die _))"

      it "returns Parallel cause when both sides fail" do
        let
          left :: RIO () Errs Int
          left = fail (Proxy :: Proxy "boom") "left"

          right :: RIO () Errs String
          right = fail (Proxy :: Proxy "oops") 99

          program :: RIO () Errs (Either (Cause Errs) (Tuple Int String))
          program = bothPar left right
        r <- runRIO program
        case r of
          Right (Left (Parallel (Fail _) (Fail _))) -> pure unit
          _ -> shouldEqual "" "expected Parallel of two Fails"

      it "returns Parallel when one fails typed and the other dies" do
        let
          left :: RIO () Errs Int
          left = fail (Proxy :: Proxy "boom") "left"

          right :: RIO () Errs String
          right = die (Exception.error "right defect")

          program :: RIO () Errs (Either (Cause Errs) (Tuple Int String))
          program = bothPar left right
        r <- runRIO program
        case r of
          Right (Left (Parallel (Fail _) (Die _))) -> pure unit
          _ -> shouldEqual "" "expected Parallel (Fail _) (Die _)"

    describe "concatParallel / concatSequential" do
      it "build the corresponding cause constructors" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "oops") 5 :: Variant Errs
          p = concatParallel (Fail v1) (Fail v2)
          s = concatSequential (Fail v1) (Fail v2)
        case p of
          Parallel (Fail _) (Fail _) -> pure unit
          _ -> shouldEqual "" "expected Parallel"
        case s of
          Sequential (Fail _) (Fail _) -> pure unit
          _ -> shouldEqual "" "expected Sequential"

    describe "prettyCause" do
      it "renders a typed Fail on a single line" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "x" :: Variant Errs
          out = prettyCause renderErrs (Fail v)
        out `shouldEqual` "fail: boom: x"

      it "renders a Die using the exception's message" do
        let out = prettyCause renderErrs (Die (Exception.error "kaboom"))
        out `shouldEqual` "defect: kaboom"

      it "renders Parallel with a header and indented branches" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "oops") 5 :: Variant Errs
          out = prettyCause renderErrs (Parallel (Fail v1) (Fail v2))
        out `shouldEqual`
          "parallel failures:\n  fail: boom: a\n  fail: oops: 5"

      it "renders Sequential with a different header" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "oops") 5 :: Variant Errs
          out = prettyCause renderErrs (Sequential (Fail v1) (Fail v2))
        out `shouldEqual`
          "sequenced failures:\n  fail: boom: a\n  fail: oops: 5"

      it "indents nested combinators progressively" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "oops") 5 :: Variant Errs
          v3 = Variant.inj (Proxy :: Proxy "boom") "c" :: Variant Errs
          tree = Parallel (Fail v1) (Sequential (Fail v2) (Fail v3))
          out = prettyCause renderErrs tree
        out `shouldEqual`
          ( "parallel failures:\n"
              <> "  fail: boom: a\n"
              <> "  sequenced failures:\n"
              <> "    fail: oops: 5\n"
              <> "    fail: boom: c"
          )

      it "works end-to-end with bothPar output" do
        let
          left :: RIO () Errs Int
          left = fail (Proxy :: Proxy "boom") "left"

          right :: RIO () Errs String
          right = fail (Proxy :: Proxy "oops") 7

          program :: RIO () Errs String
          program = do
            r <- bothPar left right
            pure case r of
              Right _ -> "ok"
              Left c -> prettyCause renderErrs c
        res <- runRIO program
        case res of
          Right out ->
            out `shouldEqual`
              "parallel failures:\n  fail: boom: left\n  fail: oops: 7"
          Left _ -> shouldEqual "" "expected Right"

    describe "attemptCause" do
      it "wraps a success as Right" do
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program = attemptCause (pure 42 :: RIO () Errs Int)
        r <- runRIO program
        case r of
          Right (Right 42) -> pure unit
          _ -> shouldEqual "" "expected Right (Right 42)"

      it "wraps a typed failure as Left (Fail _)" do
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program =
            attemptCause
              ( fail (Proxy :: Proxy "boom") "nope" :: RIO () Errs Int
              )
        r <- runRIO program
        case r of
          Right (Left (Fail _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Fail _))"

      it "wraps a defect as Left (Die _)" do
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program =
            attemptCause
              ( die (Exception.error "kaboom") :: RIO () Errs Int
              )
        r <- runRIO program
        case r of
          Right (Left (Die _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Die _))"

    describe "parTraverseCause" do
      it "returns Right (Array b) when every branch succeeds" do
        let
          program :: RIO () Errs (Either (Cause Errs) (Array Int))
          program = parTraverseCause (\n -> pure (n * 2)) [ 1, 2, 3 ]
        r <- runRIO program
        case r of
          Right (Right xs) -> xs `shouldEqual` [ 2, 4, 6 ]
          _ -> shouldEqual "" "expected Right (Right [2,4,6])"

      it "captures every typed failure into a Parallel tree" do
        let
          step n =
            if n `mod` 2 == 0 then pure n
            else fail (Proxy :: Proxy "oops") n

          program :: RIO () Errs (Either (Cause Errs) (Array Int))
          program = parTraverseCause step [ 1, 2, 3 ]
        r <- runRIO program
        case r of
          Right (Left (Parallel (Fail _) (Fail _))) -> pure unit
          _ ->
            shouldEqual "" "expected Parallel of two Fails"

      it "captures a defect alongside a typed failure" do
        let
          step n
            | n == 1 = fail (Proxy :: Proxy "boom") "bad-1"
            | n == 2 = die (Exception.error "bad-2")
            | otherwise = pure n

          program :: RIO () Errs (Either (Cause Errs) (Array Int))
          program = parTraverseCause step [ 1, 2, 3 ]
        r <- runRIO program
        case r of
          Right (Left (Parallel (Fail _) (Die _))) -> pure unit
          _ ->
            shouldEqual "" "expected Parallel (Fail _) (Die _)"

      it "returns Right [] for an empty input" do
        let
          program :: RIO () Errs (Either (Cause Errs) (Array Int))
          program = parTraverseCause pure []
        r <- runRIO program
        case r of
          Right (Right xs) -> xs `shouldEqual` []
          _ -> shouldEqual "" "expected Right (Right [])"

    describe "parSequenceCause" do
      it "runs every action and collects failures" do
        let
          actions :: Array (RIO () Errs Int)
          actions =
            [ pure 1
            , fail (Proxy :: Proxy "boom") "two"
            , fail (Proxy :: Proxy "oops") 3
            ]

          program :: RIO () Errs (Either (Cause Errs) (Array Int))
          program = parSequenceCause actions
        r <- runRIO program
        case r of
          Right (Left (Parallel (Fail _) (Fail _))) -> pure unit
          _ -> shouldEqual "" "expected Parallel of two Fails"

    describe "raceCause" do
      it "returns the success when one side succeeds" do
        let
          program :: RIO () Errs (Either (Cause Errs) Int)
          program = raceCause
            (fail (Proxy :: Proxy "boom") "slow-fail")
            (pure 7)
        r <- runRIO program
        case r of
          Right (Right 7) -> pure unit
          _ -> shouldEqual "" "expected Right (Right 7)"

      it "prefers the left side when both succeed" do
        let
          program :: RIO () Errs (Either (Cause Errs) Int)
          program = raceCause (pure 1) (pure 2)
        r <- runRIO program
        case r of
          Right (Right 1) -> pure unit
          _ -> shouldEqual "" "expected Right (Right 1)"

      it "returns Parallel when both sides fail" do
        let
          program :: RIO () Errs (Either (Cause Errs) Int)
          program = raceCause
            (fail (Proxy :: Proxy "boom") "left")
            (fail (Proxy :: Proxy "oops") 9)
        r <- runRIO program
        case r of
          Right (Left (Parallel (Fail _) (Fail _))) -> pure unit
          _ -> shouldEqual "" "expected Parallel of two Fails"

      it "renders both failures via prettyCause" do
        let
          program :: RIO () Errs String
          program = do
            r <- raceCause
              (fail (Proxy :: Proxy "boom") "left")
              (fail (Proxy :: Proxy "oops") 9)
            pure case r of
              Right _ -> "ok"
              Left c -> prettyCause renderErrs c
        res <- runRIO program
        case res of
          Right out ->
            out `shouldEqual`
              "parallel failures:\n  fail: boom: left\n  fail: oops: 9"
          Left _ -> shouldEqual "" "expected Right"

    describe "acquireReleaseCause" do
      it "returns the body's value when both phases succeed" do
        countRef <- liftEffect (Ref.new 0)
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program = acquireReleaseCause
            (pure 1 :: RIO () Errs Int)
            (\_ -> liftEffect (Ref.modify_ (_ + 1) countRef))
            (\a -> pure (a + 41))
        r <- runRIO program
        releases <- liftEffect (Ref.read countRef)
        releases `shouldEqual` 1
        case r of
          Right (Right 42) -> pure unit
          _ -> shouldEqual "" "expected Right (Right 42)"

      it "runs the release even when the body fails typed" do
        countRef <- liftEffect (Ref.new 0)
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program = acquireReleaseCause
            (pure 1 :: RIO () Errs Int)
            (\_ -> liftEffect (Ref.modify_ (_ + 1) countRef))
            ( \_ ->
                fail (Proxy :: Proxy "boom") "body-bad"
                  :: RIO () Errs Int
            )
        r <- runRIO program
        releases <- liftEffect (Ref.read countRef)
        releases `shouldEqual` 1
        case r of
          Right (Left (Fail _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Fail _))"

      it "skips the release when acquire itself fails" do
        countRef <- liftEffect (Ref.new 0)
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program = acquireReleaseCause
            ( fail (Proxy :: Proxy "boom") "acq-bad"
                :: RIO () Errs Int
            )
            (\_ -> liftEffect (Ref.modify_ (_ + 1) countRef))
            (\a -> pure a)
        r <- runRIO program
        releases <- liftEffect (Ref.read countRef)
        releases `shouldEqual` 0
        case r of
          Right (Left (Fail _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Fail _))"

      it "returns a release Die when only the release fails" do
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program = acquireReleaseCause
            (pure 1 :: RIO () Errs Int)
            (\_ -> die (Exception.error "release-bad"))
            (\a -> pure (a + 41))
        r <- runRIO program
        case r of
          Right (Left (Die _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Die _))"

      it "returns Sequential (body, release) when both fail" do
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program = acquireReleaseCause
            (pure 1 :: RIO () Errs Int)
            (\_ -> die (Exception.error "release-bad"))
            (\_ -> fail (Proxy :: Proxy "boom") "body-bad")
        r <- runRIO program
        case r of
          Right (Left (Sequential (Fail _) (Die _))) -> pure unit
          _ ->
            shouldEqual ""
              "expected Sequential (Fail _) (Die _)"

      it "renders the Sequential cause via prettyCause" do
        let
          program :: RIO () () String
          program = do
            r <- acquireReleaseCause
              (pure 1 :: RIO () Errs Int)
              (\_ -> die (Exception.error "close failed"))
              (\_ -> fail (Proxy :: Proxy "boom") "write failed")
            pure case r of
              Right _ -> "ok"
              Left c -> prettyCause renderErrs c
        res <- runRIO program
        case res of
          Right out ->
            out `shouldEqual`
              ( "sequenced failures:\n"
                  <> "  fail: boom: write failed\n"
                  <> "  defect: close failed"
              )
          Left _ -> shouldEqual "" "expected Right"
