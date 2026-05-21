module Test.RIO.Aff.CauseSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.String (contains, split, stripPrefix)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message) as Exception
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Cause
  ( Cause(..)
  , acquireReleaseCause
  , attemptCause
  , bothPar
  , concatParallel
  , concatSequential
  , firstDefect
  , firstFailure
  , flatten
  , fromOutcome
  , mapFailures
  , parSequenceCause
  , parTraverseCause
  , prettyCause
  , prettyCauseWithStack
  , raceCause
  , squash
  , stripDefects
  , stripFailures
  )
import RIO.Aff.Core (RIO, die, fail, runRIO, runRIO')

type Errs = (boom :: String, oops :: Int)

renderErrs :: Variant Errs -> String
renderErrs = Variant.match
  { boom: \s -> "boom: " <> s
  , oops: \n -> "oops: " <> show n
  }

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Cause" do

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

    describe "prettyCauseWithStack" do
      it "behaves like prettyCause on a Fail leaf" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "x" :: Variant Errs
          out = prettyCauseWithStack renderErrs (Fail v)
        out `shouldEqual` "fail: boom: x"

      it "always includes the defect's message" do
        let
          out = prettyCauseWithStack renderErrs
            (Die (Exception.error "kaboom"))
        (contains (Pattern "defect: kaboom") out) `shouldEqual` true

      it "indents stack lines under the defect header when present" do
        let
          out = prettyCauseWithStack renderErrs
            (Die (Exception.error "kaboom"))
        case stripStackHead out of
          Just rest ->
            -- Every stack line that appears must be indented under
            -- the defect header. We accept either zero lines (no
            -- stack from this engine) or one or more correctly
            -- indented lines.
            (allLinesIndented "  " rest) `shouldEqual` true
          Nothing -> shouldEqual "" "expected 'defect: kaboom' prefix"

      it "preserves Parallel structure around defects" do
        let
          tree = Parallel
            (Die (Exception.error "left-defect"))
            (Die (Exception.error "right-defect"))
          out = prettyCauseWithStack renderErrs tree
        (contains (Pattern "parallel failures:") out) `shouldEqual` true
        (contains (Pattern "  defect: left-defect") out) `shouldEqual`
          true
        (contains (Pattern "  defect: right-defect") out) `shouldEqual`
          true

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

      it "preserves input order even when branches finish out of order" do
        -- Docstring promise: "If every branch succeeds, returns
        -- `Right` of the result array in the original input
        -- order." The existing all-success test uses
        -- `\n -> pure (n * 2)` so every branch completes in the
        -- same tick, making the ordering contract trivially
        -- satisfied by the input traversal. Pin the contract by
        -- giving each branch a wall-clock delay inversely
        -- proportional to its position: branch 4 finishes well
        -- before branch 1, yet the output array must still be
        -- in input order.
        let
          step n =
            liftAff (delay (Milliseconds (toNumber (50 - 10 * n))))
              *> pure (n * 10)

          program :: RIO () Errs (Either (Cause Errs) (Array Int))
          program = parTraverseCause step [ 1, 2, 3, 4 ]
        r <- runRIO program
        case r of
          Right (Right xs) -> xs `shouldEqual` [ 10, 20, 30, 40 ]
          _ -> shouldEqual ""
            "expected Right (Right [10, 20, 30, 40]) in input order"

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

      it "a fast failure does not beat a slow success" do
        -- Docstring promise: "`raceCause` waits for at least one
        -- success before giving up on the other side, so a fast
        -- failure does not beat a slow success." The existing
        -- "both succeed" and "both fail" pins do not exercise
        -- the asymmetric case where one side fails fast and the
        -- other succeeds slowly. A regression that surfaced the
        -- first completion (success or failure), like
        -- `RIO.Aff.Concurrency.race` does, would still pass the
        -- "both succeed" pin (left finishes first) and the
        -- "both fail" pin (both are failures), but it would
        -- return the fast failure here instead of waiting for
        -- the slow success. Pin the slow-success-wins promise
        -- by giving the left side an immediate typed failure
        -- and the right side a delayed `pure 7`.
        let
          left :: RIO () Errs Int
          left = fail (Proxy :: Proxy "boom") "fast-fail"

          right :: RIO () Errs Int
          right = do
            liftAff (delay (Milliseconds 30.0))
            pure 7

          program :: RIO () Errs (Either (Cause Errs) Int)
          program = raceCause left right
        r <- runRIO program
        case r of
          Right (Right 7) -> pure unit
          _ -> shouldEqual ""
            "expected Right (Right 7) (slow success beats fast failure)"

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

      it "skips the release when acquire fails with a defect" do
        -- Docstring promise: "Acquire failures short-circuit
        -- before any release runs, just like the existing
        -- primitive: a failure during acquire becomes a single
        -- `Fail` / `Die` cause and the use / release phases are
        -- skipped entirely." The pinned "skips the release when
        -- acquire itself fails" test only covers the typed
        -- failure half (`Fail`). The defect half — where
        -- acquire raises via `die` and the implementation hits
        -- the `Left err -> pure (Right (Left (Die err)))` branch
        -- — has no test. A regression that called the release
        -- when acquire threw a defect would still pass every
        -- existing test, since the typed-failure path is
        -- handled by a separate case.
        countRef <- liftEffect (Ref.new 0)
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program = acquireReleaseCause
            ( die (Exception.error "acq-defect")
                :: RIO () Errs Int
            )
            (\_ -> liftEffect (Ref.modify_ (_ + 1) countRef))
            (\a -> pure a)
        r <- runRIO program
        releases <- liftEffect (Ref.read countRef)
        releases `shouldEqual` 0
        case r of
          Right (Left (Die _)) -> pure unit
          _ -> shouldEqual "" "expected Right (Left (Die _))"

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

      it "release runs even when the body is killed mid-flight" do
        -- Docstring promise: "The release is run through
        -- `Aff.bracket`'s uninterruptible release phase, so a
        -- kill landing during the body is queued until the
        -- release completes." None of the success / typed-fail /
        -- defect / acquire-fail pins above exercise the
        -- kill-mid-body path: every other test runs to a
        -- well-defined termination of the body before the
        -- release should fire. A regression that replaced
        -- `bracket` with a plain `try` / `finally` (which is
        -- interruptible under some Aff versions) would still
        -- pass every test above while skipping the release on
        -- kill. Pin the contract by forking the program, killing
        -- the fiber while the body is sleeping, then observing
        -- the release counter from outside the fiber.
        countRef <- liftEffect (Ref.new 0)
        let
          program :: RIO () () (Either (Cause Errs) Int)
          program = acquireReleaseCause
            (pure 1 :: RIO () Errs Int)
            (\_ -> liftEffect (Ref.modify_ (_ + 1) countRef))
            ( \_ -> do
                liftAff (delay (Milliseconds 200.0))
                pure 0
            )
        f <- forkAff (runRIO' program)
        delay (Milliseconds 10.0)
        killFiber (error "test-cancel") f
        -- Give the uninterruptible release phase room to land.
        delay (Milliseconds 50.0)
        releases <- liftEffect (Ref.read countRef)
        releases `shouldEqual` 1

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

    describe "firstFailure" do
      it "returns the leftmost typed failure in a parallel tree" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "oops") 5 :: Variant Errs
          tree = Parallel (Fail v1) (Fail v2)
        case firstFailure tree of
          Just _ -> pure unit
          Nothing -> shouldEqual "" "expected Just"

      it "skips defects and finds the failure on the right" do
        let
          v = Variant.inj (Proxy :: Proxy "oops") 5 :: Variant Errs
          tree = Sequential (Die (Exception.error "x")) (Fail v)
        case firstFailure tree of
          Just _ -> pure unit
          Nothing -> shouldEqual "" "expected Just"

      it "returns Nothing for a defect-only tree" do
        let
          tree :: Cause Errs
          tree = Parallel
            (Die (Exception.error "a"))
            (Die (Exception.error "b"))
        firstFailure tree `shouldEqual` Nothing

    describe "firstDefect" do
      it "returns the leftmost defect in a sequential tree" do
        let
          tree :: Cause Errs
          tree = Sequential (Die (Exception.error "first")) (Die (Exception.error "second"))
        case firstDefect tree of
          Just err -> (Exception.message err) `shouldEqual` "first"
          Nothing -> shouldEqual "" "expected Just"

      it "skips typed failures and finds the defect on the right" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "x" :: Variant Errs
          tree :: Cause Errs
          tree = Parallel (Fail v) (Die (Exception.error "boom!"))
        case firstDefect tree of
          Just err -> (Exception.message err) `shouldEqual` "boom!"
          Nothing -> shouldEqual "" "expected Just"

      it "returns Nothing for a failure-only tree" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "oops") 1 :: Variant Errs
          tree = Sequential (Fail v1) (Fail v2)
        case firstDefect tree of
          Nothing -> pure unit
          Just _ -> shouldEqual "" "expected Nothing"

    describe "mapFailures" do
      it "transforms typed failures while preserving structure" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "boom") "b" :: Variant Errs
          tree = Parallel (Fail v1) (Sequential (Fail v2) (Die (Exception.error "d")))
          mapped = mapFailures
            ( Variant.over { boom: \s -> "[mapped]" <> s } )
            tree
        case mapped of
          Parallel (Fail _) (Sequential (Fail _) (Die _)) -> pure unit
          _ -> shouldEqual "" "expected the same shape"
        (prettyCause renderErrs mapped) `shouldEqual`
          ( "parallel failures:\n"
              <> "  fail: boom: [mapped]a\n"
              <> "  sequenced failures:\n"
              <> "    fail: boom: [mapped]b\n"
              <> "    defect: d"
          )

    describe "stripFailures" do
      it "drops every typed failure, keeping only defects" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "x" :: Variant Errs
          tree = Parallel (Fail v) (Die (Exception.error "d"))
        case stripFailures tree of
          Just (Die _) -> pure unit
          _ -> shouldEqual "" "expected Just (Die _)"

      it "returns Nothing when every leaf is a typed failure" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "oops") 1 :: Variant Errs
          tree = Sequential (Fail v1) (Fail v2)
        case stripFailures tree of
          Nothing -> pure unit
          Just _ -> shouldEqual "" "expected Nothing"

      it "preserves the surrounding tree shape when both branches keep defects" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "x" :: Variant Errs
          tree = Sequential
            (Parallel (Fail v) (Die (Exception.error "d1")))
            (Die (Exception.error "d2"))
        case stripFailures tree of
          Just (Sequential (Die _) (Die _)) -> pure unit
          _ -> shouldEqual "" "expected Sequential (Die _) (Die _)"

    describe "stripDefects" do
      it "drops every defect, keeping only typed failures" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "x" :: Variant Errs
          tree :: Cause Errs
          tree = Parallel (Die (Exception.error "d")) (Fail v)
        case stripDefects tree of
          Just (Fail _) -> pure unit
          _ -> shouldEqual "" "expected Just (Fail _)"

      it "returns Nothing when every leaf is a defect" do
        let
          tree :: Cause Errs
          tree = Sequential
            (Die (Exception.error "a"))
            (Die (Exception.error "b"))
        case stripDefects tree of
          Nothing -> pure unit
          Just _ -> shouldEqual "" "expected Nothing"

    describe "flatten" do
      it "collects every failure and defect, discarding the tree shape" do
        let
          v1 = Variant.inj (Proxy :: Proxy "boom") "a" :: Variant Errs
          v2 = Variant.inj (Proxy :: Proxy "oops") 7 :: Variant Errs
          tree = Sequential
            (Parallel (Fail v1) (Die (Exception.error "d1")))
            (Fail v2)
          out = flatten tree
        (Array.length out.failures) `shouldEqual` 2
        (Array.length out.defects) `shouldEqual` 1

      it "yields empty arrays for the complementary leaf kind" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "x" :: Variant Errs
          out = flatten (Fail v :: Cause Errs)
        (Array.length out.failures) `shouldEqual` 1
        (Array.length out.defects) `shouldEqual` 0

    describe "squash" do
      it "prefers the first defect over any typed failure" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "x" :: Variant Errs
          tree :: Cause Errs
          tree = Parallel (Fail v) (Die (Exception.error "the-defect"))
          err = squash (\_ -> Exception.error "RENDERED") tree
        (Exception.message err) `shouldEqual` "the-defect"

      it "falls back to rendering the first typed failure when there are no defects" do
        let
          v = Variant.inj (Proxy :: Proxy "boom") "leaf" :: Variant Errs
          err = squash (\v' -> Exception.error (renderErrs v'))
            (Fail v :: Cause Errs)
        (Exception.message err) `shouldEqual` "boom: leaf"

-- | Strip the leading `defect: kaboom` head, optionally followed
-- | by a newline, from a `prettyCauseWithStack` rendering. Returns
-- | whatever stack lines follow (possibly empty) or `Nothing` if
-- | the head isn't there at all.
stripStackHead :: String -> Maybe String
stripStackHead s = case stripPrefix (Pattern "defect: kaboom\n") s of
  Just rest -> Just rest
  Nothing ->
    if s == "defect: kaboom" then Just ""
    else Nothing

-- | Every line in `s` (split on `\n`) must start with `ind`. Empty
-- | lines are tolerated.
allLinesIndented :: String -> String -> Boolean
allLinesIndented ind s =
  Array.all
    ( \line ->
        line == "" || isJust (stripPrefix (Pattern ind) line)
    )
    (split (Pattern "\n") s)
