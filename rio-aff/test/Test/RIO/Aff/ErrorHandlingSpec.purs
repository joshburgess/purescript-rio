module Test.RIO.Aff.ErrorHandlingSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (attempt, error, message, throwError)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions as Assertions
import Type.Proxy (Proxy(..))

import RIO.Aff.Core
  ( RIO
  , catchAll
  , catchTag
  , die
  , fail
  , mapError
  , rethrow
  , runRIO
  , sandbox
  , unsandbox
  )

-- Two distinct payload shapes used across the tests.
type NotFound = { id :: Int }
type ParseError = String

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Error (Phase 3.1)" do
    describe "catchTag" do
      it "catches the named tag and removes it from the error row" do
        -- Inner can fail with two tags; outer handles `notFound` and
        -- so reduces the row to `(parse :: ParseError)`. We close the
        -- remaining row at the call to `runRIO` by handing the type
        -- annotation an explicit single-tag row.
        let
          inner :: RIO () (notFound :: NotFound, parse :: ParseError) Int
          inner = fail (Proxy :: Proxy "notFound") { id: 13 }

          outer :: RIO () (parse :: ParseError) Int
          outer = catchTag (Proxy :: Proxy "notFound") (\p -> pure (p.id + 1)) inner
        result <- runRIO outer
        result `shouldEqual` Right 14

      it "passes through tags that don't match the handler" do
        let
          inner :: RIO () (notFound :: NotFound, parse :: ParseError) Int
          inner = fail (Proxy :: Proxy "parse") "bad json"

          outer :: RIO () (parse :: ParseError) Int
          outer = catchTag (Proxy :: Proxy "notFound") (\_ -> pure 0) inner
        result <- runRIO outer
        case result of
          Left v ->
            Variant.case_
              # Variant.on (Proxy :: Proxy "parse")
                  (\s -> s `shouldEqual` "bad json")
              $ v
          Right _ -> Assertions.fail "expected Left parse, got Right"

      it "lets the handler introduce a new tag into the error row" do
        -- Inner fails with `notFound`; the handler re-fails with `parse`.
        -- Starting row: (notFound, parse); after catch: (parse).
        let
          inner :: RIO () (notFound :: NotFound, parse :: ParseError) Int
          inner = fail (Proxy :: Proxy "notFound") { id: 1 }

          outer :: RIO () (parse :: ParseError) Int
          outer = catchTag (Proxy :: Proxy "notFound")
            (\_ -> fail (Proxy :: Proxy "parse") "from handler")
            inner
        result <- runRIO outer
        case result of
          Left v ->
            Variant.case_
              # Variant.on (Proxy :: Proxy "parse")
                  (\s -> s `shouldEqual` "from handler")
              $ v
          Right _ -> Assertions.fail "expected Left parse, got Right"

      it "is a no-op when the inner program succeeds" do
        let
          inner :: RIO () (notFound :: NotFound) Int
          inner = pure 99

          outer :: RIO () () Int
          outer = catchTag (Proxy :: Proxy "notFound") (\_ -> pure 0) inner
        result <- runRIO outer
        result `shouldEqual` Right 99

      it "leaves all subsequent statements unrun when the inner fails" do
        -- After `inner` fails, the handler's return value is what flows
        -- forward; statements that were sequenced *inside* `inner` after
        -- the failure point did not run, which we observe indirectly by
        -- the result equalling the handler's return value (not what
        -- inner's tail would have produced).
        let
          inner :: RIO () (boom :: Unit) Int
          inner = do
            _ <- fail (Proxy :: Proxy "boom") unit
            pure 100 -- unreachable

          outer :: RIO () () Int
          outer = catchTag (Proxy :: Proxy "boom") (\_ -> pure 7) inner
        result <- runRIO outer
        result `shouldEqual` Right 7

    describe "catchAll (Phase 3.2)" do
      it "replaces the error row wholesale and routes every tag through the handler" do
        let
          inner :: RIO () (notFound :: NotFound, parse :: ParseError) Int
          inner = fail (Proxy :: Proxy "parse") "x"

          outer :: RIO () () Int
          outer = catchAll (\_ -> pure 0) inner
        result <- runRIO outer
        result `shouldEqual` Right 0

      it "is identity when the handler rethrows the same variant" do
        -- The build-plan acceptance criterion calls out
        -- `catchAll rethrow ≡ identity` as a property test. Here we
        -- exercise it on a representative failure and on a success;
        -- the full property-based version lives in Phase 7.
        let
          failing :: RIO () (boom :: Unit) Int
          failing = fail (Proxy :: Proxy "boom") unit

          succeeding :: RIO () (boom :: Unit) Int
          succeeding = pure 42

        rFail <- runRIO (catchAll rethrow failing)
        rOk <- runRIO (catchAll rethrow succeeding)
        rOriginalFail <- runRIO failing
        rOriginalOk <- runRIO succeeding

        -- Successes match by Eq on the value.
        rOk `shouldEqual` rOriginalOk

        -- Failures: both must be `Left (boom :: ())`. Discharge by
        -- inspecting the tag on both sides.
        case rFail, rOriginalFail of
          Left v1, Left v2 -> do
            let
              tagOf :: Variant (boom :: Unit) -> String
              tagOf =
                Variant.case_
                  # Variant.on (Proxy :: Proxy "boom") (\_ -> "boom")
            tagOf v1 `shouldEqual` tagOf v2
          _, _ -> Assertions.fail "expected both Left, got mismatch"

      it "is a no-op on a successful program" do
        let
          inner :: RIO () (boom :: Unit) Int
          inner = pure 11

          outer :: RIO () () Int
          outer = catchAll (\_ -> pure 0) inner
        result <- runRIO outer
        result `shouldEqual` Right 11

    describe "rethrow" do
      it "wraps an already-constructed Variant back into a Left" do
        -- The `rethrow` docstring promises it is the "dual of `catchAll`"
        -- at the Variant level: it takes an existing `Variant e` (not
        -- a tag + payload) and re-raises it as `Left v` in the same
        -- row. `catchAll rethrow ≡ identity` already exercises it in
        -- composition; this pins the raw behaviour so any future
        -- internal change to `rethrow` that breaks direct callers
        -- (such as a `catchAll` handler that pattern-matches on the
        -- Variant and re-raises unmatched cases) is caught.
        let
          v :: Variant (boom :: String)
          v = Variant.inj (Proxy :: Proxy "boom") "kaboom"

          program :: RIO () (boom :: String) Int
          program = rethrow v
        result <- runRIO program
        case result of
          Left v' ->
            let
              tagOf :: Variant (boom :: String) -> String
              tagOf =
                Variant.case_
                  # Variant.on (Proxy :: Proxy "boom") identity
            in
              tagOf v' `shouldEqual` "kaboom"
          Right _ -> Assertions.fail "expected Left, got Right"

    describe "mapError (Phase 3.2)" do
      it "transforms the failure value into a different row" do
        let
          inner :: RIO () (parse :: ParseError) Int
          inner = fail (Proxy :: Proxy "parse") "oops"

          translate :: Variant (parse :: ParseError) -> Variant (parseEcho :: String)
          translate =
            Variant.case_
              # Variant.on (Proxy :: Proxy "parse")
                  (\s -> Variant.inj (Proxy :: Proxy "parseEcho") ("translated: " <> s))

          outer :: RIO () (parseEcho :: String) Int
          outer = mapError translate inner
        result <- runRIO outer
        case result of
          Left v ->
            Variant.case_
              # Variant.on (Proxy :: Proxy "parseEcho")
                  (\s -> s `shouldEqual` "translated: oops")
              $ v
          Right _ -> Assertions.fail "expected Left parseEcho, got Right"

      it "leaves successes untouched" do
        let
          inner :: RIO () (boom :: Unit) Int
          inner = pure 5

          outer :: RIO () (gone :: Unit) Int
          outer = mapError
            ( Variant.case_
                # Variant.on (Proxy :: Proxy "boom")
                    (\_ -> Variant.inj (Proxy :: Proxy "gone") unit)
            )
            inner
        result <- runRIO outer
        result `shouldEqual` Right 5

    describe "defects (Phase 3.3)" do
      describe "die" do
        it "raises a defect that bypasses the typed-error row" do
          -- `die` produces an `Aff` exception, not a `Left` in the
          -- error row. We observe it by `attempt`-ing the underlying
          -- `Aff` that `runRIO` produces.
          let
            program :: RIO () (boom :: Unit) Int
            program = die (error "kaboom")
          outcome <- attempt (runRIO program)
          case outcome of
            Left err -> message err `shouldEqual` "kaboom"
            Right _ -> Assertions.fail
              "expected an Aff defect, got a typed result"

      describe "sandbox" do
        it "reifies a defect raised inside the program into the success channel" do
          let
            program :: RIO () () (Either _ Int)
            program = sandbox (die (error "boom"))
          result <- runRIO program
          case result of
            Right (Left err) -> message err `shouldEqual` "boom"
            Right (Right _) -> Assertions.fail "expected defect, got success"
            Left _ -> Assertions.fail "expected Right, got typed Left"

        it "reifies a defect from a lifted Aff that threw" do
          let
            program :: RIO () () (Either _ Int)
            program = sandbox (liftAff (throwError (error "aff-boom")))
          result <- runRIO program
          case result of
            Right (Left err) -> message err `shouldEqual` "aff-boom"
            Right (Right _) -> Assertions.fail "expected defect, got success"
            Left _ -> Assertions.fail "expected Right, got typed Left"

        it "leaves a successful program as Right (Right a)" do
          let
            program :: RIO () () (Either _ Int)
            program = sandbox (pure 42)
          result <- runRIO program
          case result of
            Right (Right a) -> a `shouldEqual` 42
            _ -> Assertions.fail "expected Right (Right 42)"

        it "does NOT absorb typed failures; they keep propagating" do
          -- Sandbox is for defects only. A typed failure in `e` must
          -- still surface as `Left v` in the outer result.
          let
            program :: RIO () (boom :: Unit) (Either _ Int)
            program = sandbox (fail (Proxy :: Proxy "boom") unit)
          result <- runRIO program
          case result of
            Left v ->
              Variant.case_
                # Variant.on (Proxy :: Proxy "boom") (\_ -> pure unit)
                $ v
            Right _ -> Assertions.fail
              "expected typed Left to propagate through sandbox"

      describe "unsandbox" do
        it "re-raises a Left in the success channel as a defect" do
          -- Start with a sandboxed program, mutate the inner Left, run
          -- unsandbox, and observe the defect.
          let
            program :: RIO () () Int
            program = unsandbox (pure (Left (error "re-raised")))
          outcome <- attempt (runRIO program)
          case outcome of
            Left err -> message err `shouldEqual` "re-raised"
            Right _ -> Assertions.fail
              "expected an Aff defect, got a typed result"

        it "is the inverse of sandbox on successes" do
          let
            program :: RIO () () Int
            program = unsandbox (sandbox (pure 7))
          result <- runRIO program
          result `shouldEqual` Right 7

        it "is the inverse of sandbox on defects" do
          let
            program :: RIO () () Int
            program = unsandbox (sandbox (die (error "round-trip")))
          outcome <- attempt (runRIO program)
          case outcome of
            Left err -> message err `shouldEqual` "round-trip"
            Right _ -> Assertions.fail
              "expected defect to round-trip through sandbox/unsandbox"

        it "preserves typed failures unchanged (does not convert them to defects)" do
          -- Docstring promise: "Typed failures in the input are
          -- preserved unchanged." `unsandbox` only re-raises a
          -- `Left Error` in the SUCCESS channel as a defect; a
          -- typed failure flowing through must still surface as a
          -- typed Left on the same row.
          let
            inner :: RIO () (boom :: Unit) (Either _ Int)
            inner = fail (Proxy :: Proxy "boom") unit

            program :: RIO () (boom :: Unit) Int
            program = unsandbox inner
          result <- runRIO program
          case result of
            Left v ->
              Variant.case_
                # Variant.on (Proxy :: Proxy "boom") (\_ -> pure unit)
                $ v
            Right _ -> Assertions.fail
              "expected typed Left to pass through unsandbox unchanged"
