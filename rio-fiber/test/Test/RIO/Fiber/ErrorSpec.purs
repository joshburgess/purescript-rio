module Test.RIO.Fiber.ErrorSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Class (liftEffect) as EC
import Effect.Exception (error, message)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Fiber.Aff (runAffThrow)
import RIO.Fiber.Cause (Cause(..))
import RIO.Fiber.Cause as Cause
import RIO.Fiber.FiberId as FiberId
import RIO.Fiber.Core (RIO, causeOf, die, liftEffect)
import RIO.Fiber.Core as F
import RIO.Fiber.Error
  ( absolve
  , catchSome
  , catchSomeCause
  , catchTag
  , either
  , foldRIO
  , fromEither
  , fromMaybe
  , mapBoth
  , mapError
  , matchCause
  , matchCauseRIO
  , option
  , orDie
  , orElse
  , orElseFail
  , orElseSucceed
  , refineOrDie
  , rethrow
  , tap
  , tapBoth
  , tapError
  , tapErrorCause
  , unsandbox
  )

type Errs = (notFound :: Int, parse :: String)

notFound :: Proxy "notFound"
notFound = Proxy

parseTag :: Proxy "parse"
parseTag = Proxy

renderErrs :: Variant Errs -> String
renderErrs =
  Variant.case_
    # Variant.on notFound (\n -> "notFound:" <> show n)
    # Variant.on parseTag (\s -> "parse:" <> s)

-- | Run a program whose typed-error row is `e` and turn the failure
-- | (if any) into the success channel for assertion.
runE
  :: forall e a
   . RIO () e a
  -> Aff (Either (Cause e) a)
runE = runAffThrow <<< causeOf

spec :: Spec Unit
spec = describe "RIO.Fiber.Error" do
  describe "catchTag" do
    it "handles the named tag and shrinks the row" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 7)
        catchTag notFound (\n -> pure (100 + n)) program
      case result of
        Right n -> n `shouldEqual` 107
        Left _ -> fail "expected success 107"

    it "lets a different tag pass through on the shrunk row" do
      result :: Either (Cause (parse :: String)) Int <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj parseTag "bad")
        catchTag notFound (\_ -> pure 0) program
      case result of
        Left (Fail v) ->
          (Variant.case_ # Variant.on parseTag identity $ v)
            `shouldEqual` "bad"
        _ -> fail "expected parse:bad to propagate"

    it "passes a success through untouched" do
      result <- runE do
        let program = pure 9 :: RIO () Errs Int
        catchTag notFound (\_ -> pure 0) program
      case result of
        Right n -> n `shouldEqual` 9
        Left _ -> fail "expected success 9"

  describe "catchSome" do
    it "handles a matching failure" do
      result <- runE do
        let
          handle :: Variant Errs -> Maybe (RIO () Errs Int)
          handle = Variant.default Nothing
            # Variant.on notFound (\_ -> Just (pure 0))
        catchSome handle (F.fail (Variant.inj notFound 7) :: RIO () Errs Int)
      case result of
        Right n -> n `shouldEqual` 0
        Left _ -> fail "expected success 0"

    it "re-raises a non-matching failure on the same row" do
      result <- runE do
        let
          handle :: Variant Errs -> Maybe (RIO () Errs Int)
          handle = Variant.default Nothing
            # Variant.on notFound (\_ -> Just (pure 0))
        catchSome handle (F.fail (Variant.inj parseTag "bad") :: RIO () Errs Int)
      case result of
        Left (Fail v) -> renderErrs v `shouldEqual` "parse:bad"
        _ -> fail "expected parse:bad to propagate"

  describe "mapError / rethrow" do
    it "mapError relabels a typed failure" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 5)
        mapError
          ( Variant.case_
              # Variant.on notFound
                  (\n -> Variant.inj (Proxy :: _ "lookupFailed") n)
              # Variant.on parseTag
                  (\_ -> Variant.inj (Proxy :: _ "lookupFailed") (-1))
          )
          program
      case result of
        Left (Fail v) ->
          ( Variant.case_
              # Variant.on (Proxy :: _ "lookupFailed") (\n -> n)
              $ v
          ) `shouldEqual` 5
        _ -> fail "expected mapped lookupFailed"

    it "rethrow inside catchAll forwards on the same row" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 7)
        F.catchAll
          (\v -> rethrow v)
          program
      case result of
        Left (Fail v) -> renderErrs v `shouldEqual` "notFound:7"
        _ -> fail "expected notFound:7 to survive rethrow"

  describe "tap / tapError / tapBoth" do
    it "tap fires on success and threads the value through" do
      counter <- EC.liftEffect (Ref.new 0)
      _ <- runAffThrow do
        let program = pure 21 :: RIO () () Int
        tap (\v -> liftEffect (Ref.write v counter)) program
      seen <- EC.liftEffect (Ref.read counter)
      seen `shouldEqual` 21

    it "tapError fires on failure and re-raises unchanged" do
      counter <- EC.liftEffect (Ref.new 0)
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 3)
        tapError
          (\_ -> liftEffect (Ref.modify_ (_ + 1) counter))
          program
      seen <- EC.liftEffect (Ref.read counter)
      seen `shouldEqual` 1
      case result of
        Left (Fail _) -> pure unit
        _ -> fail "expected typed failure to survive tapError"

    it "tapBoth fires the success arm and only the success arm on success" do
      okC <- EC.liftEffect (Ref.new 0)
      errC <- EC.liftEffect (Ref.new 0)
      _ <- runAffThrow do
        let program = pure 7 :: RIO () () Int
        tapBoth
          (\_ -> liftEffect (Ref.modify_ (_ + 1) errC))
          (\_ -> liftEffect (Ref.modify_ (_ + 1) okC))
          program
      okHits <- EC.liftEffect (Ref.read okC)
      errHits <- EC.liftEffect (Ref.read errC)
      okHits `shouldEqual` 1
      errHits `shouldEqual` 0

    it "tapBoth fires only the failure arm on failure" do
      okC <- EC.liftEffect (Ref.new 0)
      errC <- EC.liftEffect (Ref.new 0)
      _ <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj parseTag "bad")
        tapBoth
          (\_ -> liftEffect (Ref.modify_ (_ + 1) errC))
          (\_ -> liftEffect (Ref.modify_ (_ + 1) okC))
          program
      okHits <- EC.liftEffect (Ref.read okC)
      errHits <- EC.liftEffect (Ref.read errC)
      okHits `shouldEqual` 0
      errHits `shouldEqual` 1

  describe "fromEither / fromMaybe / either / absolve" do
    it "fromEither lifts Right into success" do
      result <- runE (fromEither (Right 5 :: Either (Variant Errs) Int))
      case result of
        Right n -> n `shouldEqual` 5
        Left _ -> fail "expected success 5"

    it "fromEither lifts Left into a typed failure" do
      result <- runE
        ( fromEither
            (Left (Variant.inj notFound 1) :: Either (Variant Errs) Int)
        )
      case result of
        Left (Fail v) -> renderErrs v `shouldEqual` "notFound:1"
        _ -> fail "expected notFound:1"

    it "fromMaybe lifts Nothing into the supplied failure" do
      result <- runE
        ( fromMaybe
            (Variant.inj notFound 9 :: Variant Errs)
            Nothing
        )
      case result of
        Left (Fail v) -> renderErrs v `shouldEqual` "notFound:9"
        _ -> fail "expected notFound:9"

    it "either reflects a typed failure into Left" do
      out <- runAffThrow do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 8)
        either program
      case out of
        Left v -> renderErrs v `shouldEqual` "notFound:8"
        Right _ -> fail "expected Left"

    it "either reflects a success into Right" do
      out <- runAffThrow do
        let program = pure 5 :: RIO () Errs Int
        either program
      out `shouldEqual` (Right 5 :: Either (Variant Errs) Int)

    it "absolve round-trips through either" do
      result <- runE (absolve (pure (Right 11 :: Either (Variant Errs) Int)))
      case result of
        Right n -> n `shouldEqual` 11
        Left _ -> fail "expected success 11"

  describe "foldRIO / mapBoth / orElse / orElseSucceed / orElseFail / option" do
    it "foldRIO runs the error arm on failure" do
      out <- runAffThrow do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 3)
        foldRIO
          (\_ -> pure 103)
          (\a -> pure a)
          program
      out `shouldEqual` 103

    it "foldRIO runs the success arm on success" do
      out <- runAffThrow do
        let program = pure 17 :: RIO () Errs Int
        foldRIO (\_ -> pure 0) pure program
      out `shouldEqual` 17

    it "mapBoth maps both arms" do
      result :: Either (Cause (parse :: String)) Int <- runE do
        let
          program = pure 7 :: RIO () Errs Int
          relabel :: Variant Errs -> Variant (parse :: String)
          relabel =
            Variant.case_
              # Variant.on notFound (Variant.inj parseTag <<< show)
              # Variant.on parseTag (Variant.inj parseTag)
        mapBoth relabel (_ * 2) program
      case result of
        Right n -> n `shouldEqual` 14
        Left _ -> fail "expected success 14"

    it "orElse uses the fallback on failure" do
      out <- runAffThrow do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 1)
        orElse program (pure 99)
      out `shouldEqual` 99

    it "orElseSucceed replaces any failure with the supplied value" do
      out <- runAffThrow do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 0)
        orElseSucceed 42 program
      out `shouldEqual` 42

    it "orElseFail replaces any failure with the supplied Variant" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 0)
        orElseFail (Variant.inj (Proxy :: _ "mapped") unit) program
      case result of
        Left (Fail v) ->
          ( Variant.case_
              # Variant.on (Proxy :: _ "mapped") (\_ -> "ok")
              $ v
          ) `shouldEqual` "ok"
        _ -> fail "expected mapped"

    it "option produces Just on success, Nothing on failure" do
      okOut <- runAffThrow do
        let program = pure 5 :: RIO () Errs Int
        option program
      okOut `shouldEqual` (Just 5)
      noOut <- runAffThrow do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 1)
        option program
      noOut `shouldEqual` (Nothing :: Maybe Int)

  describe "orDie / refineOrDie" do
    it "orDie converts a typed failure into a defect" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 1)
        orDie (\_ -> error "translated") program
      case result of
        Left (Die err) -> message err `shouldEqual` "translated"
        _ -> fail "expected Die translated"

    it "refineOrDie keeps a refined tag" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 5)
        refineOrDie
          (Variant.default Nothing
              # Variant.on notFound (Just <<< Variant.inj notFound)
          )
          program
      case result of
        Left (Fail v) ->
          (Variant.case_ # Variant.on notFound identity) v
            `shouldEqual` 5
        _ -> fail "expected refined notFound:5"

    it "refineOrDie defects the unrefined tag" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj parseTag "bad")
        refineOrDie
          (Variant.default Nothing
              # Variant.on notFound (Just <<< Variant.inj notFound)
          )
          program
      case result of
        Left (Die err) ->
          (message err) `shouldEqual`
            "RIO.Fiber.refineOrDie: unrefined failure"
        _ -> fail "expected Die for unrefined parse"

  describe "cause-level combinators" do
    it "catchSomeCause classifies on the full Cause and recovers" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj notFound 3)
        catchSomeCause
          ( \cause -> case Cause.firstFailure cause of
              Just _ -> Just (pure 0)
              Nothing -> Nothing
          )
          program
      case result of
        Right n -> n `shouldEqual` 0
        Left _ -> fail "expected success 0"

    it "catchSomeCause re-raises a non-classified cause unchanged" do
      result <- runE do
        let
          program :: RIO () Errs Int
          program = die (error "deep")
        catchSomeCause
          ( \cause -> case Cause.firstFailure cause of
              Just _ -> Just (pure 0)
              Nothing -> Nothing
          )
          program
      case result of
        Left (Die err) -> message err `shouldEqual` "deep"
        _ -> fail "expected Die deep"

    it "matchCause runs the success handler and projects out the cause" do
      let
        prog :: RIO () Errs String
        prog = pure "ok"
      result <- runAffThrow
        ( matchCause
            ( \c -> case Cause.firstFailure c of
                Just _ -> "cause-failure"
                Nothing -> "cause-other"
            )
            (\s -> "success:" <> s)
            prog
        )
      result `shouldEqual` "success:ok"

    it "matchCause runs the cause handler on typed failure" do
      let
        prog :: RIO () Errs String
        prog = F.fail (Variant.inj notFound 99)
      result <- runAffThrow
        ( matchCause
            ( \c -> case Cause.firstFailure c of
                Just v -> "fail:" <> renderErrs v
                Nothing -> "no-fail"
            )
            (\s -> "success:" <> s)
            prog
        )
      result `shouldEqual` "fail:notFound:99"

    it "matchCause sees an interrupt cause when the body is interrupted" do
      let
        prog :: RIO () () Int
        prog = do
          fib <- F.fork (F.sleep (Milliseconds 500.0) *> pure 1)
          F.interrupt fib
          F.join fib
      result <- runAffThrow
        ( matchCause
            ( \c ->
                if Cause.isInterrupted c then "interrupted"
                else "other-cause"
            )
            (\_ -> "success")
            prog
        )
      result `shouldEqual` "interrupted"

    it "matchCauseRIO runs the effectful success handler" do
      seen <- EC.liftEffect (Ref.new "")
      let
        prog :: RIO () Errs Int
        prog = pure 42
      _ <- runAffThrow
        ( matchCauseRIO
            (\_ -> liftEffect (Ref.write "cause" seen))
            (\n -> liftEffect (Ref.write ("success:" <> show n) seen))
            prog
        )
      observed <- EC.liftEffect (Ref.read seen)
      observed `shouldEqual` "success:42"

    it "matchCauseRIO routes a defect through the cause handler" do
      let
        prog :: RIO () Errs Int
        prog = die (error "boom")
      result <- runAffThrow
        ( matchCauseRIO
            ( \c -> pure
                ( if Cause.hasDefect c then "defect"
                  else "other"
                )
            )
            (\_ -> pure "success")
            prog
        )
      result `shouldEqual` "defect"

    it "tapErrorCause sees the cause and re-raises it" do
      seen <- EC.liftEffect (Ref.new 0)
      result <- runE do
        let
          program :: RIO () Errs Int
          program = F.fail (Variant.inj parseTag "x")
        tapErrorCause
          ( \c -> liftEffect (Ref.write (Array.length (Cause.failures c)) seen)
          )
          program
      count <- EC.liftEffect (Ref.read seen)
      count `shouldEqual` 1
      case result of
        Left (Fail _) -> pure unit
        _ -> fail "expected typed failure"

  describe "linearize" do
    it "flattens Then/Both into a single left-to-right leaf list" do
      let
        c :: Cause (a :: String, b :: String)
        c = Cause.then_
          (Cause.fail (Variant.inj (Proxy :: _ "a") "first"))
          (Cause.both
              (Cause.fail (Variant.inj (Proxy :: _ "b") "second"))
              (Cause.die (error "third"))
          )
      Array.length (Cause.linearize c) `shouldEqual` 3

    it "drops Empty and Interrupt markers" do
      let
        c :: Cause ()
        c = Cause.then_ Cause.empty (Cause.both (Cause.interrupt FiberId.externalFiberId) Cause.empty)
      Array.length (Cause.linearize c) `shouldEqual` 0

  describe "unsandbox" do
    it "round-trips a typed failure through causeOf" do
      let
        program :: RIO () Errs Int
        program = F.fail (Variant.inj notFound 42)

        roundTripped :: RIO () Errs Int
        roundTripped = unsandbox (causeOf program)
      result <- runE roundTripped
      case result of
        Left (Fail v) -> renderErrs v `shouldEqual` "notFound:42"
        _ -> fail "expected a typed failure to round-trip"

    it "passes a success straight through" do
      let
        roundTripped :: RIO () Errs Int
        roundTripped = unsandbox (causeOf (pure 7 :: RIO () Errs Int))
      result <- runE roundTripped
      case result of
        Right n -> n `shouldEqual` 7
        Left _ -> fail "expected Right"

    it "replays a defect captured by causeOf" do
      let
        program :: RIO () Errs Int
        program = die (error "boom")

        roundTripped :: RIO () Errs Int
        roundTripped = unsandbox (causeOf program)
      result <- runE roundTripped
      case result of
        Left (Die e) -> message e `shouldEqual` "boom"
        _ -> fail "expected a defect to round-trip"
