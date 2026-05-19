module Test.RIO.Fiber.CauseSpec (spec) where

import Prelude

import Data.Array (length)
import Data.Either (Either(..))
import Data.Variant as Variant
import Effect.Exception (error, message)
import RIO.Fiber.Cause (Cause(..))
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Core as F
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Cause" do
  describe "data type" do
    it "isEmpty distinguishes Empty from a real cause" do
      Cause.isEmpty (Cause.empty :: Cause ()) `shouldEqual` true
      Cause.isEmpty (Cause.Then Cause.empty Cause.empty :: Cause ())
        `shouldEqual` true
      Cause.isEmpty (Cause.interrupt :: Cause ()) `shouldEqual` false

    it "then_ and both collapse Empty on either side" do
      let
        a = Cause.interrupt :: Cause ()
      case Cause.then_ Cause.empty a of
        Interrupt -> pure unit
        _ -> fail "Then Empty a should collapse to a"
      case Cause.both a Cause.empty of
        Interrupt -> pure unit
        _ -> fail "Both a Empty should collapse to a"

    it "isInterrupted descends into compositions" do
      let
        c :: Cause ()
        c = Cause.both (Cause.die (error "boom")) Cause.interrupt
      Cause.isInterrupted c `shouldEqual` true
      Cause.hasDefect c `shouldEqual` true

    it "failures and defects collect leaves in order" do
      let
        c :: Cause (a :: String, b :: String)
        c = Cause.then_
          (Cause.fail (Variant.inj (Proxy :: _ "a") "first"))
          (Cause.both
              (Cause.fail (Variant.inj (Proxy :: _ "b") "second"))
              (Cause.die (error "kaboom"))
          )
      length (Cause.failures c) `shouldEqual` 2
      length (Cause.defects c) `shouldEqual` 1

  describe "causeOf" do
    it "captures Right on success" do
      let
        prog :: F.RIO () () (Either (Cause ()) Int)
        prog = F.causeOf (pure 42 :: F.RIO () () Int)
      out <- runAff prog {}
      case out of
        F.Success (Right 42) -> pure unit
        _ -> fail "expected Right 42"

    it "captures a typed failure as Cause.Fail" do
      let
        boom :: F.RIO () (oops :: String) Int
        boom = F.fail (Variant.inj (Proxy :: _ "oops") "x")

        prog :: F.RIO () () (Either (Cause (oops :: String)) Int)
        prog = F.causeOf boom
      out <- runAff prog {}
      case out of
        F.Success (Left (Fail v)) ->
          (Variant.case_ # Variant.on (Proxy :: _ "oops") identity) v
            `shouldEqual` "x"
        _ -> fail "expected Left (Cause.Fail ...)"

    it "captures a defect as Cause.Die" do
      let
        prog :: F.RIO () () (Either (Cause ()) Int)
        prog = F.causeOf (F.die (error "boom") :: F.RIO () () Int)
      out <- runAff prog {}
      case out of
        F.Success (Left (Die err)) -> message err `shouldEqual` "boom"
        _ -> fail "expected Left (Cause.Die ...)"

    it "composes action + finalizer failure as Cause.Then" do
      let
        action :: F.RIO () (oops :: String) Int
        action = F.fail (Variant.inj (Proxy :: _ "oops") "action")

        prog :: F.RIO () () (Either (Cause (oops :: String)) Int)
        prog = F.causeOf
          (F.ensuring (F.die (error "fin")) action)
      out <- runAff prog {}
      case out of
        F.Success (Left (Then (Fail _) (Die err))) ->
          message err `shouldEqual` "fin"
        _ -> fail "expected Left (Cause.Then (Fail _) (Die _))"

    it "validatePar composes parallel failures with Cause.both" do
      let
        prog :: F.RIO () () (Either (Cause (a :: String, b :: String)) (Array Int))
        prog = F.causeOf
          ( F.validatePar identity
              [ F.fail (Variant.inj (Proxy :: _ "a") "first")
              , F.fail (Variant.inj (Proxy :: _ "b") "second")
              ] :: F.RIO () (a :: String, b :: String) (Array Int)
          )
      out <- runAff prog {}
      case out of
        F.Success (Left c) -> do
          length (Cause.failures c) `shouldEqual` 2
          length (Cause.defects c) `shouldEqual` 0
        _ -> fail "expected Left (Cause with two failures)"

    it "validatePar succeeds with all results when no branch fails" do
      let
        prog :: F.RIO () () (Either (Cause ()) (Array Int))
        prog = F.causeOf
          ( F.validatePar (\n -> pure (n + 1)) [ 1, 2, 3 ]
              :: F.RIO () () (Array Int)
          )
      out <- runAff prog {}
      case out of
        F.Success (Right xs) -> xs `shouldEqual` [ 2, 3, 4 ]
        _ -> fail "expected Right [2, 3, 4]"

    it "failCause round-trips through causeOf" do
      let
        c :: Cause (oops :: String)
        c = Cause.then_
          (Cause.fail (Variant.inj (Proxy :: _ "oops") "x"))
          Cause.interrupt

        prog :: F.RIO () () (Either (Cause (oops :: String)) Int)
        prog = F.causeOf (F.failCause c :: F.RIO () (oops :: String) Int)
      out <- runAff prog {}
      case out of
        F.Success (Left (Then (Fail _) Interrupt)) -> pure unit
        _ -> fail "expected Then (Fail _) Interrupt"
