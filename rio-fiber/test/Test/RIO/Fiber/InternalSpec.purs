-- | Hot-path / fusion regression spec.
-- |
-- | The fiber runtime's `opMap` / `opApply` constructors collapse
-- | `pure`-leaf cases and propagate failures at build time; the step
-- | loop has fast paths for K_MAP / K_APPLY unwinds and for FORK / JOIN
-- | leaves inside spine walks. The semantics of those fusions are not
-- | observable through any single existing spec: a regression that
-- | turned a PURE/PURE map fusion into the wrong value would only fail
-- | here.
-- |
-- | Tests in this module pin observable outcomes (return values, side-
-- | effect order, failure propagation) for each fusion entry point, plus
-- | deep-chain stress checks so any allocator regression that re-emits
-- | the unfused tree shape shows up either as a wrong value or as a
-- | stack overflow rather than as a silent perf cliff.
module Test.RIO.Fiber.InternalSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Cause as Cause
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Internal" do

  describe "opMap PURE fusion" do
    it "map f (pure x) collapses to pure (f x)" do
      out <- runAff (map (_ + 1) (pure 41) :: F.RIO () () Int) {}
      assertSuccess out 42

    it "two stacked maps over pure compose" do
      out <- runAff (map (_ * 3) (map (_ + 1) (pure 4)) :: F.RIO () () Int) {}
      assertSuccess out 15

    it "left-to-right map composition equals right-to-left" do
      let
        f = (_ + 1)
        g = (_ * 5)
        lhs = map (f <<< g) (pure 6 :: F.RIO () () Int)
        rhs = map f (map g (pure 6 :: F.RIO () () Int))
      lOut <- runAff lhs {}
      rOut <- runAff rhs {}
      assertSuccess lOut 31
      assertSuccess rOut 31

    it "1000-deep map chain over pure produces correct value" do
      let
        go :: Int -> F.RIO () () Int -> F.RIO () () Int
        go 0 m = m
        go k m = go (k - 1) (map (_ + 1) m)
      out <- runAff (go 1000 (pure 0)) {}
      assertSuccess out 1000

    it "1000-deep map chain of identity does not stack overflow" do
      let
        go :: Int -> F.RIO () () Int -> F.RIO () () Int
        go 0 m = m
        go k m = go (k - 1) (map identity m)
      out <- runAff (go 1000 (pure 7)) {}
      assertSuccess out 7

  describe "opMap FAIL short-circuit" do
    it "map f over a typed failure does not run f and surfaces the failure" do
      let
        prog :: F.RIO () (boom :: String) Int
        prog = map (_ + 1) (F.fail (Variant.inj (Proxy :: _ "boom") "x"))
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "x"
        other -> fail ("expected Fail, got " <> describeOutcome other)

    it "map over failCause surfaces the cause unchanged" do
      let
        c :: Cause.Cause (oops :: String)
        c = Cause.then_
          (Cause.fail (Variant.inj (Proxy :: _ "oops") "first"))
          Cause.interrupt

        prog :: F.RIO () () (Either (Cause.Cause (oops :: String)) Int)
        prog = F.causeOf
          (map (_ + 1) (F.failCause c) :: F.RIO () (oops :: String) Int)
      out <- runAff prog {}
      case out of
        Success (Left (Cause.Then (Cause.Fail _) Cause.Interrupt)) -> pure unit
        other -> fail
          ("expected Then (Fail _) Interrupt, got " <> describeOutcome other)

  describe "opMap SYNC fusion" do
    it "map f over liftEffect runs the effect and applies f to its result" do
      ref <- liftEffect (Ref.new 9)
      out <- runAff
        (map (_ + 1) (F.liftEffect (Ref.read ref)) :: F.RIO () () Int)
        {}
      assertSuccess out 10

    it "stacked maps over liftEffect produce composed result" do
      ref <- liftEffect (Ref.new 3)
      out <- runAff
        ( map (_ * 2)
            (map (_ + 1) (F.liftEffect (Ref.read ref))) :: F.RIO () () Int
        )
        {}
      assertSuccess out 8

  describe "opApply PURE/PURE fusion" do
    it "pure f <*> pure x reduces to pure (f x)" do
      out <- runAff (pure (_ + 1) <*> pure 41 :: F.RIO () () Int) {}
      assertSuccess out 42

    it "1000-deep apply chain over pure produces correct value" do
      let
        go :: Int -> F.RIO () () Int -> F.RIO () () Int
        go 0 m = m
        go k m = go (k - 1) (pure (_ + 1) <*> m)
      out <- runAff (go 1000 (pure 0)) {}
      assertSuccess out 1000

  describe "opApply degenerate-to-map" do
    it "pure f <*> sync a equals map f over the sync action" do
      ref <- liftEffect (Ref.new 5)
      out <- runAff
        ( (pure (_ + 1) <*> F.liftEffect (Ref.read ref)) :: F.RIO () () Int
        )
        {}
      assertSuccess out 6

    it "sync f <*> pure a applies the read function to a" do
      ref <- liftEffect (Ref.new (\(n :: Int) -> n * 10))
      out <- runAff
        ( (F.liftEffect (Ref.read ref) <*> pure 7) :: F.RIO () () Int
        )
        {}
      assertSuccess out 70

  describe "opApply FAIL short-circuit" do
    it "fail <*> ma surfaces the failure and does not run ma" do
      ranRef <- liftEffect (Ref.new false)
      let
        ma :: F.RIO () (boom :: String) Int
        ma = do
          F.liftEffect (Ref.write true ranRef)
          pure 0

        prog :: F.RIO () (boom :: String) Int
        prog = F.fail (Variant.inj (Proxy :: _ "boom") "left") <*> ma
      out <- runAff prog {}
      case out of
        Fail v ->
          (Variant.case_ # Variant.on (Proxy :: _ "boom") identity) v
            `shouldEqual` "left"
        other -> fail ("expected Fail, got " <> describeOutcome other)
      ran <- liftEffect (Ref.read ranRef)
      ran `shouldEqual` false

  describe "opBind PURE fast path" do
    it "bind (pure x) k equals k x" do
      let
        prog :: F.RIO () () Int
        prog = pure 41 >>= \x -> pure (x + 1)
      out <- runAff prog {}
      assertSuccess out 42

    it "left-identity holds for a non-trivial k" do
      let
        k n = pure (n * 2 + 1) :: F.RIO () () Int
        lhs = pure 7 >>= k
        rhs = k 7
      lOut <- runAff lhs {}
      rOut <- runAff rhs {}
      assertSuccess lOut 15
      assertSuccess rOut 15

    it "1000-deep bind chain over pure produces correct value" do
      let
        go :: Int -> Int -> F.RIO () () Int
        go acc 0 = pure acc
        go acc k = pure (acc + 1) >>= \a -> go a (k - 1)
      out <- runAff (go 0 1000) {}
      assertSuccess out 1000

  describe "opBind FAIL short-circuit" do
    it "fail >>= k never runs k" do
      ranRef <- liftEffect (Ref.new false)
      let
        k :: Int -> F.RIO () (boom :: String) Int
        k _ = do
          F.liftEffect (Ref.write true ranRef)
          pure 1

        prog :: F.RIO () (boom :: String) Int
        prog = F.fail (Variant.inj (Proxy :: _ "boom") "x") >>= k
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)
      ran <- liftEffect (Ref.read ranRef)
      ran `shouldEqual` false

    it "failCause >>= k never runs k" do
      ranRef <- liftEffect (Ref.new false)
      let
        c :: Cause.Cause (boom :: String)
        c = Cause.fail (Variant.inj (Proxy :: _ "boom") "x")

        k :: Int -> F.RIO () (boom :: String) Int
        k _ = do
          F.liftEffect (Ref.write true ranRef)
          pure 1

        prog :: F.RIO () (boom :: String) Int
        prog = F.failCause c >>= k
      out <- runAff prog {}
      case out of
        Fail _ -> pure unit
        other -> fail ("expected Fail, got " <> describeOutcome other)
      ran <- liftEffect (Ref.read ranRef)
      ran `shouldEqual` false

  describe "Monad laws" do
    it "left identity: pure a >>= f == f a" do
      let
        f :: Int -> F.RIO () () Int
        f n = pure (n * 3)
      eqRun (pure 7 >>= f) (f 7)

    it "right identity: m >>= pure == m" do
      let
        m :: F.RIO () () Int
        m = pure 42
      eqRun (m >>= pure) m

    it "associativity: (m >>= f) >>= g == m >>= (\\x -> f x >>= g)" do
      let
        m :: F.RIO () () Int
        m = pure 4

        f :: Int -> F.RIO () () Int
        f n = pure (n + 1)

        g :: Int -> F.RIO () () Int
        g n = pure (n * 2)
      eqRun ((m >>= f) >>= g) (m >>= \x -> f x >>= g)

  describe "Apply laws" do
    it "identity: pure identity <*> v == v" do
      eqRun (pure identity <*> pure 9 :: F.RIO () () Int) (pure 9)

    it "homomorphism: pure f <*> pure x == pure (f x)" do
      eqRun
        (pure (_ + 1) <*> pure 41 :: F.RIO () () Int)
        (pure 42)

    it "interchange: u <*> pure y == pure (\\f -> f y) <*> u" do
      let
        u :: F.RIO () () (Int -> Int)
        u = pure (_ + 5)
      eqRun (u <*> pure 10) (pure (\f -> f 10) <*> u)

  describe "side effects through fusions" do
    it "map composition over a sync effect runs the effect exactly once" do
      counter <- liftEffect (Ref.new 0)
      let
        bump :: F.RIO () () Int
        bump = F.liftEffect (Ref.modify (_ + 1) counter)

        prog :: F.RIO () () Int
        prog = map (_ * 2) (map (_ + 1) bump)
      out <- runAff prog {}
      assertSuccess out 4
      seen <- liftEffect (Ref.read counter)
      seen `shouldEqual` 1

    it "apply through fusions runs both sides exactly once, left then right" do
      events <- liftEffect (Ref.new ([] :: Array String))
      let
        record :: String -> F.RIO () () Unit
        record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

        lhs :: F.RIO () () (Int -> Int)
        lhs = record "lhs" *> pure (_ + 1)

        rhs :: F.RIO () () Int
        rhs = record "rhs" *> pure 41

        prog :: F.RIO () () Int
        prog = lhs <*> rhs
      out <- runAff prog {}
      assertSuccess out 42
      seq <- liftEffect (Ref.read events)
      seq `shouldEqual` [ "lhs", "rhs" ]

  describe "spine walk leaf fast paths" do
    it "two siblings via fork+join return both child values" do
      let
        prog :: F.RIO () () Int
        prog = do
          fa <- F.fork (pure 10 :: F.RIO () () Int)
          fb <- F.fork (pure 32 :: F.RIO () () Int)
          a <- F.join fa
          b <- F.join fb
          pure (a + b)
      out <- runAff prog {}
      assertSuccess out 42

    it "16-wide fork+join under traverse matches forkAll+joinAll" do
      let
        bodies :: Array (F.RIO () () Int)
        bodies = map (\n -> pure (n * 2)) (Array.range 1 16)

        viaTraverse :: F.RIO () () (Array Int)
        viaTraverse = do
          fibs <- traverseRIO F.fork bodies
          traverseRIO F.join fibs

        viaForkAll :: F.RIO () () (Array Int)
        viaForkAll = do
          fibs <- F.forkAll bodies
          F.joinAll fibs
      t <- runAff viaTraverse {}
      a <- runAff viaForkAll {}
      case t, a of
        Success ts, Success as -> ts `shouldEqual` as
        _, _ -> fail
          ( "expected matching Success, got "
              <> describeOutcome t
              <> " vs "
              <> describeOutcome a
          )

  describe "forEach vs traverse equivalence" do
    it "forEach matches a hand-rolled sequential traverse for pure work" do
      let
        xs = Array.range 1 16
        body n = pure (n + 100) :: F.RIO () () Int
      fr <- runAff (F.forEach body xs) {}
      tr <- runAff (traverseRIO body xs) {}
      case fr, tr of
        Success a, Success b -> a `shouldEqual` b
        _, _ -> fail
          ( "expected matching Success, got "
              <> describeOutcome fr
              <> " vs "
              <> describeOutcome tr
          )

eqRun
  :: forall a
   . Eq a
  => Show a
  => F.RIO () () a
  -> F.RIO () () a
  -> Aff Unit
eqRun l r = do
  lOut <- runAff l {}
  rOut <- runAff r {}
  case lOut, rOut of
    Success a, Success b -> a `shouldEqual` b
    _, _ -> fail
      ( "law violation: "
          <> describeOutcome lOut
          <> " vs "
          <> describeOutcome rOut
      )

traverseRIO
  :: forall a b
   . (a -> F.RIO () () b)
  -> Array a
  -> F.RIO () () (Array b)
traverseRIO f xs0 = go xs0 []
  where
  go ys acc = case Array.uncons ys of
    Nothing -> pure acc
    Just { head, tail } -> do
      b <- f head
      go tail (acc <> [ b ])

assertSuccess
  :: forall e a
   . Eq a
  => Show a
  => Outcome e a
  -> a
  -> Aff Unit
assertSuccess (Success a) expected = a `shouldEqual` expected
assertSuccess other _ = fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
