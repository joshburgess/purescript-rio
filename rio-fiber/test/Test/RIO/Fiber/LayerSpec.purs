module Test.RIO.Fiber.LayerSpec (spec) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.Layer as Layer
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "rio-fiber: Layer" do
  it "fromValue + provide makes the record available" do
    let
      makeGreeting :: Layer.Layer () () (greeting :: String)
      makeGreeting = Layer.fromValue { greeting: "hello" }

      prog :: F.RIO () () String
      prog = Layer.provide makeGreeting do
        env <- F.ask
        pure env.greeting
    out <- runAff prog {}
    case out of
      Success s -> s `shouldEqual` "hello"
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "chainLayer threads the first output into the second" do
    let
      step1 :: Layer.Layer () () (n :: Int)
      step1 = Layer.fromValue { n: 21 }

      step2 :: Layer.Layer () (n :: Int) (m :: Int)
      step2 = Layer.fromRIO do
        env <- F.ask
        pure { m: env.n * 2 }

      combined :: Layer.Layer () () (m :: Int)
      combined = Layer.chainLayer step1 step2

      prog :: F.RIO () () Int
      prog = Layer.provide combined do
        env <- F.ask
        pure env.m
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 42
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "mergeLayers builds both records from the same input" do
    let
      a :: Layer.Layer () () (x :: Int)
      a = Layer.fromValue { x: 1 }

      b :: Layer.Layer () () (y :: String)
      b = Layer.fromValue { y: "hi" }

      merged :: Layer.Layer () () (x :: Int, y :: String)
      merged = Layer.mergeLayers a b

      prog :: F.RIO () () { x :: Int, y :: String }
      prog = Layer.provide merged F.ask
    out <- runAff prog {}
    case out of
      Success r -> do
        r.x `shouldEqual` 1
        r.y `shouldEqual` "hi"
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "provideScoped runs the layer's finalizers on exit" do
    ref <- liftEffect (Ref.new false)
    let
      buildResource
        :: Scope.Scope -> F.RIO () () (Record (n :: Int))
      buildResource scope = do
        n <- Scope.acquireRelease scope
          (pure 7)
          (\_ -> F.liftEffect (Ref.write true ref))
        pure { n }

      prog :: F.RIO () () Int
      prog = Layer.provideScoped buildResource do
        env <- F.ask
        pure env.n
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 7
      other -> fail ("expected Success, got " <> describeOutcome other)
    -- the scoped finalizer fires fire-and-forget; wait for it
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    fired <- liftEffect (Ref.read ref)
    fired `shouldEqual` true

  it "chainLayer associativity: (a >>> b) >>> c == a >>> (b >>> c)" do
    let
      a :: Layer.Layer () () (n :: Int)
      a = Layer.fromValue { n: 1 }

      b :: Layer.Layer () (n :: Int) (m :: Int)
      b = Layer.fromRIO do
        env <- F.ask
        pure { m: env.n + 10 }

      c :: Layer.Layer () (m :: Int) (k :: Int)
      c = Layer.fromRIO do
        env <- F.ask
        pure { k: env.m * 2 }

      lhs :: Layer.Layer () () (k :: Int)
      lhs = Layer.chainLayer (Layer.chainLayer a b) c

      rhs :: Layer.Layer () () (k :: Int)
      rhs = Layer.chainLayer a (Layer.chainLayer b c)

      readK l = Layer.provide l (F.ask <#> _.k :: F.RIO (k :: Int) () Int)
    lOut <- runAff (readK lhs) {}
    rOut <- runAff (readK rhs) {}
    case lOut, rOut of
      Success x, Success y -> do
        x `shouldEqual` 22
        x `shouldEqual` y
      _, _ -> fail
        ( "expected matching Success, got "
            <> describeOutcome lOut
            <> " vs "
            <> describeOutcome rOut
        )

  it "fromRIO can read the input environment when chained" do
    let
      seed :: Layer.Layer () () (start :: Int)
      seed = Layer.fromValue { start: 100 }

      bumped :: Layer.Layer () (start :: Int) (n :: Int)
      bumped = Layer.fromRIO do
        env <- F.ask
        pure { n: env.start + 5 }

      combined :: Layer.Layer () () (n :: Int)
      combined = Layer.chainLayer seed bumped

      prog :: F.RIO () () Int
      prog = Layer.provide combined do
        env <- F.ask
        pure env.n
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 105
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "a failing layer build propagates the typed error" do
    let
      bad :: Layer.Layer (oops :: String) () (n :: Int)
      bad = Layer.fromRIO
        (F.fail (Variant.inj (Proxy :: _ "oops") "build failed"))

      prog :: F.RIO () (oops :: String) Int
      prog = Layer.provide bad do
        env <- F.ask
        pure env.n
    out <- runAff prog {}
    case out of
      Fail v ->
        (Variant.case_ # Variant.on (Proxy :: _ "oops") identity) v
          `shouldEqual` "build failed"
      other -> fail ("expected Fail, got " <> describeOutcome other)

  it "mergeLayers preserves both halves' values from a shared input" do
    let
      seed :: Layer.Layer () () (start :: Int)
      seed = Layer.fromValue { start: 4 }

      timesTwo :: Layer.Layer () (start :: Int) (doubled :: Int)
      timesTwo = Layer.fromRIO do
        env <- F.ask
        pure { doubled: env.start * 2 }

      plusOne :: Layer.Layer () (start :: Int) (plus :: Int)
      plusOne = Layer.fromRIO do
        env <- F.ask
        pure { plus: env.start + 1 }

      merged :: Layer.Layer () (start :: Int) (doubled :: Int, plus :: Int)
      merged = Layer.mergeLayers timesTwo plusOne

      combined :: Layer.Layer () () (doubled :: Int, plus :: Int)
      combined = Layer.chainLayer seed merged

      prog :: F.RIO () () { doubled :: Int, plus :: Int }
      prog = Layer.provide combined F.ask
    out <- runAff prog {}
    case out of
      Success r -> do
        r.doubled `shouldEqual` 8
        r.plus `shouldEqual` 5
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "provideScoped finalizer runs after a failing body" do
    ref <- liftEffect (Ref.new false)
    let
      buildResource
        :: Scope.Scope -> F.RIO () (boom :: String) (Record (n :: Int))
      buildResource scope = do
        n <- Scope.acquireRelease scope
          (pure 1)
          (\_ -> F.liftEffect (Ref.write true ref))
        pure { n }

      prog :: F.RIO () (boom :: String) Int
      prog = Layer.provideScoped buildResource
        (F.fail (Variant.inj (Proxy :: _ "boom") "use failed"))
    out <- runAff prog {}
    case out of
      Fail _ -> pure unit
      other -> fail ("expected Fail, got " <> describeOutcome other)
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    fired <- liftEffect (Ref.read ref)
    fired `shouldEqual` true

  it "scoped: a layer can register finalizers via its scope" do
    ref <- liftEffect (Ref.new false)
    let
      buildResource
        :: Scope.Scope -> F.RIO () () (Record (n :: Int))
      buildResource scope = do
        n <- Scope.acquireRelease scope
          (pure 9)
          (\_ -> F.liftEffect (Ref.write true ref))
        pure { n }

      layer :: Layer.Layer () () (n :: Int)
      layer = Layer.scoped buildResource

      prog :: F.RIO () () Int
      prog = Layer.provide layer do
        env <- F.ask
        pure env.n
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 9
      other -> fail ("expected Success, got " <> describeOutcome other)
    _ <- runAff (F.sleep (Milliseconds 10.0) :: F.RIO () () Unit) {}
    fired <- liftEffect (Ref.read ref)
    fired `shouldEqual` true

  it "memoize: a memoized layer runs its build exactly once across uses" do
    runs <- liftEffect (Ref.new 0)
    let
      raw :: Layer.Layer () () (n :: Int)
      raw = Layer.fromRIO do
        F.liftEffect (Ref.modify_ (_ + 1) runs)
        pure { n: 11 }
    memo <- liftEffect (Layer.memoize raw)
    let
      readN :: Layer.Layer () () (n :: Int) -> F.RIO () () Int
      readN l = Layer.provide l (F.ask <#> _.n)
    a <- runAff (readN memo) {}
    b <- runAff (readN memo) {}
    c <- runAff (readN memo) {}
    case a, b, c of
      Success x, Success y, Success z -> do
        x `shouldEqual` 11
        y `shouldEqual` 11
        z `shouldEqual` 11
      _, _, _ -> fail "expected three Success"
    count <- liftEffect (Ref.read runs)
    count `shouldEqual` 1

  it "memoize: replays a typed failure without re-running the build" do
    runs <- liftEffect (Ref.new 0)
    let
      raw :: Layer.Layer (boom :: String) () (n :: Int)
      raw = Layer.fromRIO do
        F.liftEffect (Ref.modify_ (_ + 1) runs)
        F.fail (Variant.inj (Proxy :: _ "boom") "nope")
    memo <- liftEffect (Layer.memoize raw)
    let
      readN
        :: Layer.Layer (boom :: String) () (n :: Int)
        -> F.RIO () (boom :: String) Int
      readN l = Layer.provide l (F.ask <#> _.n)
    a <- runAff (readN memo) {}
    b <- runAff (readN memo) {}
    case a, b of
      Fail _, Fail _ -> pure unit
      _, _ -> fail "expected two Fail"
    count <- liftEffect (Ref.read runs)
    count `shouldEqual` 1

  it "fresh: identity on layer values (currently a no-op marker)" do
    let
      raw :: Layer.Layer () () (n :: Int)
      raw = Layer.fromValue { n: 3 }

      prog :: F.RIO () () Int
      prog = Layer.provide (Layer.fresh raw) (F.ask <#> _.n)
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 3
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
