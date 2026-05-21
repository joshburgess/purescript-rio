module Test.RIO.Fiber.FiberSetSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import RIO.Fiber.Core (Outcome(..))
import RIO.Fiber.Core as F
import RIO.Fiber.FiberSet as FS
import RIO.Fiber.Scope as Scope
import Test.RIO.Fiber.Helpers (runAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = describe "rio-fiber: FiberSet" do
  it "tracks the size of running fibers" do
    let
      prog :: F.RIO () () { mid :: Int, final :: Int }
      prog = Scope.scoped \scope -> do
        set <- FS.make scope
        traverse_ (\_ -> FS.run set (F.sleep (Milliseconds 100.0)))
          (Array.range 1 5)
        F.sleep (Milliseconds 10.0)
        mid <- FS.size set
        FS.awaitEmpty set
        final <- FS.size set
        pure { mid, final }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.mid `shouldEqual` 5
        r.final `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "removes fibers from the set when they finish" do
    let
      prog :: F.RIO () () Int
      prog = Scope.scoped \scope -> do
        set <- FS.make scope
        _ <- FS.run set (pure 1)
        _ <- FS.run set (pure 2)
        FS.awaitEmpty set
        FS.size set
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "awaitEmpty resumes immediately when the set is already empty" do
    let
      prog :: F.RIO () () Int
      prog = Scope.scoped \scope -> do
        set <- FS.make scope
        FS.awaitEmpty set
        FS.size set
    out <- runAff prog {}
    case out of
      Success n -> n `shouldEqual` 0
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "scope close interrupts every fiber" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: forall e. String -> F.RIO () e Unit
      record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      prog :: F.RIO () () Unit
      prog = do
        Scope.scoped \scope -> do
          set <- FS.make scope
          traverse_
            ( \n -> FS.run set
                ( F.ensuring (record ("done-" <> show n))
                    (F.sleep (Milliseconds 200.0))
                )
            )
            (Array.range 1 3)
          F.sleep (Milliseconds 10.0)
        -- Wait one tick for the interrupt finalizers to run.
        F.sleep (Milliseconds 30.0)
    out <- runAff prog {}
    case out of
      Success _ -> do
        events <- liftEffect (Ref.read log)
        Array.length events `shouldEqual` 3
      other -> fail ("expected Success, got " <> describeOutcome other)

  it "interruptAll interrupts every fiber and reports the count" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: forall e. String -> F.RIO () e Unit
      record s = F.liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      prog :: F.RIO () () { n :: Int }
      prog = Scope.scoped \scope -> do
        set <- FS.make scope
        traverse_
          ( \k -> FS.run set
              ( F.ensuring (record ("done-" <> show k))
                  (F.sleep (Milliseconds 200.0))
              )
          )
          (Array.range 1 4)
        F.sleep (Milliseconds 5.0)
        n <- FS.interruptAll set
        FS.awaitEmpty set
        pure { n }
    out <- runAff prog {}
    case out of
      Success r -> do
        r.n `shouldEqual` 4
        events <- liftEffect (Ref.read log)
        Array.length events `shouldEqual` 4
      other -> fail ("expected Success, got " <> describeOutcome other)

describeOutcome :: forall e a. Outcome e a -> String
describeOutcome (Success _) = "Success"
describeOutcome (Fail _) = "Fail"
describeOutcome (Die _) = "Die"
describeOutcome Interrupted = "Interrupted"
