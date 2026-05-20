module Test.RIO.Aff.ConditionalSpec (spec) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, ifM, runRIO', unlessM, whenM)

spec :: Spec Unit
spec = describe "RIO.Aff.Core (whenM / unlessM / ifM)" do

  describe "whenM" do
    it "runs the body when the predicate returns true" do
      counter <- liftEffect (Ref.new 0)
      let
        program :: RIO () () Unit
        program = whenM (pure true) (liftEffect (Ref.modify_ (_ + 1) counter))
      _ <- runRIO' program
      final <- liftEffect (Ref.read counter)
      final `shouldEqual` 1

    it "skips the body when the predicate returns false" do
      counter <- liftEffect (Ref.new 0)
      let
        program :: RIO () () Unit
        program = whenM (pure false) (liftEffect (Ref.modify_ (_ + 1) counter))
      _ <- runRIO' program
      final <- liftEffect (Ref.read counter)
      final `shouldEqual` 0

    it "evaluates the effectful predicate exactly once" do
      probe <- liftEffect (Ref.new 0)
      let
        cond :: RIO () () Boolean
        cond = do
          liftEffect (Ref.modify_ (_ + 1) probe)
          pure true

        program :: RIO () () Unit
        program = whenM cond (pure unit)
      _ <- runRIO' program
      times <- liftEffect (Ref.read probe)
      times `shouldEqual` 1

  describe "unlessM" do
    it "runs the body when the predicate returns false" do
      counter <- liftEffect (Ref.new 0)
      let
        program :: RIO () () Unit
        program =
          unlessM (pure false) (liftEffect (Ref.modify_ (_ + 1) counter))
      _ <- runRIO' program
      final <- liftEffect (Ref.read counter)
      final `shouldEqual` 1

    it "skips the body when the predicate returns true" do
      counter <- liftEffect (Ref.new 0)
      let
        program :: RIO () () Unit
        program =
          unlessM (pure true) (liftEffect (Ref.modify_ (_ + 1) counter))
      _ <- runRIO' program
      final <- liftEffect (Ref.read counter)
      final `shouldEqual` 0

  describe "ifM" do
    it "runs the then-branch when the predicate returns true" do
      let
        program :: RIO () () String
        program = ifM (pure true) (pure "yes") (pure "no")
      result <- runRIO' program
      result `shouldEqual` "yes"

    it "runs the else-branch when the predicate returns false" do
      let
        program :: RIO () () String
        program = ifM (pure false) (pure "yes") (pure "no")
      result <- runRIO' program
      result `shouldEqual` "no"

    it "evaluates the predicate but only one arm" do
      probe <- liftEffect (Ref.new 0)
      let
        thenBranch :: RIO () () String
        thenBranch = do
          liftEffect (Ref.modify_ (_ + 1) probe)
          pure "ran-then"

        elseBranch :: RIO () () String
        elseBranch = do
          liftEffect (Ref.modify_ (_ + 100) probe)
          pure "ran-else"

        program :: RIO () () String
        program = ifM (pure true) thenBranch elseBranch
      result <- runRIO' program
      result `shouldEqual` "ran-then"
      total <- liftEffect (Ref.read probe)
      total `shouldEqual` 1
