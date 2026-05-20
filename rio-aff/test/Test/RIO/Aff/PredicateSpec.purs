module Test.RIO.Aff.PredicateSpec (spec) where

import Prelude hiding (not)

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Predicate
  ( Predicate
  , all
  , always
  , and
  , any
  , contramap
  , mkPredicate
  , never
  , not
  , or
  , runPredicate
  )

type User = { age :: Int, name :: String }

positive :: Predicate Int
positive = mkPredicate (_ > 0)

even' :: Predicate Int
even' = mkPredicate (\n -> mod n 2 == 0)

spec :: Spec Unit
spec = describe "RIO.Aff.Predicate" do
  describe "mkPredicate + runPredicate" do
    it "round-trips a function" do
      runPredicate positive 3 `shouldEqual` true
      runPredicate positive 0 `shouldEqual` false
      runPredicate positive (-5) `shouldEqual` false

  describe "always / never" do
    it "always accepts every input" do
      runPredicate (always :: Predicate Int) 0 `shouldEqual` true
      runPredicate (always :: Predicate Int) 999 `shouldEqual` true

    it "never rejects every input" do
      runPredicate (never :: Predicate Int) 0 `shouldEqual` false
      runPredicate (never :: Predicate Int) 999 `shouldEqual` false

  describe "and" do
    it "is true iff both predicates hold" do
      let p = positive `and` even'
      runPredicate p 4 `shouldEqual` true
      runPredicate p 3 `shouldEqual` false
      runPredicate p (-2) `shouldEqual` false
      runPredicate p 0 `shouldEqual` false

    it "treats `always` as its identity" do
      runPredicate (positive `and` always) 7 `shouldEqual` true
      runPredicate (always `and` positive) 7 `shouldEqual` true
      runPredicate (positive `and` always) (-1) `shouldEqual` false

  describe "or" do
    it "is true iff at least one predicate holds" do
      let p = positive `or` even'
      runPredicate p 4 `shouldEqual` true
      runPredicate p (-2) `shouldEqual` true
      runPredicate p 3 `shouldEqual` true
      runPredicate p (-1) `shouldEqual` false

    it "treats `never` as its identity" do
      runPredicate (positive `or` never) 7 `shouldEqual` true
      runPredicate (never `or` positive) 7 `shouldEqual` true
      runPredicate (positive `or` never) (-1) `shouldEqual` false

  describe "not" do
    it "flips the verdict" do
      runPredicate (not positive) 0 `shouldEqual` true
      runPredicate (not positive) 3 `shouldEqual` false

    it "double negation is the original predicate (on samples)" do
      runPredicate (not (not positive)) 3 `shouldEqual` runPredicate positive 3
      runPredicate (not (not positive)) (-1) `shouldEqual` runPredicate positive (-1)

  describe "contramap" do
    it "adapts the input via the projection function" do
      let
        adult :: Predicate User
        adult = contramap _.age (mkPredicate (_ >= 18))
      runPredicate adult { age: 30, name: "alice" } `shouldEqual` true
      runPredicate adult { age: 17, name: "bob" } `shouldEqual` false

    it "composes with `and` after adaptation" do
      let
        adult :: Predicate User
        adult = contramap _.age (mkPredicate (_ >= 18))

        named :: Predicate User
        named = contramap _.name (mkPredicate (_ /= ""))
      runPredicate (adult `and` named) { age: 30, name: "alice" }
        `shouldEqual` true
      runPredicate (adult `and` named) { age: 30, name: "" }
        `shouldEqual` false

  describe "any / all" do
    it "any of [] is `never`" do
      runPredicate (any ([] :: Array (Predicate Int))) 5 `shouldEqual` false

    it "all of [] is `always`" do
      runPredicate (all ([] :: Array (Predicate Int))) 5 `shouldEqual` true

    it "any folds with or" do
      let p = any [ positive, even' ]
      runPredicate p 3 `shouldEqual` true
      runPredicate p (-2) `shouldEqual` true
      runPredicate p (-1) `shouldEqual` false

    it "all folds with and" do
      let p = all [ positive, even' ]
      runPredicate p 4 `shouldEqual` true
      runPredicate p 3 `shouldEqual` false
      runPredicate p (-2) `shouldEqual` false
