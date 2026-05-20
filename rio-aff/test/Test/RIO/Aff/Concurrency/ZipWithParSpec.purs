module Test.RIO.Aff.Concurrency.ZipWithParSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, fail, runRIO, runRIO', zipPar, zipWithPar)

spec :: Spec Unit
spec = describe "RIO.Aff.Concurrency.zipWithPar" do
  it "combines two parallel successes with the supplied function" do
    let
      program :: RIO () () Int
      program = zipWithPar (+) (pure 3) (pure 4)
    result <- runRIO' program
    result `shouldEqual` 7

  it "preserves argument order in the combiner (left then right)" do
    let
      program :: RIO () () (Tuple String Int)
      program = zipWithPar Tuple (pure "L") (pure 99)
    result <- runRIO' program
    result `shouldEqual` Tuple "L" 99

  it "surfaces the first typed failure on the row" do
    let
      program :: RIO () (boom :: Int) Int
      program = zipWithPar
        (\a b -> a + b)
        (pure 1)
        (fail (Proxy :: Proxy "boom") 99)
    result <- runRIO program
    case result of
      Left v ->
        let
          n =
            Variant.case_
              # Variant.on (Proxy :: Proxy "boom") identity
              $ v
        in
          n `shouldEqual` 99
      Right _ -> 1 `shouldEqual` 0

  it "agrees with zipPar followed by a pure combine" do
    let
      a = pure 11 :: RIO () () Int
      b = pure 7 :: RIO () () Int
      lhs = zipWithPar (\x y -> x * 10 + y) a b
      rhs = map (\(Tuple x y) -> x * 10 + y) (zipPar a b)
    left <- runRIO' lhs
    right <- runRIO' rhs
    left `shouldEqual` right
