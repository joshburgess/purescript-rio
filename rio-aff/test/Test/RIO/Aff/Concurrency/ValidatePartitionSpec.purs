module Test.RIO.Aff.Concurrency.ValidatePartitionSpec (spec) where

import Prelude

import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, fail, runRIO')
import RIO.Aff.Concurrency (partitionPar, validatePar)

spec :: Spec Unit
spec = describe "RIO.Aff.Concurrency (validatePar / partitionPar)" do

  describe "validatePar" do
    it "returns Right with values in input order when every branch succeeds" do
      let
        program
          :: RIO () ()
               ( Either (NEA.NonEmptyArray (Variant.Variant (boom :: Int)))
                   (Array Int)
               )
        program = validatePar
          (\a -> (pure (a * 2) :: RIO () (boom :: Int) Int))
          [ 1, 2, 3, 4 ]
      result <- runRIO' program
      result `shouldEqual` (Right [ 2, 4, 6, 8 ])

    it "accumulates every failure rather than short-circuiting on the first" do
      let
        program
          :: RIO () ()
               ( Either (NEA.NonEmptyArray (Variant.Variant (boom :: Int)))
                   (Array Int)
               )
        program = validatePar
          ( \a ->
              if a `mod` 2 == 0 then pure a
              else (fail (Proxy :: Proxy "boom") a :: RIO () (boom :: Int) Int)
          )
          [ 1, 2, 3, 4, 5 ]
      result <- runRIO' program
      case result of
        Left errs ->
          let
            payloads = map
              ( Variant.case_
                  # Variant.on (Proxy :: Proxy "boom") identity
              )
              (NEA.toArray errs)
          in
            payloads `shouldEqual` [ 1, 3, 5 ]
        Right _ -> 1 `shouldEqual` 0

    it "runs every action even when one fails (no short-circuit)" do
      counter <- liftEffect (Ref.new 0)
      let
        program
          :: RIO () ()
               ( Either (NEA.NonEmptyArray (Variant.Variant (boom :: Int)))
                   (Array Int)
               )
        program = validatePar
          ( \a -> do
              _ <- liftEffect (Ref.modify_ (_ + 1) counter)
              if a == 2 then
                (fail (Proxy :: Proxy "boom") a :: RIO () (boom :: Int) Int)
              else
                pure a
          )
          [ 1, 2, 3 ]
      _ <- runRIO' program
      seen <- liftEffect (Ref.read counter)
      seen `shouldEqual` 3

  describe "partitionPar" do
    it "splits results into (failures, successes), preserving input order" do
      let
        program
          :: RIO () ()
               (Tuple (Array (Variant.Variant (boom :: Int))) (Array Int))
        program = partitionPar
          ( \a ->
              if a `mod` 2 == 0 then pure a
              else (fail (Proxy :: Proxy "boom") a :: RIO () (boom :: Int) Int)
          )
          [ 1, 2, 3, 4, 5 ]
      Tuple errs succs <- runRIO' program
      let
        errPayloads = map
          ( Variant.case_
              # Variant.on (Proxy :: Proxy "boom") identity
          )
          errs
      errPayloads `shouldEqual` [ 1, 3, 5 ]
      succs `shouldEqual` [ 2, 4 ]

    it "is total: empty errs on all-success, empty succs on all-failure" do
      let
        allOk
          :: RIO () ()
               (Tuple (Array (Variant.Variant (boom :: Int))) (Array Int))
        allOk = partitionPar
          (\a -> (pure a :: RIO () (boom :: Int) Int))
          [ 1, 2, 3 ]
      Tuple e1 s1 <- runRIO' allOk
      e1 `shouldEqual` []
      s1 `shouldEqual` [ 1, 2, 3 ]
      let
        allBad
          :: RIO () ()
               (Tuple (Array (Variant.Variant (boom :: Int))) (Array Int))
        allBad = partitionPar
          ( \a ->
              (fail (Proxy :: Proxy "boom") a :: RIO () (boom :: Int) Int)
          )
          [ 1, 2, 3 ]
      Tuple e2 s2 <- runRIO' allBad
      let
        ePayloads = map
          ( Variant.case_
              # Variant.on (Proxy :: Proxy "boom") identity
          )
          e2
      ePayloads `shouldEqual` [ 1, 2, 3 ]
      s2 `shouldEqual` []

    it "handles the empty input case as (empty, empty)" do
      let
        program
          :: RIO () ()
               (Tuple (Array (Variant.Variant (boom :: Int))) (Array Int))
        program = partitionPar
          (\a -> (pure a :: RIO () (boom :: Int) Int))
          []
      Tuple errs succs <- runRIO' program
      errs `shouldEqual` []
      succs `shouldEqual` []
