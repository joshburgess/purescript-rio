module Test.RIO.STMSpec (spec) where

import Prelude

import Data.Array (range)
import Data.Either (Either(..))
import Data.Traversable (traverse)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, catchTag, fork, join, runRIO, runRIO')
import RIO.STM
  ( atomically
  , check
  , failSTM
  , modifyTRef
  , newTRef
  , orElse
  , readTRef
  , retry
  , writeTRef
  )

spec :: Spec Unit
spec = do
  describe "RIO.STM (v0.2)" do
    describe "atomically" do
      it "applies writes after a successful transaction" do
        let
          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 0)
            atomically (writeTRef ref 7)
            atomically (readTRef ref)
        result <- runRIO' program
        result `shouldEqual` 7

      it "subsequent reads in the same tx see staged writes" do
        let
          program :: RIO () () Int
          program = atomically do
            ref <- newTRef 1
            writeTRef ref 2
            readTRef ref
        result <- runRIO' program
        result `shouldEqual` 2

      it "modifyTRef composes a read and a write" do
        let
          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 10)
            atomically (modifyTRef ref (_ * 3))
            atomically (readTRef ref)
        result <- runRIO' program
        result `shouldEqual` 30

    describe "failSTM" do
      it "aborts the transaction and surfaces the failure" do
        let
          program :: RIO () (boom :: Unit) Int
          program = do
            ref <- atomically (newTRef 0)
            _ <- atomically do
              writeTRef ref 99
              _ <- failSTM (Proxy :: Proxy "boom") unit
              pure 0
            atomically (readTRef ref)
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

      it "discards writes from a failed transaction" do
        let
          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 0)
            _ <- catchTag (Proxy :: Proxy "boom") (\_ -> pure 0)
              ( atomically do
                  writeTRef ref 99
                  _ <- failSTM (Proxy :: Proxy "boom") unit
                  pure (0 :: Int)
              )
            atomically (readTRef ref)
        result <- runRIO' program
        result `shouldEqual` 0

    describe "retry" do
      it "suspends until another fiber writes a read TRef" do
        events <- liftEffect (Ref.new [])
        let
          push :: forall r e. String -> RIO r e Unit
          push s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) events)

          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 0)
            push "before-fork"
            waiter <- fork do
              v <- atomically do
                x <- readTRef ref
                check (x > 0)
                pure x
              push "after-await"
              pure v
            liftAff (delay (Milliseconds 20.0))
            push "before-write"
            atomically (writeTRef ref 42)
            join waiter

        result <- runRIO' program
        result `shouldEqual` 42
        order <- liftEffect (Ref.read events)
        order `shouldEqual`
          [ "before-fork", "before-write", "after-await" ]

    describe "orElse" do
      it "falls through when the left side retries" do
        let
          program :: RIO () () Int
          program = do
            refA <- atomically (newTRef 0)
            refB <- atomically (newTRef 99)
            atomically do
              orElse
                ( do
                    a <- readTRef refA
                    check (a > 0)
                    pure a
                )
                (readTRef refB)
        result <- runRIO' program
        result `shouldEqual` 99

      it "does not fall through on a typed failure" do
        let
          program :: RIO () (boom :: Unit) Int
          program = atomically do
            orElse
              ( do
                  _ <- failSTM (Proxy :: Proxy "boom") unit
                  pure 0
              )
              (pure 99)
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

    describe "concurrent increments" do
      it "preserves the invariant under many parallel updates" do
        let
          n = 50

          program :: RIO () () Int
          program = do
            ref <- atomically (newTRef 0)
            fibers <- traverse
              (\_ -> fork (atomically (modifyTRef ref (_ + 1))))
              (range 1 n)
            _ <- traverse join fibers
            atomically (readTRef ref)
        result <- runRIO' program
        result `shouldEqual` n
