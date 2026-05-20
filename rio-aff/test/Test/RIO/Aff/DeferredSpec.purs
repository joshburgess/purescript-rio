module Test.RIO.Aff.DeferredSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core
  ( RIO
  , awaitDeferred
  , failDeferred
  , fork
  , join
  , makeDeferred
  , pollDeferred
  , runRIO
  , succeedDeferred
  )

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Deferred" do
    describe "succeedDeferred + awaitDeferred" do
      it "an awaiter sees the produced value" do
        let
          program :: RIO () () Int
          program = do
            d <- makeDeferred
            _ <- fork do
              liftAff (delay (Milliseconds 10.0))
              _ <- succeedDeferred d 7
              pure unit
            awaitDeferred d
        result <- runRIO program
        result `shouldEqual` (Right 7 :: Either _ Int)

      it "second succeedDeferred returns False (write-once)" do
        let
          program :: RIO () () { first :: Boolean, second :: Boolean }
          program = do
            d <- makeDeferred
            a <- succeedDeferred d 1
            b <- succeedDeferred d 2
            pure { first: a, second: b }
        result <- runRIO program
        result `shouldEqual`
          ( Right { first: true, second: false }
              :: Either _ { first :: Boolean, second :: Boolean }
          )

    describe "failDeferred + awaitDeferred" do
      it "surfaces a typed failure on the awaiter's row" do
        let
          program :: RIO () (boom :: Unit) Int
          program = do
            d <- makeDeferred
            _ <- failDeferred d (Variant.inj (Proxy :: Proxy "boom") unit)
            awaitDeferred d
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

      it "an awaiter blocks on an empty cell then surfaces a late typed failure" do
        -- Module docstring promises `awaitDeferred` "to read it
        -- (blocking until filled)" and that `failDeferred`
        -- "surfaces a typed failure on the awaiter's `e` row".
        -- The pinned "surfaces a typed failure on the awaiter's
        -- row" test fills the cell BEFORE calling
        -- `awaitDeferred`, so it never actually blocks; a
        -- regression that swapped `AVar.read` for `AVar.tryRead`
        -- in the empty-cell case (i.e., didn't block) would
        -- still pass that test because the cell is already
        -- filled when await is called. Pin the blocking-then-
        -- typed-failure path: fork the awaiter first on an empty
        -- cell, delay, then fail the cell from the parent. The
        -- joined fiber must surface the typed failure.
        let
          program :: RIO () (boom :: Unit) Int
          program = do
            d <- makeDeferred
            f <- fork (awaitDeferred d)
            liftAff (delay (Milliseconds 10.0))
            _ <- failDeferred d
              (Variant.inj (Proxy :: Proxy "boom") unit)
            join f
        result <- runRIO program
        case result of
          Left _ -> pure unit
          Right _ -> 1 `shouldEqual` 0

    describe "pollDeferred" do
      it "is Nothing before fill, Just after fill" do
        let
          program :: RIO () () { before :: Boolean, after :: Boolean }
          program = do
            d :: _ () Int <- makeDeferred
            before <- pollDeferred d
            _ <- succeedDeferred d 42
            after <- pollDeferred d
            pure { before: isNothing before, after: isJust after }
        result <- runRIO program
        result `shouldEqual`
          ( Right { before: true, after: true }
              :: Either _ { before :: Boolean, after :: Boolean }
          )

    describe "multiple awaiters" do
      it "all see the same value (read is non-destructive)" do
        let
          program :: RIO () () (Maybe Int)
          program = do
            d <- makeDeferred
            f1 <- fork (awaitDeferred d)
            f2 <- fork (awaitDeferred d)
            _ <- fork do
              liftAff (delay (Milliseconds 5.0))
              _ <- succeedDeferred d 99
              pure unit
            r1 <- join f1
            r2 <- join f2
            pure (if r1 == r2 then Just r1 else Nothing)
        result <- runRIO program
        result `shouldEqual` (Right (Just 99) :: Either _ (Maybe Int))

    describe "write-once across success and failure" do
      it "succeedDeferred after failDeferred returns False and the failure wins" do
        let
          program
            :: RIO ()
                 (boom :: Unit)
                 { firstFill :: Boolean, secondFill :: Boolean }
          program = do
            d :: _ (boom :: Unit) Int <- makeDeferred
            first <- failDeferred d (Variant.inj (Proxy :: Proxy "boom") unit)
            second <- succeedDeferred d 99
            pure { firstFill: first, secondFill: second }
        result <- runRIO program
        result `shouldEqual`
          ( Right { firstFill: true, secondFill: false }
              :: Either _ { firstFill :: Boolean, secondFill :: Boolean }
          )

      it "failDeferred after succeedDeferred returns False and the value wins" do
        let
          program :: RIO () (boom :: Unit) Int
          program = do
            d :: _ (boom :: Unit) Int <- makeDeferred
            _ <- succeedDeferred d 42
            _ <- failDeferred d (Variant.inj (Proxy :: Proxy "boom") unit)
            awaitDeferred d
        result <- runRIO program
        result `shouldEqual` (Right 42 :: Either _ Int)

    describe "pollDeferred after each fill kind" do
      it "Just (Right _) after succeedDeferred" do
        let
          program :: RIO () () (Maybe (Either (Variant ()) Int))
          program = do
            d :: _ () Int <- makeDeferred
            _ <- succeedDeferred d 5
            pollDeferred d
        result <- runRIO program
        result `shouldEqual`
          ( Right (Just (Right 5))
              :: Either _ (Maybe (Either (Variant ()) Int))
          )

      it "Just (Left _) after failDeferred" do
        let
          program
            :: RIO ()
                 ()
                 (Maybe (Either (Variant (boom :: Unit)) Int))
          program = do
            d :: _ (boom :: Unit) Int <- makeDeferred
            _ <- failDeferred d (Variant.inj (Proxy :: Proxy "boom") unit)
            pollDeferred d
        result <- runRIO program
        let
          ok = case result of
            Right (Just (Left _)) -> true
            _ -> false
        ok `shouldEqual` true

    describe "multiple awaiters on failure" do
      it "all see the same failure" do
        let
          program :: RIO () (boom :: Unit) Int
          program = do
            d <- makeDeferred
            f1 <- fork (awaitDeferred d)
            f2 <- fork (awaitDeferred d)
            _ <- fork do
              liftAff (delay (Milliseconds 5.0))
              _ <- failDeferred d (Variant.inj (Proxy :: Proxy "boom") unit)
              pure unit
            _ <- join f1
            join f2
        result <- runRIO program
        let
          ok = case result of
            Left _ -> true
            Right _ -> false
        ok `shouldEqual` true
