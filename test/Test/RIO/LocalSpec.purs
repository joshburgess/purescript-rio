module Test.RIO.LocalSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Core
  ( RIO
  , catchTag
  , fail
  , fork
  , join
  , runRIO
  )
import RIO.Local (get, locally, newLocal, set, update)

spec :: Spec Unit
spec = do
  describe "RIO.Local" do
    describe "get / set / update" do
      it "get returns the initial value" do
        let
          program :: RIO () () String
          program = do
            l <- newLocal "init"
            get l
        result <- runRIO program
        result `shouldEqual` Right "init"

      it "set overwrites and subsequent get sees the new value" do
        let
          program :: RIO () () String
          program = do
            l <- newLocal "init"
            set l "next"
            get l
        result <- runRIO program
        result `shouldEqual` Right "next"

      it "update applies a function to the current value" do
        let
          program :: RIO () () Int
          program = do
            l <- newLocal 1
            update l (_ + 41)
            get l
        result <- runRIO program
        result `shouldEqual` Right 42

    describe "locally" do
      it "scopes the value to a block and restores after success" do
        let
          program :: RIO () () { inside :: String, after :: String }
          program = do
            l <- newLocal "outer"
            inside <- locally l "inner" (get l)
            after <- get l
            pure { inside, after }
        result <- runRIO program
        result `shouldEqual` Right { inside: "inner", after: "outer" }

      it "restores the previous value when the body raises a typed failure" do
        let
          program :: RIO () () String
          program = do
            l <- newLocal "outer"
            _ <- catchTag (Proxy :: Proxy "boom") (\_ -> pure unit)
              ( locally l "inner" do
                  fail (Proxy :: Proxy "boom") unit
              )
            get l
        result <- runRIO program
        result `shouldEqual` Right "outer"

      it "nests: inner restores to outer's value, not the initial" do
        let
          program
            :: RIO ()
                 ()
                 { atInner :: String
                 , atMiddle :: String
                 , atOuter :: String
                 }
          program = do
            l <- newLocal "outer"
            res <- locally l "middle" do
              atInner <- locally l "inner" (get l)
              atMiddle <- get l
              pure { atInner, atMiddle }
            atOuter <- get l
            pure
              { atInner: res.atInner
              , atMiddle: res.atMiddle
              , atOuter
              }
        result <- runRIO program
        result `shouldEqual` Right
          { atInner: "inner"
          , atMiddle: "middle"
          , atOuter: "outer"
          }

    describe "fork inheritance (implicit-context semantics)" do
      it "a forked fiber observes whatever value the parent has at read-time" do
        let
          program :: RIO () () String
          program = do
            l <- newLocal "initial"
            set l "before-fork"
            fib <- fork do
              liftAff (delay (Milliseconds 5.0))
              get l
            -- Parent mutates while child is still sleeping; child
            -- reads the post-mutation value because the Ref is
            -- shared.
            set l "after-fork"
            join fib
        result <- runRIO program
        result `shouldEqual` Right "after-fork"

      it
        "a child fiber inside `locally` sees the override; restore awaits the child if join is used"
        do
          let
            program :: RIO () () { childSaw :: String, afterLocally :: String }
            program = do
              l <- newLocal "outer"
              res <- locally l "inner" do
                fib <- fork (get l)
                childSaw <- join fib
                pure { childSaw }
              afterLocally <- get l
              pure
                { childSaw: res.childSaw
                , afterLocally
                }
          result <- runRIO program
          result `shouldEqual` Right
            { childSaw: "inner"
            , afterLocally: "outer"
            }

      it "`set` from a child fiber is visible to the parent (shared Ref)" do
        let
          program :: RIO () () String
          program = do
            l <- newLocal "initial"
            fib <- fork (set l "from-child")
            _ <- join fib
            get l
        result <- runRIO program
        result `shouldEqual` Right "from-child"
