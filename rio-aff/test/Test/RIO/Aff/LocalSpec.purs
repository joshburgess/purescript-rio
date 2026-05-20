module Test.RIO.Aff.LocalSpec (spec) where

import Prelude hiding (join)

import Data.Either (Either(..))
import Effect.Aff (Milliseconds(..), attempt, delay, error, forkAff, killFiber)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core
  ( RIO
  , catchTag
  , die
  , fail
  , fork
  , join
  , runRIO
  , runRIO'
  )
import Effect.Class (liftEffect)

import RIO.Aff.Local (get, locally, newLocal, newLocalEffect, set, update)

spec :: Spec Unit
spec = do
  describe "RIO.Aff.Local" do
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

      it "newLocalEffect produces a Local usable from within RIO" do
        -- Docstring promise: newLocalEffect is the Effect-typed
        -- variant "for callers that build their environment
        -- record outside an RIO action (e.g. at the top of main
        -- before launchAff_)". Pin that a Local allocated in
        -- Effect and then captured in a program is observable
        -- through get/set/update with the same semantics as
        -- newLocal.
        l <- liftEffect (newLocalEffect 7)
        let
          program :: RIO () () { initial :: Int, afterUpdate :: Int }
          program = do
            initial <- get l
            update l (_ + 100)
            afterUpdate <- get l
            pure { initial, afterUpdate }
        result <- runRIO program
        result `shouldEqual`
          ( Right { initial: 7, afterUpdate: 107 }
              :: Either _ { initial :: Int, afterUpdate :: Int }
          )

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

      it "restores the previous value when the body raises a defect" do
        -- Docstring promise: `locally` restores the previous
        -- value "regardless of how it terminates (success, typed
        -- failure, defect, or interrupt)". The success and typed-
        -- failure paths are pinned above; pin the defect path so
        -- the full bracket contract is documented.
        l <- liftEffect (newLocalEffect "outer")
        let
          program :: RIO () () Unit
          program = locally l "inner" (die (error "boom"))
        _ <- attempt (runRIO' program)
        after <- runRIO' (get l :: RIO () () String)
        after `shouldEqual` "outer"

      it "restores the previous value when the fiber is killed mid-body" do
        -- Pin the last termination path the `locally` docstring
        -- promises: a fiber kill mid-action must still trigger
        -- the `finally`-wired restore. Allocate the Local via
        -- newLocalEffect so the parent thread retains a handle
        -- it can read after the fiber is killed.
        l <- liftEffect (newLocalEffect "outer")
        let
          program :: RIO () () Unit
          program = locally l "inner"
            (liftAff (delay (Milliseconds 50.0)))
        f <- forkAff (runRIO' program)
        delay (Milliseconds 5.0)
        killFiber (error "test-cancel") f
        delay (Milliseconds 10.0)
        after <- runRIO' (get l :: RIO () () String)
        after `shouldEqual` "outer"

      it "nests: outer restores its snapshot when inner exits via typed failure" do
        -- Docstring promises BOTH that `locally` blocks nest and
        -- that they restore "regardless of how it terminates
        -- (success, typed failure, defect, or interrupt)". The
        -- success cross-product is pinned by "nests: inner
        -- restores to outer's value, not the initial" below; the
        -- termination cross-products are pinned only at depth-1.
        -- Pin the depth-2-on-failure case: the inner `locally`'s
        -- typed failure must still trigger the OUTER's `finally`
        -- so the outer's snapshot ("middle") is restored, not the
        -- initial ("outer"). A regression where the outer block's
        -- `finally` was wired to only the success path of the
        -- body would observe "outer" at the after-middle read.
        let
          program
            :: RIO ()
                 ()
                 { atOuter :: String, atAfterMiddle :: String }
          program = do
            l <- newLocal "outer"
            atAfterMiddle <- locally l "middle" do
              _ <- catchTag (Proxy :: Proxy "boom") (\_ -> pure unit)
                ( locally l "inner" do
                    fail (Proxy :: Proxy "boom") unit
                )
              get l
            atOuter <- get l
            pure { atOuter, atAfterMiddle }
        result <- runRIO program
        result `shouldEqual` Right
          { atOuter: "outer", atAfterMiddle: "middle" }

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
