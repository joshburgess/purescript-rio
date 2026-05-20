module Test.RIO.Aff.RuntimeSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect.Ref as Ref
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, fail)
import RIO.Aff.Runtime as Runtime

type Counter = { increment :: Int -> Int -> Int }

incrementer :: Counter
incrementer = { increment: (+) }

spec :: Spec Unit
spec = describe "RIO.Aff.Runtime" do
  it "run executes a program against the bundled env" do
    let
      runtime = Runtime.make { counter: incrementer }

      program :: RIO (counter :: Counter) () Int
      program = do
        c <- ask (Proxy :: Proxy "counter")
        pure (c.increment 40 2)

    result <- Runtime.run runtime program
    result `shouldEqual` Right 42

  it "runOrThrow returns the success value directly when the row is empty" do
    let
      runtime = Runtime.make { counter: incrementer }

      program :: RIO (counter :: Counter) () String
      program = pure "hello"

    result <- Runtime.runOrThrow runtime program
    result `shouldEqual` "hello"

  it "run reuses the same env across multiple programs" do
    -- The runtime carries a Ref-backed counter; both programs
    -- share it. After both runs the counter must show both
    -- mutations.
    ref <- liftEffect (Ref.new 0)
    let
      runtime = Runtime.make { ref }

      bumpBy :: Int -> RIO (ref :: Ref.Ref Int) () Unit
      bumpBy n = do
        r <- ask (Proxy :: Proxy "ref")
        liftEffect (Ref.modify_ (_ + n) r)

    _ <- Runtime.run runtime (bumpBy 3)
    _ <- Runtime.run runtime (bumpBy 4)
    n <- liftEffect (Ref.read ref)
    n `shouldEqual` 7

  it "env returns the underlying record" do
    let
      runtime = Runtime.make { counter: incrementer }
      r = Runtime.env runtime
    r.counter.increment 1 1 `shouldEqual` 2

  it "unitRuntime + run is equivalent to runRIO for the no-env case" do
    let
      program :: RIO () () Int
      program = pure 99
    result <- Runtime.run Runtime.unitRuntime program
    result `shouldEqual` Right 99

  it "run surfaces typed failures on the Left branch" do
    let
      runtime = Runtime.make { counter: incrementer }

      program :: RIO (counter :: Counter) (boom :: String) Int
      program = fail (Proxy :: Proxy "boom") "no good"

    result <- Runtime.run runtime program
    case result of
      Left _ -> pure unit
      Right _ -> 1 `shouldEqual` 0
