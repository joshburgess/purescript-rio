module Test.RIO.Aff.FiberSetSpec (spec) where

import Prelude

import Data.Array as Array
import Data.Traversable (traverse_)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, runRIO')
import RIO.Aff.FiberSet as FS
import RIO.Aff.Resource (ensuring, scoped)

spec :: Spec Unit
spec = describe "RIO.Aff.FiberSet" do
  it "tracks the size of running fibers" do
    let
      program :: RIO () () { mid :: Int, final :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        set <- FS.make scope
        traverse_
          (\_ -> FS.run set (liftAff (delay (Milliseconds 100.0))))
          (Array.range 1 5)
        liftAff (delay (Milliseconds 10.0))
        mid <- FS.size set
        FS.awaitEmpty set
        final <- FS.size set
        pure { mid, final }
    result <- runRIO' program
    result `shouldEqual` { mid: 5, final: 0 }

  it "removes fibers from the set when they finish" do
    let
      program :: RIO () () Int
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        set <- FS.make scope
        _ <- FS.run set (pure 1)
        _ <- FS.run set (pure 2)
        FS.awaitEmpty set
        FS.size set
    result <- runRIO' program
    result `shouldEqual` 0

  it "awaitEmpty resumes immediately when the set is already empty" do
    let
      program :: RIO () () Int
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        set <- FS.make scope
        FS.awaitEmpty set
        FS.size set
    result <- runRIO' program
    result `shouldEqual` 0

  it "scope close interrupts every fiber" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: forall r e. String -> RIO r e Unit
      record s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      program :: RIO () () Unit
      program = do
        scoped do
          scope <- ask (Proxy :: Proxy "scope")
          set <- FS.make scope
          traverse_
            ( \n -> FS.run set
                ( ensuring (liftAff (delay (Milliseconds 200.0)))
                    (record ("done-" <> show n))
                )
            )
            (Array.range 1 3)
          liftAff (delay (Milliseconds 10.0))
        -- Wait one tick for the interrupt finalizers to run.
        liftAff (delay (Milliseconds 30.0))
    _ <- runRIO' program
    events <- liftEffect (Ref.read log)
    Array.length events `shouldEqual` 3

  it "interruptAll interrupts every fiber and reports the count" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: forall r e. String -> RIO r e Unit
      record s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      program :: RIO () () { n :: Int }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        set <- FS.make scope
        traverse_
          ( \k -> FS.run set
              ( ensuring (liftAff (delay (Milliseconds 200.0)))
                  (record ("done-" <> show k))
              )
          )
          (Array.range 1 4)
        liftAff (delay (Milliseconds 5.0))
        n <- FS.interruptAll set
        FS.awaitEmpty set
        pure { n }
    result <- runRIO' program
    result.n `shouldEqual` 4
    events <- liftEffect (Ref.read log)
    Array.length events `shouldEqual` 4
