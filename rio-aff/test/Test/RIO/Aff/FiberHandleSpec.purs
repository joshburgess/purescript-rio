module Test.RIO.Aff.FiberHandleSpec (spec) where

import Prelude hiding (join)

import Data.Maybe (isJust, isNothing)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask, join, runRIO')
import RIO.Aff.FiberHandle as FH
import RIO.Aff.Resource (ensuring, scoped)

spec :: Spec Unit
spec = describe "RIO.Aff.FiberHandle" do
  it "stores and exposes the running fiber" do
    let
      program :: RIO () () Boolean
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        h <- FH.make scope
        _ <- FH.run h (liftAff (delay (Milliseconds 50.0)) *> pure 42)
        mf <- FH.get h
        pure (isJust mf)
    result <- runRIO' program
    result `shouldEqual` true

  it "second run interrupts the first" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: forall r e. String -> RIO r e Unit
      record s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      program :: RIO () () Unit
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        h <- FH.make scope
        _ <- FH.run h
          ( ensuring (liftAff (delay (Milliseconds 200.0)))
              (record "first-out")
          )
        liftAff (delay (Milliseconds 10.0))
        f2 <- FH.run h
          (ensuring (record "second-in") (record "second-out"))
        _ <- join f2
        pure unit
    _ <- runRIO' program
    events <- liftEffect (Ref.read log)
    events `shouldEqual` [ "first-out", "second-in", "second-out" ]

  it "slot auto-clears after the fiber finishes" do
    let
      program :: RIO () () Boolean
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        h <- FH.make scope
        f <- FH.run h (pure 1)
        _ <- join f
        liftAff (delay (Milliseconds 5.0))
        mf <- FH.get h
        pure (isNothing mf)
    result <- runRIO' program
    result `shouldEqual` true

  it "scope close interrupts the current occupant" do
    log <- liftEffect (Ref.new ([] :: Array String))
    let
      record :: forall r e. String -> RIO r e Unit
      record s = liftEffect (Ref.modify_ (\xs -> xs <> [ s ]) log)

      program :: RIO () () Unit
      program = do
        scoped do
          scope <- ask (Proxy :: Proxy "scope")
          h <- FH.make scope
          _ <- FH.run h
            ( ensuring (liftAff (delay (Milliseconds 200.0)))
                (record "interrupted")
            )
          liftAff (delay (Milliseconds 10.0))
        -- scope is closed here; the occupant has been interrupted
        -- but its finalizer needs a tick to run.
        liftAff (delay (Milliseconds 20.0))
    _ <- runRIO' program
    events <- liftEffect (Ref.read log)
    events `shouldEqual` [ "interrupted" ]

  it "clear interrupts the current occupant and reports true" do
    let
      program :: RIO () () { didInterrupt :: Boolean, after :: Boolean }
      program = scoped do
        scope <- ask (Proxy :: Proxy "scope")
        h <- FH.make scope
        _ <- FH.run h (liftAff (delay (Milliseconds 500.0)))
        liftAff (delay (Milliseconds 5.0))
        didInterrupt <- FH.clear h
        again <- FH.clear h
        pure { didInterrupt, after: again }
    result <- runRIO' program
    result `shouldEqual` { didInterrupt: true, after: false }
