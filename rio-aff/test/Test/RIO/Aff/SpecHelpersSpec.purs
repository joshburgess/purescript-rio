module Test.RIO.Aff.SpecHelpersSpec (spec) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

import RIO.Aff.Core (RIO, ask)
import RIO.Aff.Spec (itRIO, itRIO_)

type Greeter =
  { greet :: String -> Aff String
  }

greet :: forall r e. String -> RIO (greeter :: Greeter | r) e String
greet name = do
  g <- ask (Proxy :: Proxy "greeter")
  liftAff (g.greet name)

upperCaseGreeter :: Greeter
upperCaseGreeter = { greet: \n -> pure ("HELLO " <> n) }

spec :: Spec Unit
spec =
  describe "RIO.Aff.Spec (Phase 7.2)" do
    itRIO "itRIO runs a fully-handled program body" do
      ref <- liftEffect (Ref.new 0)
      liftEffect (Ref.modify_ (_ + 1) ref)
      liftEffect (Ref.modify_ (_ + 1) ref)
      n <- liftEffect (Ref.read ref)
      liftAff (n `shouldEqual` 2)

    itRIO_ "itRIO_ provides the service record before running"
      { greeter: upperCaseGreeter }
      do
        msg <- greet "world"
        liftAff (msg `shouldEqual` "HELLO world")
