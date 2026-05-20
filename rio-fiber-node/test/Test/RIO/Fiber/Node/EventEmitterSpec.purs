module Test.RIO.Fiber.Node.EventEmitterSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Effect.Uncurried (EffectFn1, EffectFn2, mkEffectFn1, runEffectFn2)
import Node.EventEmitter (EventEmitter, EventHandle(..))
import Node.EventEmitter (unsafeEmitFn) as NE
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Fiber.Aff (runAffThrow)
import RIO.Fiber.Core (RIO, liftEffect)
import RIO.Fiber.Node.EventEmitter as EE

runEE :: forall a. RIO () () a -> Aff a
runEE = runAffThrow

-- Custom event handle that delivers a single `String` payload.
pingH :: EventHandle EventEmitter (String -> Effect Unit) (EffectFn1 String Unit)
pingH = EventHandle "ping" \cb -> mkEffectFn1 cb

emit :: EventEmitter -> String -> RIO () () Unit
emit ee payload = liftEffect do
  _ <- runEffectFn2
    (NE.unsafeEmitFn ee :: EffectFn2 String String Boolean)
    "ping"
    payload
  pure unit

spec :: Spec Unit
spec = describe "RIO.Fiber.Node.EventEmitter" do
  it "on registers a listener that fires for each emission" do
    out <- runEE do
      ref <- liftEffect (Ref.new [])
      ee <- EE.new
      EE.on_ pingH (\s -> Ref.modify_ (_ <> [ s ]) ref) ee
      emit ee "a"
      emit ee "b"
      emit ee "c"
      liftEffect (Ref.read ref)
    out `shouldEqual` [ "a", "b", "c" ]

  it "the remover returned by on stops further deliveries" do
    out <- runEE do
      ref <- liftEffect (Ref.new 0)
      ee <- EE.new
      remove <- EE.on pingH (\_ -> Ref.modify_ (_ + 1) ref) ee
      emit ee "a"
      emit ee "b"
      remove
      emit ee "c"
      liftEffect (Ref.read ref)
    out `shouldEqual` 2

  it "once fires exactly once" do
    out <- runEE do
      ref <- liftEffect (Ref.new 0)
      ee <- EE.new
      EE.once_ pingH (\_ -> Ref.modify_ (_ + 1) ref) ee
      emit ee "x"
      emit ee "y"
      emit ee "z"
      liftEffect (Ref.read ref)
    out `shouldEqual` 1

  it "listenerCount tracks active subscriptions" do
    out <- runEE do
      ee <- EE.new
      before <- EE.listenerCount ee "ping"
      EE.on_ pingH (\_ -> pure unit) ee
      EE.on_ pingH (\_ -> pure unit) ee
      after <- EE.listenerCount ee "ping"
      pure { before, after }
    out.before `shouldEqual` 0
    out.after `shouldEqual` 2

  it "setMaxListeners updates the cap reported by getMaxListeners" do
    out <- runEE do
      ee <- EE.new
      EE.setMaxListeners 42 ee
      EE.getMaxListeners ee
    out `shouldEqual` 42

  it "prependListener runs before listeners added with on" do
    out <- runEE do
      ref <- liftEffect (Ref.new [])
      ee <- EE.new
      EE.on_ pingH (\_ -> Ref.modify_ (_ <> [ "tail" ]) ref) ee
      EE.prependListener_ pingH (\_ -> Ref.modify_ (_ <> [ "head" ]) ref) ee
      emit ee "ignored"
      liftEffect (Ref.read ref)
    out `shouldEqual` [ "head", "tail" ]

  it "the newListener event fires when subscriptions are added" do
    out <- runEE do
      ref <- liftEffect (Ref.new [])
      ee <- EE.new
      EE.on_ EE.newListenerH
        ( case _ of
            Right name -> Ref.modify_ (_ <> [ name ]) ref
            Left _ -> pure unit
        )
        ee
      EE.on_ pingH (\_ -> pure unit) ee
      EE.once_ pingH (\_ -> pure unit) ee
      liftEffect (Ref.read ref)
    out `shouldEqual` [ "ping", "ping" ]
