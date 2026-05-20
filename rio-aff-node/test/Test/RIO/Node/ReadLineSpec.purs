module Test.RIO.Aff.Node.ReadLineSpec (spec) where

import Prelude

import Data.Options ((:=))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, forkAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Node.Encoding (Encoding(..))
import Node.Stream (newPassThrough, writeString, end)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Node.EventEmitter as EE
import RIO.Aff.Node.ReadLine
  ( closeH
  , lineH
  , output
  , terminal
  ) as Exports
import RIO.Aff.Node.ReadLine as RL

runRL :: forall a. RIO () () a -> Aff a
runRL = runRIO'

spec :: Spec Unit
spec = describe "RIO.Aff.Node.ReadLine" do
  it "lineH fires once per newline-terminated line on the input stream" do
    received <- runRL do
      input <- liftEffect newPassThrough
      output <- liftEffect newPassThrough
      iface <- RL.createInterface input
        ( Exports.output := output
            <> Exports.terminal := false
        )
      ref <- liftEffect (Ref.new [])
      _ <- EE.on_ Exports.lineH
        (\s -> Ref.modify_ (_ <> [ s ]) ref)
        iface
      liftEffect do
        _ <- writeString input UTF8 "alpha\nbeta\ngamma\n"
        end input
      _ <- RL.blockUntilClosed iface
      liftEffect (Ref.read ref)
    received `shouldEqual` [ "alpha", "beta", "gamma" ]

  it "countLines returns the number of lines before close" do
    n <- runRL do
      input <- liftEffect newPassThrough
      output <- liftEffect newPassThrough
      iface <- RL.createInterface input
        ( Exports.output := output
            <> Exports.terminal := false
        )
      -- countLines is `makeAff`-based and only registers its listeners
      -- when it starts; schedule the writes on a separate fiber so they
      -- happen after countLines has attached.
      _ <- liftAff $ forkAff do
        delay (Milliseconds 0.0)
        liftEffect do
          _ <- writeString input UTF8 "one\ntwo\nthree\nfour\n"
          end input
      RL.countLines iface
    n `shouldEqual` 4

  it "setPrompt / getPrompt round-trip" do
    p <- runRL do
      input <- liftEffect newPassThrough
      output <- liftEffect newPassThrough
      iface <- RL.createInterface input
        ( Exports.output := output
            <> Exports.terminal := false
        )
      RL.setPrompt "rio> " iface
      pStr <- RL.getPrompt iface
      RL.close iface
      pure pStr
    p `shouldEqual` "rio> "

  it "close emits the close event exactly once" do
    out <- runRL do
      input <- liftEffect newPassThrough
      output <- liftEffect newPassThrough
      iface <- RL.createInterface input
        ( Exports.output := output
            <> Exports.terminal := false
        )
      ref <- liftEffect (Ref.new 0)
      _ <- EE.on_ Exports.closeH
        (Ref.modify_ (_ + 1) ref)
        iface
      liftEffect (end input)
      _ <- RL.blockUntilClosed iface
      liftEffect (Ref.read ref)
    out `shouldEqual` 1
