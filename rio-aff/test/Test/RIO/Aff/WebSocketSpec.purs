-- | Contract tests for `RIO.Aff.WebSocket`. The `RIO.Aff.Test.WebSocket`
-- | recording helper has its own spec (`Test.RIO.Aff.Test.WebSocketSpec`)
-- | that already exercises connect / send / receive / close as a
-- | flow. This file pins the surface this module owns directly:
-- | the `Message` Eq / Ord / Show instances and the `mockWebSocket`
-- | constructor.
module Test.RIO.Aff.WebSocketSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.WebSocket (Message(..), mockWebSocket)

spec :: Spec Unit
spec = describe "RIO.Aff.WebSocket" do
  describe "Message Eq" do
    it "TextMessage equals itself with the same payload" do
      TextMessage "hi" `shouldEqual` TextMessage "hi"

    it "BinaryMessage equals itself with the same payload" do
      BinaryMessage "raw" `shouldEqual` BinaryMessage "raw"

    it "different payloads are unequal at the same constructor" do
      (TextMessage "a" == TextMessage "b") `shouldEqual` false

    it "Text and Binary with the same payload are unequal" do
      (TextMessage "x" == BinaryMessage "x") `shouldEqual` false

  describe "Message Ord" do
    it "TextMessage sorts before BinaryMessage at the constructor level" do
      compare (TextMessage "a") (BinaryMessage "a") `shouldEqual` LT

    it "within TextMessage, ordering follows the payload" do
      compare (TextMessage "a") (TextMessage "b") `shouldEqual` LT

    it "within BinaryMessage, ordering follows the payload" do
      compare (BinaryMessage "z") (BinaryMessage "a") `shouldEqual` GT

    it "equal messages compare EQ" do
      compare (TextMessage "k") (TextMessage "k") `shouldEqual` EQ

  describe "Message Show" do
    it "TextMessage shows with the constructor name and quoted payload" do
      show (TextMessage "hi") `shouldEqual` "(TextMessage \"hi\")"

    it "BinaryMessage shows with the constructor name and quoted payload" do
      show (BinaryMessage "raw") `shouldEqual` "(BinaryMessage \"raw\")"

  describe "mockWebSocket" do
    it "wires the supplied connect function into the service record" do
      let
        stubConn =
          { send: \_ -> pure unit
          , receive: pure Nothing
          , close: pure unit
          }
        ws = mockWebSocket \url ->
          if url == "ws://test" then pure stubConn
          else pure stubConn { close = pure unit }
      conn <- liftAff (ws.connect "ws://test")
      m <- liftAff conn.receive
      m `shouldEqual` (Nothing :: Maybe Message)
