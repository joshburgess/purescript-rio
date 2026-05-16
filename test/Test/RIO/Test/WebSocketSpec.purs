module Test.RIO.Test.WebSocketSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.WebSocket (Message(..))
import RIO.Test.WebSocket (newRecordingWebSocket)

spec :: Spec Unit
spec = describe "RIO.Test.WebSocket" do
  it "captures the URL passed to connect" do
    rec <- newRecordingWebSocket []
    _ <- liftAff (rec.webSocket.connect "ws://localhost/test")
    snap <- liftEffect rec.snapshot
    snap.connects `shouldEqual` [ "ws://localhost/test" ]

  it "serves scripted inbound messages in order via receive" do
    rec <- newRecordingWebSocket
      [ TextMessage "first"
      , BinaryMessage "second"
      , TextMessage "third"
      ]
    conn <- liftAff (rec.webSocket.connect "ws://test")
    m1 <- liftAff conn.receive
    m2 <- liftAff conn.receive
    m3 <- liftAff conn.receive
    m4 <- liftAff conn.receive
    m1 `shouldEqual` Just (TextMessage "first")
    m2 `shouldEqual` Just (BinaryMessage "second")
    m3 `shouldEqual` Just (TextMessage "third")
    m4 `shouldEqual` Nothing

  it "returns Nothing on receive after close" do
    rec <- newRecordingWebSocket
      [ TextMessage "ignored" ]
    conn <- liftAff (rec.webSocket.connect "ws://test")
    liftAff conn.close
    m <- liftAff conn.receive
    m `shouldEqual` Nothing

  it "captures every send with the connection's URL" do
    rec <- newRecordingWebSocket []
    conn <- liftAff (rec.webSocket.connect "ws://target")
    liftAff (conn.send (TextMessage "a"))
    liftAff (conn.send (TextMessage "b"))
    liftAff (conn.send (BinaryMessage "c"))
    snap <- liftEffect rec.snapshot
    snap.sent `shouldEqual`
      [ Tuple "ws://target" (TextMessage "a")
      , Tuple "ws://target" (TextMessage "b")
      , Tuple "ws://target" (BinaryMessage "c")
      ]

  it "counts close calls" do
    rec <- newRecordingWebSocket []
    conn1 <- liftAff (rec.webSocket.connect "ws://a")
    conn2 <- liftAff (rec.webSocket.connect "ws://b")
    liftAff conn1.close
    liftAff conn2.close
    -- double-close on the same connection does not double-count
    liftAff conn1.close
    snap <- liftEffect rec.snapshot
    snap.closes `shouldEqual` 2

  it "multiple connections share the inbound script" do
    rec <- newRecordingWebSocket
      [ TextMessage "one"
      , TextMessage "two"
      ]
    conn1 <- liftAff (rec.webSocket.connect "ws://a")
    conn2 <- liftAff (rec.webSocket.connect "ws://b")
    m1 <- liftAff conn1.receive
    m2 <- liftAff conn2.receive
    m1 `shouldEqual` Just (TextMessage "one")
    m2 `shouldEqual` Just (TextMessage "two")

  it "starts with an empty snapshot" do
    rec <- newRecordingWebSocket []
    snap <- liftEffect rec.snapshot
    Array.length snap.connects `shouldEqual` 0
    Array.length snap.sent `shouldEqual` 0
    snap.closes `shouldEqual` 0
