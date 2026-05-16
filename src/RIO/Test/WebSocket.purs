-- | A recording `WebSocket` for tests.
-- |
-- | `newRecordingWebSocket` takes a script of inbound messages
-- | and returns:
-- |
-- |   * `webSocket`: a `WebSocket` service to provide to the
-- |     program under test.
-- |   * `snapshot`: an `Effect` that returns every URL the
-- |     program connected to and every message it sent, in
-- |     order.
-- |
-- | Each connection produced by `connect` shares the same
-- | scripted inbound stream: every `receive` consumes the next
-- | message off the head, returning `Nothing` once the script
-- | is exhausted or `close` has been called. Sent messages from
-- | all connections are merged into a single ordered tape on
-- | the snapshot so tests can assert on the global send order
-- | without tracking per-connection cursors.
-- |
-- | ```purescript
-- | itRIO "echoes one frame" do
-- |   rec <- liftAff (newRecordingWebSocket
-- |     [ TextMessage "hello" ])
-- |   conn <- liftAff (rec.webSocket.connect "ws://test")
-- |   m <- liftAff conn.receive
-- |   case m of
-- |     Just msg -> liftAff (conn.send msg)
-- |     Nothing -> pure unit
-- |   snap <- liftEffect rec.snapshot
-- |   snap.connects `shouldEqual` [ "ws://test" ]
-- |   snap.sent `shouldEqual` [ Tuple "ws://test" (TextMessage "hello") ]
-- | ```
module RIO.Test.WebSocket
  ( RecordingWebSocket
  , RecordingSnapshot
  , newRecordingWebSocket
  ) where

import Prelude

import Data.Array (snoc, uncons) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref

import RIO.WebSocket (Message, WebSocket, WebSocketConnection, mockWebSocket)

-- | The recording client paired with a snapshot reader.
type RecordingWebSocket =
  { webSocket :: WebSocket
  , snapshot :: Effect RecordingSnapshot
  }

-- | The shape returned by `snapshot`:
-- |
-- |   * `connects`: every URL passed to `connect`, in order.
-- |   * `sent`: every (url, message) pair sent across all
-- |     connections, in the order the calls happened.
-- |   * `closes`: how many `close` calls were observed across
-- |     all connections.
type RecordingSnapshot =
  { connects :: Array String
  , sent :: Array (Tuple String Message)
  , closes :: Int
  }

-- | Allocate a fresh recording WebSocket. The supplied array
-- | is the inbound script consumed by `receive` calls across
-- | all connections from the same recorder.
newRecordingWebSocket
  :: Array Message
  -> Aff RecordingWebSocket
newRecordingWebSocket initialInbox = liftEffect do
  inboxRef <- Ref.new initialInbox
  connectsRef <- Ref.new ([] :: Array String)
  sentRef <- Ref.new ([] :: Array (Tuple String Message))
  closesRef <- Ref.new 0
  let
    snapshot :: Effect RecordingSnapshot
    snapshot = do
      connects <- Ref.read connectsRef
      sent <- Ref.read sentRef
      closes <- Ref.read closesRef
      pure { connects, sent, closes }

    connect :: String -> Aff WebSocketConnection
    connect url = liftEffect do
      Ref.modify_ (\xs -> Array.snoc xs url) connectsRef
      closedRef <- Ref.new false
      let
        send :: Message -> Aff Unit
        send msg = liftEffect
          (Ref.modify_ (\xs -> Array.snoc xs (Tuple url msg)) sentRef)

        receive :: Aff (Maybe Message)
        receive = liftEffect do
          closed <- Ref.read closedRef
          if closed then pure Nothing
          else do
            inbox <- Ref.read inboxRef
            case Array.uncons inbox of
              Nothing -> pure Nothing
              Just { head, tail } -> do
                Ref.write tail inboxRef
                pure (Just head)

        close :: Aff Unit
        close = liftEffect do
          alreadyClosed <- Ref.read closedRef
          when (not alreadyClosed) do
            Ref.write true closedRef
            Ref.modify_ (_ + 1) closesRef
      pure { send, receive, close }

  pure
    { webSocket: mockWebSocket connect
    , snapshot
    }
