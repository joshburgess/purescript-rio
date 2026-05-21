-- | The WebSocket service shape.
-- |
-- | `RIO.Fiber.WebSocket` is the contract: a client-style service for
-- | opening a WebSocket connection to a URL and exchanging
-- | `Message`s with the remote peer. The wire layer (a real
-- | implementation over `node-ws` or the browser `WebSocket`
-- | API) lives outside this module; what lands here is the
-- | service record, the message ADT, and `mockWebSocket` for
-- | tests / fixtures.
-- |
-- | Connection lifetime: `connect` returns a `WebSocketConnection`
-- | with `send` / `receive` / `close` actions. `receive` blocks
-- | until the next inbound message arrives, returning `Nothing`
-- | when the peer (or `close`) closes the channel. Subsequent
-- | `receive` calls after a close keep returning `Nothing`, so
-- | drain loops terminate cleanly.
-- |
-- | The service operations live in `Aff` so existing browser /
-- | Node WebSocket bindings can be wired in directly; `fromAff`
-- | bridges any of them into the fiber runtime at the call site.
-- |
-- | ```purescript
-- | -- client-side: open a connection, echo until the peer hangs up
-- | echo :: forall r. WebSocketConnection -> RIO r () Unit
-- | echo conn = do
-- |   msg <- fromAff conn.receive
-- |   case msg of
-- |     Nothing -> pure unit
-- |     Just m -> do
-- |       fromAff (conn.send m)
-- |       echo conn
-- | ```
module RIO.Fiber.WebSocket
  ( WebSocket
  , WebSocketConnection
  , Message(..)
  , mockWebSocket
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff (Aff)

-- | The service: a way to open a connection to a `String` URL.
-- | Implementations decide how to interpret the URL (ws://...
-- | over the network, in-memory mock, etc.).
type WebSocket =
  { connect :: String -> Aff WebSocketConnection
  }

-- | A live connection. `send` posts a message to the peer;
-- | `receive` blocks for the next inbound message or `Nothing`
-- | on close; `close` ends the channel and wakes any pending
-- | `receive`.
type WebSocketConnection =
  { send :: Message -> Aff Unit
  , receive :: Aff (Maybe Message)
  , close :: Aff Unit
  }

-- | The frames a connection carries. `TextMessage` is the
-- | common case; `BinaryMessage` carries an opaque byte payload
-- | represented as a `String` for ergonomic transit (drivers
-- | may use base64 or raw bytes depending on the wire).
data Message
  = TextMessage String
  | BinaryMessage String

derive instance eqMessage :: Eq Message
derive instance ordMessage :: Ord Message

instance showMessage :: Show Message where
  show = case _ of
    TextMessage s -> "(TextMessage " <> show s <> ")"
    BinaryMessage s -> "(BinaryMessage " <> show s <> ")"

-- | Adapt an arbitrary connect function into a `WebSocket`
-- | service. Useful for tests and one-off fixtures.
mockWebSocket
  :: (String -> Aff WebSocketConnection)
  -> WebSocket
mockWebSocket f = { connect: f }
