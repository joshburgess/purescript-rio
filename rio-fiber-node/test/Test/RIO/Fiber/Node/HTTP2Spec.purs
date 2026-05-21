module Test.RIO.Fiber.Node.HTTP2Spec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on, once) as NE
import Node.FS.Aff (readFile) as FSAff
import Node.Http2.Client (connect') as H2Client
import Node.Http2.Server (createSecureServer, streamH) as H2Srv
import Node.Http2.Session (request) as H2Sess
import Node.Http2.Stream (respond, toDuplex) as H2Stream
import Node.Net.Server (addressTcp, close, listenTcp, listeningH) as NSrv
import Node.Stream (end, writeString) as NStream
import Node.Stream.Aff (readableToStringUtf8) as NStreamAff
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Fiber.Node.HTTP2
  ( cancel
  , endHeaders
  , endStream
  , frameData
  , frameSettings
  , isEnabled
  , noError
  , protocolError
  )
import RIO.Fiber.Node.HTTP2.Headers as H
import RIO.Fiber.Node.HTTP2.Server (toNetServer) as H2Srv
import RIO.Fiber.Node.HTTP2.Settings as S

spec :: Spec Unit
spec = describe "RIO.Fiber.Node.HTTP2" do
  describe "Headers (pure helpers)" do
    it "mkHeadersI exposes :method / :path / :scheme / :authority" do
      let
        h = H.mkHeadersI
          { ":method": "GET"
          , ":path": "/hello"
          , ":scheme": "https"
          , ":authority": "example.com"
          }
      H.method h `shouldEqual` Just "GET"
      H.path h `shouldEqual` Just "/hello"
      H.scheme h `shouldEqual` Just "https"
      H.authority h `shouldEqual` Just "example.com"

    it "lookup returns Nothing for missing headers" do
      let h = H.mkHeadersI { ":method": "GET" }
      H.lookup ":missing" h `shouldEqual` Nothing

    it "mkHeaders combines insensitive and sensitive headers" do
      let
        h = H.mkHeaders
          { ":method": "POST" }
          { "secret-token": "deadbeef" }
      H.method h `shouldEqual` Just "POST"
      H.lookup "secret-token" h `shouldEqual` Just "deadbeef"

  describe "ErrorCode / FrameType newtypes" do
    it "ErrorCode values are distinct" do
      (unwrap noError /= unwrap protocolError) `shouldEqual` true
      (unwrap noError /= unwrap cancel) `shouldEqual` true

    it "FrameType values are distinct" do
      (unwrap frameData /= unwrap frameSettings) `shouldEqual` true

  describe "Flags" do
    it "enabled flags are recognised by isEnabled" do
      isEnabled endStream endStream `shouldEqual` true
      isEnabled endHeaders endHeaders `shouldEqual` true
      isEnabled endStream endHeaders `shouldEqual` false

  describe "Settings" do
    it "defaultSettings is a complete record" do
      S.defaultSettings.enablePush `shouldEqual` true
      S.defaultSettings.enableConnectProtocol `shouldEqual` false

  describe "Secure server + client round-trip" do
    it "a streamH responder serves a body the client reads back" do
      body <- roundTrip
      body `shouldEqual` "ok-body"

-- | Exercise the createSecureServer / streamH / respond / connect'
-- | / Session.request / Stream.toDuplex surface end-to-end. The
-- | server speaks raw-stream mode (`streamH`), writes a body, and
-- | ends the stream; the client opens a request, reads the
-- | response body, and tears the connection down.
roundTrip :: Aff String
roundTrip = do
  certBuf <- FSAff.readFile "rio-fiber-node/test/fixtures/http2-localhost-cert.pem"
  keyBuf <- FSAff.readFile "rio-fiber-node/test/fixtures/http2-localhost-key.pem"

  srv <- liftEffect $ H2Srv.createSecureServer
    { cert: [ certBuf ], key: [ keyBuf ] }

  _ <- liftEffect $ srv # NE.on H2Srv.streamH \stream _hdrs _flags _origins -> do
    H2Stream.respond stream
      (H.mkHeadersI { ":status": "200" })
      { endStream: false, waitForTrailers: false }
    let dup = H2Stream.toDuplex stream
    _ <- NStream.writeString dup UTF8 "ok-body"
    NStream.end dup

  let net = H2Srv.toNetServer srv

  makeAff \done -> do
    remove <- net # NE.once NSrv.listeningH (done (Right unit))
    NSrv.listenTcp net { port: 0, host: "127.0.0.1" }
    pure (effectCanceler remove)

  mAddr <- liftEffect (NSrv.addressTcp net)
  let
    port = case mAddr of
      Just a -> a.port
      Nothing -> 0
    authority = "https://127.0.0.1:" <> show port

  session <- liftEffect $ H2Client.connect' authority
    { rejectUnauthorized: false }
  stream <- liftEffect $ H2Sess.request session
    ( H.mkHeadersI
        { ":method": "GET"
        , ":path": "/"
        , ":scheme": "https"
        , ":authority": "127.0.0.1:" <> show port
        }
    )

  out <- NStreamAff.readableToStringUtf8 (H2Stream.toDuplex stream)
  liftEffect (NSrv.close net)
  pure out
