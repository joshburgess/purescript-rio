module Test.RIO.Node.HTTP2Spec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Node.HTTP2
  ( cancel
  , endHeaders
  , endStream
  , frameData
  , frameSettings
  , isEnabled
  , noError
  , protocolError
  )
import RIO.Node.HTTP2.Headers as H
import RIO.Node.HTTP2.Settings as S

spec :: Spec Unit
spec = describe "RIO.Node.HTTP2" do
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
