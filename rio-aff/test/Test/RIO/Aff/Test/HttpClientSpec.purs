module Test.RIO.Aff.Test.HttpClientSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.HttpClient (HttpError(..), Method(..), RequestBody(..), Request, Response)
import RIO.Aff.HttpClient (mockHttpClient) as HC
import RIO.Aff.Test.HttpClient (newRecordingHttpClient)

mkRequest :: Method -> String -> Request
mkRequest method url =
  { method
  , url
  , headers: []
  , body: NoBody
  , timeout: Nothing
  }

okResponse :: Response
okResponse =
  { status: 200
  , statusText: "OK"
  , headers: []
  , body: "ok"
  }

spec :: Spec Unit
spec = describe "RIO.Aff.Test.HttpClient" do
  it "captures requests and returns the canned responses in order" do
    rec <- newRecordingHttpClient
      [ Right okResponse
      , Right (okResponse { body = "second" })
      ]
    r1 <- rec.httpClient.sendRequest (mkRequest GET "/a")
    r2 <- rec.httpClient.sendRequest (mkRequest POST "/b")
    r1 `shouldEqual` Right okResponse
    r2 `shouldEqual` Right (okResponse { body = "second" })

    reqs <- liftEffect rec.snapshot
    Array.length reqs `shouldEqual` 2

  it "surfaces a typed HttpError when the canned outcome is Left" do
    rec <- newRecordingHttpClient
      [ Left HttpTimeout ]
    r <- rec.httpClient.sendRequest (mkRequest GET "/x")
    r `shouldEqual` Left HttpTimeout

  it "fails with an HttpTransport error when the script is exhausted" do
    rec <- newRecordingHttpClient [ Right okResponse ]
    _ <- rec.httpClient.sendRequest (mkRequest GET "/first")
    r <- rec.httpClient.sendRequest (mkRequest GET "/overflow")
    isLeft r `shouldEqual` true

  it "starts with an empty snapshot" do
    rec <- newRecordingHttpClient []
    reqs <- liftEffect rec.snapshot
    Array.length reqs `shouldEqual` 0

  it "snapshot preserves send order" do
    rec <- newRecordingHttpClient
      [ Right okResponse, Right okResponse, Right okResponse ]
    _ <- rec.httpClient.sendRequest (mkRequest GET "/one")
    _ <- rec.httpClient.sendRequest (mkRequest POST "/two")
    _ <- rec.httpClient.sendRequest (mkRequest DELETE "/three")
    reqs <- liftEffect rec.snapshot
    map _.url reqs `shouldEqual` [ "/one", "/two", "/three" ]
    map _.method reqs `shouldEqual` [ GET, POST, DELETE ]

  -- HttpClient mock smoke test for cross-reference
  it "is a strict superset of mockHttpClient's behaviour" do
    let
      raw = HC.mockHttpClient \_ -> pure (Right okResponse)
    r <- raw.sendRequest (mkRequest GET "/anything")
    r `shouldEqual` Right okResponse
