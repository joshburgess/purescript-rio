module Test.RIO.Aff.Test.HttpServerSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.HttpClient (Method(..), RequestBody(..))
import RIO.Aff.HttpServer
  ( Handler
  , ResponseBody(..)
  , ServerRequest
  , ServerResponse
  )
import RIO.Aff.Test.HttpServer (newRecordingHttpServer)

mkRequest :: Method -> String -> ServerRequest
mkRequest method path =
  { method
  , path
  , query: []
  , headers: []
  , body: NoBody
  , params: []
  }

okResponse :: String -> ServerResponse
okResponse body =
  { status: 200
  , headers: []
  , body: TextResponseBody body
  }

echoHandler :: Handler
echoHandler req = pure (okResponse req.path)

spec :: Spec Unit
spec = describe "RIO.Aff.Test.HttpServer" do
  it "dispatch runs the handler and returns its response" do
    rec <- newRecordingHttpServer
    resp <- rec.dispatch echoHandler (mkRequest GET "/hello")
    resp.status `shouldEqual` 200
    resp.body `shouldEqual` TextResponseBody "/hello"

  it "captures every (request, response) pair in dispatch order" do
    rec <- newRecordingHttpServer
    _ <- rec.dispatch echoHandler (mkRequest GET "/a")
    _ <- rec.dispatch echoHandler (mkRequest POST "/b")
    _ <- rec.dispatch echoHandler (mkRequest DELETE "/c")
    calls <- liftEffect rec.snapshot
    Array.length calls `shouldEqual` 3
    case map (\(Tuple req _) -> req.path) calls of
      [ "/a", "/b", "/c" ] -> pure unit
      _ -> 1 `shouldEqual` 0

  it "starts with an empty snapshot" do
    rec <- newRecordingHttpServer
    calls <- liftEffect rec.snapshot
    Array.length calls `shouldEqual` 0

  it "httpServer.listen and httpServer.shutdown are no-ops" do
    rec <- newRecordingHttpServer
    rec.httpServer.listen { host: "0.0.0.0", port: 9999 }
    rec.httpServer.shutdown
    -- snapshot still empty because listen/shutdown don't dispatch
    calls <- liftEffect rec.snapshot
    Array.length calls `shouldEqual` 0

  it "snapshot records the response the handler actually returned" do
    rec <- newRecordingHttpServer
    let
      failingHandler :: Handler
      failingHandler _ = pure
        { status: 500, headers: [], body: TextResponseBody "boom" }
    _ <- rec.dispatch failingHandler (mkRequest GET "/")
    calls <- liftEffect rec.snapshot
    case calls of
      [ Tuple _ resp ] -> do
        resp.status `shouldEqual` 500
        resp.body `shouldEqual` TextResponseBody "boom"
      _ -> 1 `shouldEqual` 0
