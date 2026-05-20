module Test.RIO.Aff.HttpServerSpec (spec) where

import Prelude

import Data.Argonaut.Core as Json
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

import RIO.Aff.HttpClient (Method(..), RequestBody(..))
import RIO.Aff.HttpServer
  ( Handler
  , ResponseBody(..)
  , ServerRequest
  , ServerResponse
  )
import RIO.Aff.HttpServer as Server

emptyReq :: Method -> String -> ServerRequest
emptyReq m path =
  { method: m
  , path
  , query: []
  , headers: []
  , body: NoBody
  , params: []
  }

ok200 :: Handler
ok200 = \_ -> pure Server.ok

textHandler :: String -> Handler
textHandler t = \_ -> pure (Server.textResponse t)

echoIdHandler :: Handler
echoIdHandler = \req -> case Server.captureParam "id" req of
  Just id -> pure (Server.textResponse ("hello " <> id))
  Nothing -> pure
    (Server.status 500 (Server.textResponse "missing id"))

spec :: Spec Unit
spec = describe "RIO.Aff.HttpServer (shape-only framework)" do
  describe "response builders" do
    it "ok is status 200 with no body" do
      Server.ok.status `shouldEqual` 200
      Server.ok.body `shouldEqual` NoResponseBody

    it "textResponse sets Content-Type to text/plain" do
      let r = Server.textResponse "hi"
      r.body `shouldEqual` TextResponseBody "hi"
      r.headers `shouldEqual`
        [ Tuple "Content-Type" "text/plain; charset=utf-8" ]

    it "jsonResponse sets Content-Type to application/json" do
      let
        r = Server.jsonResponse (Json.fromString "hi")
      r.headers `shouldEqual`
        [ Tuple "Content-Type" "application/json" ]

    it "status overrides the status code" do
      (Server.status 201 Server.ok).status `shouldEqual` 201

    it "withHeader appends a header" do
      (Server.withHeader "X-Foo" "bar" Server.ok).headers
        `shouldEqual` [ Tuple "X-Foo" "bar" ]

  describe "routing" do
    it "matches a literal path on the right method" do
      let
        app = Server.router
          [ Server.route GET "/ping" (textHandler "pong") ]
      resp <- (app (emptyReq GET "/ping") :: Aff ServerResponse)
      resp.status `shouldEqual` 200
      resp.body `shouldEqual` TextResponseBody "pong"

    it "returns 404 when no route matches the path" do
      let
        app = Server.router
          [ Server.route GET "/ping" (textHandler "pong") ]
      resp <- app (emptyReq GET "/missing")
      resp.status `shouldEqual` 404

    it "returns 404 when method does not match" do
      let
        app = Server.router
          [ Server.route GET "/ping" (textHandler "pong") ]
      resp <- app (emptyReq POST "/ping")
      resp.status `shouldEqual` 404

    it "captures :name params and exposes them via captureParam" do
      let
        app = Server.router
          [ Server.route GET "/users/:id" echoIdHandler ]
      resp <- app (emptyReq GET "/users/42")
      resp.body `shouldEqual` TextResponseBody "hello 42"

    it "the first matching route wins" do
      let
        app = Server.router
          [ Server.route GET "/a" (textHandler "first")
          , Server.route GET "/a" (textHandler "second")
          ]
      resp <- app (emptyReq GET "/a")
      resp.body `shouldEqual` TextResponseBody "first"

    it "tries multiple routes until one matches" do
      let
        app = Server.router
          [ Server.route GET "/a" (textHandler "a")
          , Server.route GET "/b" (textHandler "b")
          , Server.route GET "/c" (textHandler "c")
          ]
      resp <- app (emptyReq GET "/b")
      resp.body `shouldEqual` TextResponseBody "b"

  describe "middleware" do
    it "withMiddleware wraps a handler" do
      let
        addHeader :: String -> String -> Server.Middleware
        addHeader k v inner = \req -> do
          r <- inner req
          pure (Server.withHeader k v r)
        app = Server.withMiddleware
          (addHeader "X-Trace" "abc")
          ok200
      resp <- app (emptyReq GET "/anything")
      resp.headers `shouldEqual` [ Tuple "X-Trace" "abc" ]

    it "middleware can short-circuit by ignoring the inner handler" do
      let
        block :: Server.Middleware
        block _ = \_ -> pure
          (Server.status 401 (Server.textResponse "no"))
        app = Server.withMiddleware block ok200
      resp <- app (emptyReq GET "/anything")
      resp.status `shouldEqual` 401

  describe "mockHttpServer" do
    it "listen / shutdown are no-ops (do not throw)" do
      Server.mockHttpServer.listen { host: "::", port: 0 }
      Server.mockHttpServer.shutdown

    it "tests drive handlers directly via the handler signature" do
      let
        log = \ref req -> do
          liftEffect (Ref.modify_ (\xs -> xs <> [ req.path ]) ref)
          pure Server.ok
      ref <- liftEffect (Ref.new ([] :: Array String))
      _ <- log ref (emptyReq GET "/one")
      _ <- log ref (emptyReq GET "/two")
      observed <- liftEffect (Ref.read ref)
      observed `shouldEqual` [ "/one", "/two" ]

  describe "captureParam" do
    it "returns Nothing when the named param is missing" do
      let req = emptyReq GET "/x"
      case Server.captureParam "id" req of
        Nothing -> pure unit
        Just v -> fail ("expected Nothing, got Just " <> show v)
