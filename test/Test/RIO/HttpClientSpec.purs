module Test.RIO.HttpClientSpec (spec) where

import Prelude

import Data.Argonaut.Core as Json
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant as Variant
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
import Type.Proxy (Proxy(..))

import RIO.Core (RIO, provideAll, runRIO)
import RIO.HttpClient
  ( HttpClient
  , HttpError(..)
  , Method(..)
  , Request
  , RequestBody(..)
  , Response
  , StatusClass(..)
  )
import RIO.HttpClient as Http
import RIO.Schema as Schema

-- A handler that records every request it sees and returns a
-- configurable response (or error). Used to assert on the
-- outgoing request shape.
newtype Handler = Handler
  { calls :: Ref.Ref (Array Request)
  , next :: Ref.Ref (Either HttpError Response)
  }

mkHandler :: Either HttpError Response -> Aff Handler
mkHandler resp = liftEffect do
  calls <- Ref.new []
  next <- Ref.new resp
  pure (Handler { calls, next })

mkClient :: Handler -> HttpClient
mkClient (Handler h) = Http.mockHttpClient \req -> do
  liftEffect (Ref.modify_ (\xs -> xs <> [ req ]) h.calls)
  liftEffect (Ref.read h.next)

okResponse :: String -> Response
okResponse body =
  { status: 200, statusText: "OK", headers: [], body }

errorResponse :: Int -> Response
errorResponse code =
  { status: code
  , statusText: "Boom"
  , headers: []
  , body: ""
  }

run
  :: forall a
   . HttpClient
  -> RIO (httpClient :: HttpClient) (httpError :: HttpError) a
  -> Aff (Either HttpError a)
run client p = do
  res <- runRIO (provideAll { httpClient: client } p)
  pure case res of
    Right a -> Right a
    Left v -> Left (Variant.case_ # Variant.on (Proxy :: Proxy "httpError") identity $ v)

spec :: Spec Unit
spec = describe "RIO.HttpClient" do
  describe "request builders" do
    it "newRequest defaults to GET with no body and no headers" do
      let r = Http.newRequest "https://example.com"
      r.method `shouldEqual` GET
      r.url `shouldEqual` "https://example.com"
      r.headers `shouldEqual` []
      r.timeout `shouldEqual` Nothing
      case r.body of
        NoBody -> pure unit
        _ -> fail "expected NoBody"

    it "withHeader appends headers in order" do
      let
        r = Http.newRequest "u"
          # Http.withHeader "X-A" "1"
          # Http.withHeader "X-B" "2"
      r.headers `shouldEqual`
        [ Tuple "X-A" "1", Tuple "X-B" "2" ]

    it "withJsonBody sets the body and adds Content-Type" do
      let
        r = Http.newRequest "u"
          # Http.withJsonBody (Json.fromString "x")
      case r.body of
        JsonBody j -> Json.stringify j `shouldEqual` "\"x\""
        _ -> fail "expected JsonBody"
      r.headers `shouldEqual`
        [ Tuple "Content-Type" "application/json" ]

    it "withJsonBody preserves an existing Content-Type (case-insensitive)" do
      let
        r = Http.newRequest "u"
          # Http.withHeader "content-type" "application/vnd.api+json"
          # Http.withJsonBody Json.jsonEmptyObject
      r.headers `shouldEqual`
        [ Tuple "content-type" "application/vnd.api+json" ]

  describe "send / get / post" do
    it "send threads the Request through the wired-in HttpClient" do
      h@(Handler hRef) <- mkHandler
        (Right (okResponse "hello"))
      let client = mkClient h
      result <- run client (Http.get "https://example.com/x")
      case result of
        Right r -> r.body `shouldEqual` "hello"
        Left e -> fail ("expected Right, got: " <> show e)
      calls <- liftEffect (Ref.read hRef.calls)
      case calls of
        [ req ] -> do
          req.method `shouldEqual` GET
          req.url `shouldEqual` "https://example.com/x"
        _ -> fail "expected exactly one recorded call"

    it "post records the method and body" do
      h@(Handler hRef) <- mkHandler (Right (okResponse ""))
      let client = mkClient h
      _ <- run client (Http.post "u" (TextBody "payload"))
      calls <- liftEffect (Ref.read hRef.calls)
      case calls of
        [ req ] -> do
          req.method `shouldEqual` POST
          case req.body of
            TextBody s -> s `shouldEqual` "payload"
            _ -> fail "expected TextBody"
        _ -> fail "expected one recorded call"

    it "surfaces transport errors on the httpError row" do
      h <- mkHandler (Left (HttpTransport "boom"))
      result <- run (mkClient h) (Http.get "u")
      case result of
        Left (HttpTransport msg) -> msg `shouldEqual` "boom"
        other -> fail ("expected HttpTransport, got: " <> show other)

  describe "statusClass / isSuccess / ensureStatus" do
    it "classifies status codes into the five canonical bands" do
      Http.statusClass 100 `shouldEqual` Informational
      Http.statusClass 200 `shouldEqual` Success
      Http.statusClass 301 `shouldEqual` Redirection
      Http.statusClass 404 `shouldEqual` ClientError
      Http.statusClass 500 `shouldEqual` ServerError
      Http.statusClass 999 `shouldEqual` Unknown

    it "isSuccess only fires for 2xx" do
      Http.isSuccess (okResponse "") `shouldEqual` true
      Http.isSuccess (errorResponse 500) `shouldEqual` false

    it "ensureStatus fails with HttpUnexpectedStatus for non-2xx" do
      h <- mkHandler (Right (errorResponse 500))
      let client = mkClient h
      result <- run client do
        resp <- Http.get "u"
        Http.ensureStatus resp
      case result of
        Left (HttpUnexpectedStatus r) -> r.status `shouldEqual` 500
        other -> fail ("expected HttpUnexpectedStatus, got: " <> show other)

    it "ensureStatus is a no-op on a 2xx response" do
      h <- mkHandler (Right (okResponse "ok"))
      result <- run (mkClient h) do
        resp <- Http.get "u"
        Http.ensureStatus resp
        pure resp.body
      result `shouldEqual` Right "ok"

  describe "decodeBody" do
    it "decodes a JSON body through a Schema" do
      let body = "{\"name\":\"ada\"}"
      h <- mkHandler (Right (okResponse body))
      let
        nameSchema =
          Schema.recordOf
            (Schema.field "name" identity Schema.string)
      result <- run (mkClient h) do
        resp <- Http.get "u"
        Http.decodeBody nameSchema resp
      result `shouldEqual` Right "ada"

    it "surfaces decode failures as HttpDecode" do
      h <- mkHandler (Right (okResponse "{not json"))
      let
        intSchema =
          Schema.recordOf
            (Schema.field "x" identity Schema.int)
      result <- run (mkClient h) do
        resp <- Http.get "u"
        Http.decodeBody intSchema resp
      result `shouldSatisfy` case _ of
        Left (HttpDecode _) -> true
        _ -> false
