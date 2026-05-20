module Test.RIO.Fiber.HTTPurple.RequestSpec (spec) where

import Prelude

import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

import HTTPurple (Method(..), Request, RequestHeaders)
import HTTPurple.Headers (mkRequestHeaders)

import RIO.Fiber.HTTPurple.Request
  ( defaultRequestIdHeader
  , mkRequestContext
  , newRequestCounter
  )

mockRequest :: Method -> String -> RequestHeaders -> Request Unit
mockRequest method url headers = unsafeCoerce
  { method
  , url
  , headers
  }

spec :: Spec Unit
spec = describe "RIO.Fiber.HTTPurple.Request" do
  describe "defaultRequestIdHeader" do
    it "is 'X-Request-Id'" do
      defaultRequestIdHeader `shouldEqual` "X-Request-Id"

  describe "mkRequestContext" do
    it "honours an inbound request id header verbatim" do
      counter <- liftEffect newRequestCounter
      let
        hdrs = mkRequestHeaders [ Tuple "X-Request-Id" "abc-123" ]
        req = mockRequest Get "/things" hdrs
      ctx <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } req)
      ctx.requestId `shouldEqual` "abc-123"
      ctx.method `shouldEqual` Get
      ctx.path `shouldEqual` "/things"

    it "looks up the configured header name case-insensitively" do
      counter <- liftEffect newRequestCounter
      let
        hdrs = mkRequestHeaders [ Tuple "x-request-id" "lower-id" ]
        req = mockRequest Post "/items" hdrs
      ctx <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } req)
      ctx.requestId `shouldEqual` "lower-id"

    it "honours a custom header name when configured" do
      counter <- liftEffect newRequestCounter
      let
        hdrs = mkRequestHeaders [ Tuple "X-Trace-Id" "trace-9" ]
        req = mockRequest Get "/x" hdrs
      ctx <- liftEffect
        (mkRequestContext { headerName: "X-Trace-Id", counter } req)
      ctx.requestId `shouldEqual` "trace-9"

    it "falls back to a monotonic 'req-N' id when the header is absent" do
      counter <- liftEffect newRequestCounter
      let req = mockRequest Get "/no-header" (mkRequestHeaders [])
      ctx <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } req)
      ctx.requestId `shouldEqual` "req-1"

    it "increments the counter across successive header-less requests" do
      counter <- liftEffect newRequestCounter
      let req = mockRequest Get "/no-header" (mkRequestHeaders [])
      ctx1 <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } req)
      ctx2 <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } req)
      ctx3 <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } req)
      ctx1.requestId `shouldEqual` "req-1"
      ctx2.requestId `shouldEqual` "req-2"
      ctx3.requestId `shouldEqual` "req-3"

    it "does not advance the counter when the header is present" do
      counter <- liftEffect newRequestCounter
      let
        present = mockRequest Get "/with"
          (mkRequestHeaders [ Tuple "X-Request-Id" "supplied" ])
        absent = mockRequest Get "/without" (mkRequestHeaders [])
      _ <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } present)
      _ <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } present)
      ctx <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } absent)
      ctx.requestId `shouldEqual` "req-1"
      finalCount <- liftAff (liftEffect (Ref.read counter))
      finalCount `shouldEqual` 1

    it "captures the method and url from the request" do
      counter <- liftEffect newRequestCounter
      let req = mockRequest Delete "/things/42" (mkRequestHeaders [])
      ctx <- liftEffect
        (mkRequestContext { headerName: defaultRequestIdHeader, counter } req)
      ctx.method `shouldEqual` Delete
      ctx.path `shouldEqual` "/things/42"
