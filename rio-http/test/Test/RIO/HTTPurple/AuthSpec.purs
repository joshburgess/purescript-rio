module Test.RIO.HTTPurple.AuthSpec (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Data.Variant as Variant
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

import HTTPurple (RequestHeaders)
import HTTPurple.Headers (mkRequestHeaders)

import RIO.Core (RIO, runRIO)
import RIO.HTTPurple.Auth (bearerAuthConfig, requireAuth)

type Err = (unauthorized :: String)

unauthorizedTag :: Proxy "unauthorized"
unauthorizedTag = Proxy

runAuth
  :: RequestHeaders
  -> Aff (Either (Variant Err) Unit)
runAuth hdrs = runRIO
  ( requireAuth (bearerAuthConfig "secret") hdrs unauthorizedTag "denied"
      :: RIO () Err Unit
  )

spec :: Spec Unit
spec = describe "RIO.HTTPurple.Auth" do
  describe "bearerAuthConfig" do
    it "prefixes the token with 'Bearer '" do
      (bearerAuthConfig "abc").expected `shouldEqual` "Bearer abc"

  describe "requireAuth" do
    it "succeeds when the Authorization header matches exactly" do
      let hdrs = mkRequestHeaders [ Tuple "Authorization" "Bearer secret" ]
      result <- runAuth hdrs
      case result of
        Right _ -> pure unit
        Left _ -> fail "expected requireAuth to succeed on matching header"

    it "treats Authorization header lookup case-insensitively" do
      let hdrs = mkRequestHeaders [ Tuple "authorization" "Bearer secret" ]
      result <- runAuth hdrs
      case result of
        Right _ -> pure unit
        Left _ -> fail "expected requireAuth to honour case-insensitive header lookup"

    it "fails with the supplied tag when the header is missing" do
      let hdrs = mkRequestHeaders []
      result <- runAuth hdrs
      case result of
        Left v ->
          (Variant.case_ # Variant.on unauthorizedTag identity $ v)
            `shouldEqual` "denied"
        Right _ -> fail "expected requireAuth to fail when header is absent"

    it "fails when the Authorization header does not match" do
      let hdrs = mkRequestHeaders [ Tuple "Authorization" "Bearer wrong" ]
      result <- runAuth hdrs
      case result of
        Left v ->
          (Variant.case_ # Variant.on unauthorizedTag identity $ v)
            `shouldEqual` "denied"
        Right _ -> fail "expected requireAuth to fail on mismatched header"

    it "fails when the value matches the token but omits the 'Bearer ' prefix" do
      let hdrs = mkRequestHeaders [ Tuple "Authorization" "secret" ]
      result <- runAuth hdrs
      case result of
        Left _ -> pure unit
        Right _ -> fail "expected requireAuth to reject token without scheme"
