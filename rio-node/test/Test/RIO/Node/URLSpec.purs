module Test.RIO.Node.URLSpec (spec) where

import Prelude

import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Core (RIO, runRIO')
import RIO.Node.URL as URL

runURL :: forall a. RIO () () a -> Aff a
runURL = runRIO'

spec :: Spec Unit
spec = describe "RIO.Node.URL" do
  it "new parses an absolute URL and exposes its accessors" do
    out <- runURL do
      url <- URL.new "https://user:pw@example.com:8443/path?q=1#frag"
      proto <- URL.protocol url
      hostName <- URL.hostname url
      portStr <- URL.port url
      path <- URL.pathname url
      query <- URL.search url
      fragment <- URL.hash url
      user <- URL.username url
      pwd <- URL.password url
      pure { proto, hostName, portStr, path, query, fragment, user, pwd }
    out.proto `shouldEqual` "https:"
    out.hostName `shouldEqual` "example.com"
    out.portStr `shouldEqual` "8443"
    out.path `shouldEqual` "/path"
    out.query `shouldEqual` "?q=1"
    out.fragment `shouldEqual` "#frag"
    out.user `shouldEqual` "user"
    out.pwd `shouldEqual` "pw"

  it "new' resolves a relative path against a base" do
    h <- runURL do
      url <- URL.new' "/relative" "https://example.com/base/"
      URL.href url
    h `shouldEqual` "https://example.com/relative"

  it "setters mutate the URL through the RIO accessors" do
    h <- runURL do
      url <- URL.new "https://example.com/"
      URL.setProtocol "http:" url
      URL.setHost "example.org:9000" url
      URL.setPathname "/new" url
      URL.href url
    h `shouldEqual` "http://example.org:9000/new"

  it "origin is the pure scheme + host + port projection" do
    o <- runURL do
      url <- URL.new "https://example.com:8443/whatever"
      pure (URL.origin url)
    o `shouldEqual` "https://example.com:8443"

  it "canParse is a pure parse-probe" do
    URL.canParse "https://example.com/" "https://example.com/" `shouldEqual` true
    URL.canParse "" "" `shouldEqual` false
    URL.canParse "::::bad::::" "" `shouldEqual` false

  it "pathToFileURL / fileURLToPath' round-trip an absolute path" do
    out <- runURL do
      url <- URL.pathToFileURL "/tmp/example.txt"
      URL.fileURLToPath' url
    out `shouldEqual` "/tmp/example.txt"
