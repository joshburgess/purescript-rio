module Test.RIO.Aff.Node.OSSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.String (length) as String
import Effect.Aff (Aff)
import Foreign.Object (size) as Object
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Aff.Core (RIO, provideAll, runRIO')
import RIO.Aff.Node.OS
  ( OS
  , constants
  , cpus
  , devNull
  , eol
  , freemem
  , homedir
  , hostname
  , liveOS
  , networkInterfaces
  , platform
  , release
  , tmpdir
  , totalmem
  , uptime
  , version
  )

runOS :: forall a. RIO (os :: OS) () a -> Aff a
runOS p = runRIO' (provideAll { os: liveOS } p)

spec :: Spec Unit
spec = describe "RIO.Aff.Node.OS (live)" do
  it "eol is a non-empty platform string" do
    String.length eol `shouldSatisfy` (_ > 0)

  it "devNull names a non-empty path" do
    String.length devNull `shouldSatisfy` (_ > 0)

  it "constants exposes the underlying OS-constants object" do
    Object.size constants `shouldSatisfy` (_ > 0)

  it "hostname returns a non-empty string" do
    h <- runOS hostname
    String.length h `shouldSatisfy` (_ > 0)

  it "homedir returns a non-empty path" do
    h <- runOS homedir
    String.length h `shouldSatisfy` (_ > 0)

  it "tmpdir returns a non-empty path" do
    t <- runOS tmpdir
    String.length t `shouldSatisfy` (_ > 0)

  it "platform, release, and version are all non-empty" do
    out <- runOS do
      p <- platform
      r <- release
      v <- version
      pure { p, r, v }
    String.length out.p `shouldSatisfy` (_ > 0)
    String.length out.r `shouldSatisfy` (_ > 0)
    String.length out.v `shouldSatisfy` (_ > 0)

  it "totalmem >= freemem" do
    { total, free } <- runOS do
      total <- totalmem
      free <- freemem
      pure { total, free }
    (total >= free) `shouldEqual` true

  it "uptime is non-negative" do
    u <- runOS uptime
    (u >= 0.0) `shouldEqual` true

  it "cpus returns at least one core" do
    cs <- runOS cpus
    (Array.length cs > 0) `shouldEqual` true

  it "networkInterfaces returns an object (possibly empty)" do
    _ <- runOS networkInterfaces
    pure unit
