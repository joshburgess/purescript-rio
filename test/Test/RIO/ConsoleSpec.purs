-- | Smoke test for `RIO.Console`. These functions are thin
-- | `liftEffect` wrappers over `Effect.Console`; what we are
-- | verifying here is only that each one round-trips through
-- | `runRIO'` without throwing and returns `Unit`. The actual
-- | byte output goes to the test process's stdout / stderr by
-- | design.
module Test.RIO.ConsoleSpec (spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Console
  ( debug
  , debugShow
  , error
  , errorShow
  , info
  , infoShow
  , log
  , logShow
  , warn
  , warnShow
  )
import RIO.Core (RIO, runRIO')

silentTag :: String
silentTag = "[RIO.ConsoleSpec smoke]"

spec :: Spec Unit
spec = describe "RIO.Console" do
  it "log runs to completion inside RIO" do
    result <- runRIO' (log silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "logShow runs to completion inside RIO" do
    result <- runRIO' (logShow silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "warn runs to completion inside RIO" do
    result <- runRIO' (warn silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "warnShow runs to completion inside RIO" do
    result <- runRIO' (warnShow silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "error runs to completion inside RIO" do
    result <- runRIO' (error silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "errorShow runs to completion inside RIO" do
    result <- runRIO' (errorShow silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "info runs to completion inside RIO" do
    result <- runRIO' (info silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "infoShow runs to completion inside RIO" do
    result <- runRIO' (infoShow silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "debug runs to completion inside RIO" do
    result <- runRIO' (debug silentTag :: RIO () () Unit)
    result `shouldEqual` unit

  it "debugShow runs to completion inside RIO" do
    result <- runRIO' (debugShow silentTag :: RIO () () Unit)
    result `shouldEqual` unit
