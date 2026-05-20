module Test.RioConfigFile.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Aff.Config.FileSpec as FileSpec
import Test.RIO.Aff.Config.FileIntegrationSpec as IntegrationSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  FileSpec.spec
  IntegrationSpec.spec
