module Test.RioFiberConfigFile.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Fiber.Config.FileSpec as FileSpec
import Test.RIO.Fiber.Config.FileIntegrationSpec as IntegrationSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  FileSpec.spec
  IntegrationSpec.spec
