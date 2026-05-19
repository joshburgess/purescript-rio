module Test.RioFiber.Main where

import Prelude

import Effect (Effect)
import Test.RIO.Fiber.CoreSpec as CoreSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CoreSpec.spec
