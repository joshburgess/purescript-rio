module Test.RioFiberOtel.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Fiber.Tracer.OTel.AdapterSpec as OTelSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  OTelSpec.spec
