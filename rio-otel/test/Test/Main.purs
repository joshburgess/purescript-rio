module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Tracer.OTelSpec as OTelSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  OTelSpec.spec
