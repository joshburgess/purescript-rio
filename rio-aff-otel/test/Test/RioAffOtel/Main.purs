module Test.RioAffOtel.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Aff.Tracer.OTel.AdapterSpec as OTelSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  OTelSpec.spec
