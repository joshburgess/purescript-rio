module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.CoreSpec as CoreSpec
import Test.RIO.EffectAndFailSpec as EffectAndFailSpec
import Test.RIO.EnvSpec as EnvSpec
import Test.RIO.ErrorHandlingSpec as ErrorHandlingSpec
import Test.RIO.LayerSpec as LayerSpec
import Test.RIO.ResourceSpec as ResourceSpec
import Test.RIO.TestHelpersSpec as TestHelpersSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CoreSpec.spec
  EffectAndFailSpec.spec
  EnvSpec.spec
  ErrorHandlingSpec.spec
  ResourceSpec.spec
  LayerSpec.spec
  TestHelpersSpec.spec
