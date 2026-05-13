module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.ClockSpec as ClockSpec
import Test.RIO.ConcurrencySpec as ConcurrencySpec
import Test.RIO.CoreSpec as CoreSpec
import Test.RIO.DeferredSpec as DeferredSpec
import Test.RIO.EffectAndFailSpec as EffectAndFailSpec
import Test.RIO.EnvSpec as EnvSpec
import Test.RIO.ErrorHandlingSpec as ErrorHandlingSpec
import Test.RIO.LayerSpec as LayerSpec
import Test.RIO.ResourceSpec as ResourceSpec
import Test.RIO.ScheduleSpec as ScheduleSpec
import Test.RIO.SpecHelpersSpec as SpecHelpersSpec
import Test.RIO.TestHelpersSpec as TestHelpersSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CoreSpec.spec
  EffectAndFailSpec.spec
  EnvSpec.spec
  ErrorHandlingSpec.spec
  ResourceSpec.spec
  LayerSpec.spec
  ConcurrencySpec.spec
  DeferredSpec.spec
  ClockSpec.spec
  ScheduleSpec.spec
  TestHelpersSpec.spec
  SpecHelpersSpec.spec
