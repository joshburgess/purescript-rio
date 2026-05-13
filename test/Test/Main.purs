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
import Test.RIO.MetricsSpec as MetricsSpec
import Test.RIO.ResourceSpec as ResourceSpec
import Test.RIO.ScheduleSpec as ScheduleSpec
import Test.RIO.SpecHelpersSpec as SpecHelpersSpec
import Test.RIO.STMSpec as STMSpec
import Test.RIO.STM.TMapSpec as TMapSpec
import Test.RIO.STM.TQueueSpec as TQueueSpec
import Test.RIO.STM.TSemaphoreSpec as TSemaphoreSpec
import Test.RIO.TestHelpersSpec as TestHelpersSpec
import Test.RIO.TracerSpec as TracerSpec

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
  STMSpec.spec
  TQueueSpec.spec
  TMapSpec.spec
  TSemaphoreSpec.spec
  TracerSpec.spec
  MetricsSpec.spec
  TestHelpersSpec.spec
  SpecHelpersSpec.spec
