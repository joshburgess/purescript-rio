module Test.RioFiber.Main where

import Prelude

import Effect (Effect)
import Test.RIO.Fiber.CauseSpec as CauseSpec
import Test.RIO.Fiber.CoreSpec as CoreSpec
import Test.RIO.Fiber.DeferredSpec as DeferredSpec
import Test.RIO.Fiber.HubSpec as HubSpec
import Test.RIO.Fiber.LayerSpec as LayerSpec
import Test.RIO.Fiber.PoolSpec as PoolSpec
import Test.RIO.Fiber.QueueSpec as QueueSpec
import Test.RIO.Fiber.RefSpec as RefSpec
import Test.RIO.Fiber.ScheduleSpec as ScheduleSpec
import Test.RIO.Fiber.ScopeSpec as ScopeSpec
import Test.RIO.Fiber.SemaphoreSpec as SemaphoreSpec
import Test.RIO.Fiber.ServicesSpec as ServicesSpec
import Test.RIO.Fiber.StreamSpec as StreamSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CoreSpec.spec
  CauseSpec.spec
  ScopeSpec.spec
  RefSpec.spec
  DeferredSpec.spec
  SemaphoreSpec.spec
  QueueSpec.spec
  ScheduleSpec.spec
  LayerSpec.spec
  HubSpec.spec
  PoolSpec.spec
  ServicesSpec.spec
  StreamSpec.spec
