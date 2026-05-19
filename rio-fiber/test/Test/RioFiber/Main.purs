module Test.RioFiber.Main where

import Prelude

import Effect (Effect)
import Test.RIO.Fiber.CauseSpec as CauseSpec
import Test.RIO.Fiber.CoreSpec as CoreSpec
import Test.RIO.Fiber.DeferredSpec as DeferredSpec
import Test.RIO.Fiber.HubSpec as HubSpec
import Test.RIO.Fiber.LatchSpec as LatchSpec
import Test.RIO.Fiber.LayerSpec as LayerSpec
import Test.RIO.Fiber.MetricsSpec as MetricsSpec
import Test.RIO.Fiber.PoolSpec as PoolSpec
import Test.RIO.Fiber.QueueSpec as QueueSpec
import Test.RIO.Fiber.RefSpec as RefSpec
import Test.RIO.Fiber.ScheduleSpec as ScheduleSpec
import Test.RIO.Fiber.ScopeSpec as ScopeSpec
import Test.RIO.Fiber.SemaphoreSpec as SemaphoreSpec
import Test.RIO.Fiber.ServicesSpec as ServicesSpec
import Test.RIO.Fiber.SinkSpec as SinkSpec
import Test.RIO.Fiber.STMSpec as STMSpec
import Test.RIO.Fiber.STM.TArraySpec as TArraySpec
import Test.RIO.Fiber.STM.TChanSpec as TChanSpec
import Test.RIO.Fiber.STM.TMVarSpec as TMVarSpec
import Test.RIO.Fiber.STM.TQueueSpec as TQueueSpec
import Test.RIO.Fiber.StreamSpec as StreamSpec
import Test.RIO.Fiber.SupervisorSpec as SupervisorSpec
import Test.RIO.Fiber.TestClockSpec as TestClockSpec
import Test.RIO.Fiber.TracerSpec as TracerSpec
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
  LatchSpec.spec
  QueueSpec.spec
  ScheduleSpec.spec
  LayerSpec.spec
  HubSpec.spec
  PoolSpec.spec
  MetricsSpec.spec
  ServicesSpec.spec
  SinkSpec.spec
  StreamSpec.spec
  STMSpec.spec
  TMVarSpec.spec
  TChanSpec.spec
  TQueueSpec.spec
  TArraySpec.spec
  SupervisorSpec.spec
  TestClockSpec.spec
  TracerSpec.spec
