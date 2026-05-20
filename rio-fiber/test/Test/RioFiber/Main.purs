module Test.RioFiber.Main where

import Prelude

import Effect (Effect)
import Test.RIO.Fiber.AffSpec as AffSpec
import Test.RIO.Fiber.CauseSpec as CauseSpec
import Test.RIO.Fiber.ClockSpec as ClockSpec
import Test.RIO.Fiber.ConfigSpec as ConfigSpec
import Test.RIO.Fiber.CoreSpec as CoreSpec
import Test.RIO.Fiber.DeferredSpec as DeferredSpec
import Test.RIO.Fiber.Hub.PropertiesSpec as HubPropertiesSpec
import Test.RIO.Fiber.HubSpec as HubSpec
import Test.RIO.Fiber.InternalSpec as InternalSpec
import Test.RIO.Fiber.LatchSpec as LatchSpec
import Test.RIO.Fiber.LayerSpec as LayerSpec
import Test.RIO.Fiber.LoggerSpec as LoggerSpec
import Test.RIO.Fiber.MetricsSpec as MetricsSpec
import Test.RIO.Fiber.PipeSpec as PipeSpec
import Test.RIO.Fiber.PoolSpec as PoolSpec
import Test.RIO.Fiber.Queue.PropertiesSpec as QueuePropertiesSpec
import Test.RIO.Fiber.QueueSpec as QueueSpec
import Test.RIO.Fiber.RandomSpec as RandomSpec
import Test.RIO.Fiber.RefSpec as RefSpec
import Test.RIO.Fiber.ScheduleSpec as ScheduleSpec
import Test.RIO.Fiber.ScopeSpec as ScopeSpec
import Test.RIO.Fiber.SemaphoreSpec as SemaphoreSpec
import Test.RIO.Fiber.ServicesSpec as ServicesSpec
import Test.RIO.Fiber.SinkSpec as SinkSpec
import Test.RIO.Fiber.STMSpec as STMSpec
import Test.RIO.Fiber.STM.PropertiesSpec as STMPropertiesSpec
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
  InternalSpec.spec
  CauseSpec.spec
  ClockSpec.spec
  ConfigSpec.spec
  LoggerSpec.spec
  RandomSpec.spec
  AffSpec.spec
  ScopeSpec.spec
  RefSpec.spec
  DeferredSpec.spec
  SemaphoreSpec.spec
  LatchSpec.spec
  QueueSpec.spec
  QueuePropertiesSpec.spec
  ScheduleSpec.spec
  LayerSpec.spec
  HubSpec.spec
  HubPropertiesSpec.spec
  PoolSpec.spec
  MetricsSpec.spec
  ServicesSpec.spec
  SinkSpec.spec
  PipeSpec.spec
  StreamSpec.spec
  STMSpec.spec
  TMVarSpec.spec
  TChanSpec.spec
  TQueueSpec.spec
  TArraySpec.spec
  STMPropertiesSpec.spec
  SupervisorSpec.spec
  TestClockSpec.spec
  TracerSpec.spec
