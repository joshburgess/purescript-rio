module Test.RioFiber.Main where

import Prelude

import Effect (Effect)
import Test.RIO.Fiber.AbortSignalSpec as AbortSignalSpec
import Test.RIO.Fiber.AffSpec as AffSpec
import Test.RIO.Fiber.CacheSpec as CacheSpec
import Test.RIO.Fiber.CauseSpec as CauseSpec
import Test.RIO.Fiber.CircuitBreakerSpec as CircuitBreakerSpec
import Test.RIO.Fiber.ClockSpec as ClockSpec
import Test.RIO.Fiber.Config.RotatingSpec as RotatingSpec
import Test.RIO.Fiber.ConfigSpec as ConfigSpec
import Test.RIO.Fiber.CoreSpec as CoreSpec
import Test.RIO.Fiber.DataLoaderSpec as DataLoaderSpec
import Test.RIO.Fiber.DeferredSpec as DeferredSpec
import Test.RIO.Fiber.ErrorSpec as ErrorSpec
import Test.RIO.Fiber.FiberHandleSpec as FiberHandleSpec
import Test.RIO.Fiber.FiberSetSpec as FiberSetSpec
import Test.RIO.Fiber.Hub.PropertiesSpec as HubPropertiesSpec
import Test.RIO.Fiber.HubSpec as HubSpec
import Test.RIO.Fiber.InspectSpec as InspectSpec
import Test.RIO.Fiber.InternalSpec as InternalSpec
import Test.RIO.Fiber.KeyedPoolSpec as KeyedPoolSpec
import Test.RIO.Fiber.LatchSpec as LatchSpec
import Test.RIO.Fiber.LayerSpec as LayerSpec
import Test.RIO.Fiber.LoggerSpec as LoggerSpec
import Test.RIO.Fiber.MailboxSpec as MailboxSpec
import Test.RIO.Fiber.MemoSpec as MemoSpec
import Test.RIO.Fiber.MetricsSpec as MetricsSpec
import Test.RIO.Fiber.PipeSpec as PipeSpec
import Test.RIO.Fiber.PoolSpec as PoolSpec
import Test.RIO.Fiber.PromiseSpec as PromiseSpec
import Test.RIO.Fiber.Queue.PropertiesSpec as QueuePropertiesSpec
import Test.RIO.Fiber.QueueSpec as QueueSpec
import Test.RIO.Fiber.RcRefSpec as RcRefSpec
import Test.RIO.Fiber.ReloadableSpec as ReloadableSpec
import Test.RIO.Fiber.RandomSpec as RandomSpec
import Test.RIO.Fiber.RateLimiterSpec as RateLimiterSpec
import Test.RIO.Fiber.Ref.SubscriptionSpec as SubscriptionSpec
import Test.RIO.Fiber.Ref.SynchronizedSpec as SynchronizedSpec
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
import Test.RIO.Fiber.STM.TDeferredSpec as TDeferredSpec
import Test.RIO.Fiber.STM.TMapSpec as TMapSpec
import Test.RIO.Fiber.STM.TMVarSpec as TMVarSpec
import Test.RIO.Fiber.STM.TPubSubSpec as TPubSubSpec
import Test.RIO.Fiber.STM.TQueueSpec as TQueueSpec
import Test.RIO.Fiber.STM.TSemaphoreSpec as TSemaphoreSpec
import Test.RIO.Fiber.STM.TSetSpec as TSetSpec
import Test.RIO.Fiber.Stream.AsyncIterableSpec as AsyncIterableSpec
import Test.RIO.Fiber.StreamSpec as StreamSpec
import Test.RIO.Fiber.SupervisorSpec as SupervisorSpec
import Test.RIO.Fiber.TestClockSpec as TestClockSpec
import Test.RIO.Fiber.TracerSpec as TracerSpec
import Test.RIO.Fiber.WorkerPoolSpec as WorkerPoolSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CoreSpec.spec
  InternalSpec.spec
  CauseSpec.spec
  ErrorSpec.spec
  ClockSpec.spec
  ConfigSpec.spec
  LoggerSpec.spec
  RandomSpec.spec
  AffSpec.spec
  PromiseSpec.spec
  AbortSignalSpec.spec
  ScopeSpec.spec
  RefSpec.spec
  SynchronizedSpec.spec
  SubscriptionSpec.spec
  RotatingSpec.spec
  DeferredSpec.spec
  SemaphoreSpec.spec
  LatchSpec.spec
  QueueSpec.spec
  QueuePropertiesSpec.spec
  MailboxSpec.spec
  ScheduleSpec.spec
  LayerSpec.spec
  HubSpec.spec
  HubPropertiesSpec.spec
  PoolSpec.spec
  KeyedPoolSpec.spec
  RcRefSpec.spec
  ReloadableSpec.spec
  DataLoaderSpec.spec
  MetricsSpec.spec
  ServicesSpec.spec
  SinkSpec.spec
  PipeSpec.spec
  StreamSpec.spec
  AsyncIterableSpec.spec
  STMSpec.spec
  TMVarSpec.spec
  TChanSpec.spec
  TQueueSpec.spec
  TArraySpec.spec
  TSemaphoreSpec.spec
  TDeferredSpec.spec
  TMapSpec.spec
  TSetSpec.spec
  TPubSubSpec.spec
  STMPropertiesSpec.spec
  SupervisorSpec.spec
  FiberHandleSpec.spec
  FiberSetSpec.spec
  InspectSpec.spec
  TestClockSpec.spec
  TracerSpec.spec
  MemoSpec.spec
  CacheSpec.spec
  RateLimiterSpec.spec
  CircuitBreakerSpec.spec
  WorkerPoolSpec.spec
