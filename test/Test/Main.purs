module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.ClockSpec as ClockSpec
import Test.RIO.ConcurrencySpec as ConcurrencySpec
import Test.RIO.Concurrency.ParSpec as ParSpec
import Test.RIO.Concurrency.PropertiesSpec as ConcurrencyPropertiesSpec
import Test.RIO.ConfigSpec as ConfigSpec
import Test.RIO.Config.RotatingSpec as RotatingSpec
import Test.RIO.CacheSpec as CacheSpec
import Test.RIO.CauseSpec as CauseSpec
import Test.RIO.CoreSpec as CoreSpec
import Test.RIO.DeferredSpec as DeferredSpec
import Test.RIO.EffectAndFailSpec as EffectAndFailSpec
import Test.RIO.EnvSpec as EnvSpec
import Test.RIO.ErrorHandlingSpec as ErrorHandlingSpec
import Test.RIO.HubSpec as HubSpec
import Test.RIO.Hub.PropertiesSpec as HubPropertiesSpec
import Test.RIO.LayerSpec as LayerSpec
import Test.RIO.LocalSpec as LocalSpec
import Test.RIO.LoggerSpec as LoggerSpec
import Test.RIO.MetricsSpec as MetricsSpec
import Test.RIO.PoolSpec as PoolSpec
import Test.RIO.QueueSpec as QueueSpec
import Test.RIO.Queue.PropertiesSpec as QueuePropertiesSpec
import Test.RIO.RandomSpec as RandomSpec
import Test.RIO.RefSpec as RefSpec
import Test.RIO.Ref.SynchronizedSpec as SynchronizedRefSpec
import Test.RIO.Random.PropertiesSpec as RandomPropertiesSpec
import Test.RIO.ResourceSpec as ResourceSpec
import Test.RIO.Resource.DoSpec as ResourceDoSpec
import Test.RIO.ScheduleSpec as ScheduleSpec
import Test.RIO.Schedule.PropertiesSpec as SchedulePropertiesSpec
import Test.RIO.SemaphoreSpec as SemaphoreSpec
import Test.RIO.SinkSpec as SinkSpec
import Test.RIO.Sink.PropertiesSpec as SinkPropertiesSpec
import Test.RIO.SpecHelpersSpec as SpecHelpersSpec
import Test.RIO.StreamSpec as StreamSpec
import Test.RIO.Stream.CombinatorsSpec as StreamCombinatorsSpec
import Test.RIO.Stream.ParSpec as StreamParSpec
import Test.RIO.Stream.PropertiesSpec as StreamPropertiesSpec
import Test.RIO.Stream.ResourceSpec as StreamResourceSpec
import Test.RIO.Stream.SourcesSpec as StreamSourcesSpec
import Test.RIO.STMSpec as STMSpec
import Test.RIO.STM.TArraySpec as TArraySpec
import Test.RIO.STM.TMapSpec as TMapSpec
import Test.RIO.STM.TMap.PropertiesSpec as TMapPropertiesSpec
import Test.RIO.STM.TDeferredSpec as TDeferredSpec
import Test.RIO.STM.THubSpec as THubSpec
import Test.RIO.STM.THub.PropertiesSpec as THubPropertiesSpec
import Test.RIO.STM.TQueueSpec as TQueueSpec
import Test.RIO.STM.TQueue.PropertiesSpec as TQueuePropertiesSpec
import Test.RIO.STM.TSemaphoreSpec as TSemaphoreSpec
import Test.RIO.STM.TSemaphore.PropertiesSpec as TSemaphorePropertiesSpec
import Test.RIO.TestHelpersSpec as TestHelpersSpec
import Test.RIO.TracerSpec as TracerSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CoreSpec.spec
  EffectAndFailSpec.spec
  EnvSpec.spec
  ErrorHandlingSpec.spec
  ResourceSpec.spec
  ResourceDoSpec.spec
  LayerSpec.spec
  ConcurrencySpec.spec
  ParSpec.spec
  ConcurrencyPropertiesSpec.spec
  CauseSpec.spec
  DeferredSpec.spec
  ClockSpec.spec
  RandomSpec.spec
  RandomPropertiesSpec.spec
  RefSpec.spec
  SynchronizedRefSpec.spec
  ConfigSpec.spec
  RotatingSpec.spec
  ScheduleSpec.spec
  SchedulePropertiesSpec.spec
  SemaphoreSpec.spec
  QueueSpec.spec
  QueuePropertiesSpec.spec
  PoolSpec.spec
  CacheSpec.spec
  HubSpec.spec
  HubPropertiesSpec.spec
  StreamSpec.spec
  StreamCombinatorsSpec.spec
  StreamParSpec.spec
  StreamPropertiesSpec.spec
  StreamResourceSpec.spec
  StreamSourcesSpec.spec
  SinkSpec.spec
  SinkPropertiesSpec.spec
  STMSpec.spec
  TQueueSpec.spec
  TQueuePropertiesSpec.spec
  TMapSpec.spec
  TMapPropertiesSpec.spec
  TArraySpec.spec
  TSemaphoreSpec.spec
  TSemaphorePropertiesSpec.spec
  THubSpec.spec
  THubPropertiesSpec.spec
  TDeferredSpec.spec
  TracerSpec.spec
  MetricsSpec.spec
  LocalSpec.spec
  LoggerSpec.spec
  TestHelpersSpec.spec
  SpecHelpersSpec.spec
