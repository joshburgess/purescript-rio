module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Aff.Clock.PartsSpec as ClockPartsSpec
import Test.RIO.Aff.ClockSpec as ClockSpec
import Test.RIO.Aff.ConcurrencySpec as ConcurrencySpec
import Test.RIO.Aff.Concurrency.AsyncSpec as ConcurrencyAsyncSpec
import Test.RIO.Aff.Concurrency.ParSpec as ParSpec
import Test.RIO.Aff.Concurrency.PropertiesSpec as ConcurrencyPropertiesSpec
import Test.RIO.Aff.Concurrency.NeverFilterSpec as NeverFilterSpec
import Test.RIO.Aff.Concurrency.TimeoutRaceValidateSpec as TimeoutRaceValidateSpec
import Test.RIO.Aff.Concurrency.ValidatePartitionSpec as ValidatePartitionSpec
import Test.RIO.Aff.Concurrency.ZipWithParSpec as ZipWithParSpec
import Test.RIO.Aff.ConditionalSpec as ConditionalSpec
import Test.RIO.Aff.ConfigSpec as ConfigSpec
import Test.RIO.Aff.ConsoleSpec as ConsoleSpec
import Test.RIO.Aff.Config.RotatingSpec as RotatingSpec
import Test.RIO.Aff.CacheSpec as CacheSpec
import Test.RIO.Aff.BrandSpec as BrandSpec
import Test.RIO.Aff.CauseSpec as CauseSpec
import Test.RIO.Aff.ChunkSpec as ChunkSpec
import Test.RIO.Aff.CircuitBreakerSpec as CircuitBreakerSpec
import Test.RIO.Aff.ChannelSpec as ChannelSpec
import Test.RIO.Aff.Cause.CombinatorsSpec as CauseCombinatorsSpec
import Test.RIO.Aff.Cause.CatchSomeSpec as CauseCatchSomeSpec
import Test.RIO.Aff.Cause.FoldLinearizeSpec as CauseFoldLinearizeSpec
import Test.RIO.Aff.Cause.InspectionSpec as CauseInspectionSpec
import Test.RIO.Aff.CoreSpec as CoreSpec
import Test.RIO.Aff.DeferredSpec as DeferredSpec
import Test.RIO.Aff.EffectAndFailSpec as EffectAndFailSpec
import Test.RIO.Aff.EnvSpec as EnvSpec
import Test.RIO.Aff.ExitSpec as ExitSpec
import Test.RIO.Aff.Error.CatchSomeSpec as ErrorCatchSomeSpec
import Test.RIO.Aff.Error.CombinatorsSpec as ErrorCombinatorsSpec
import Test.RIO.Aff.Error.OrElseSpec as ErrorOrElseSpec
import Test.RIO.Aff.Error.RefineSpec as ErrorRefineSpec
import Test.RIO.Aff.Error.TapSpec as ErrorTapSpec
import Test.RIO.Aff.ErrorHandlingSpec as ErrorHandlingSpec
import Test.RIO.Aff.FailSpec as FailSpec
import Test.RIO.Aff.FoldForeverSpec as FoldForeverSpec
import Test.RIO.Aff.HttpClientSpec as HttpClientSpec
import Test.RIO.Aff.HttpServerSpec as HttpServerSpec
import Test.RIO.Aff.Test.HttpClientSpec as TestHttpClientSpec
import Test.RIO.Aff.Test.HttpServerSpec as TestHttpServerSpec
import Test.RIO.Aff.Test.WebSocketSpec as TestWebSocketSpec
import Test.RIO.Aff.HttpStreamSpec as HttpStreamSpec
import Test.RIO.Aff.HubSpec as HubSpec
import Test.RIO.Aff.IterateReplicateSpec as IterateReplicateSpec
import Test.RIO.Aff.Hub.PropertiesSpec as HubPropertiesSpec
import Test.RIO.Aff.LayerSpec as LayerSpec
import Test.RIO.Aff.FiberRefSpec as FiberRefSpec
import Test.RIO.Aff.LocalSpec as LocalSpec
import Test.RIO.Aff.RuntimeSpec as RuntimeSpec
import Test.RIO.Aff.WorkerPoolSpec as WorkerPoolSpec
import Test.RIO.Aff.LoggerSpec as LoggerSpec
import Test.RIO.Aff.Logger.CompositionSpec as LoggerCompositionSpec
import Test.RIO.Aff.MemoSpec as MemoSpec
import Test.RIO.Aff.MetricSpec as MetricSpec
import Test.RIO.Aff.MetricsSpec as MetricsSpec
import Test.RIO.Aff.Metrics.OTelSpec as MetricsOTelSpec
import Test.RIO.Aff.PoolSpec as PoolSpec
import Test.RIO.Aff.PredicateSpec as PredicateSpec
import Test.RIO.Aff.QueueSpec as QueueSpec
import Test.RIO.Aff.Queue.BulkSpec as QueueBulkSpec
import Test.RIO.Aff.Queue.PropertiesSpec as QueuePropertiesSpec
import Test.RIO.Aff.QuerySpec as QuerySpec
import Test.RIO.Aff.RandomSpec as RandomSpec
import Test.RIO.Aff.RateLimiterSpec as RateLimiterSpec
import Test.RIO.Aff.RefSpec as RefSpec
import Test.RIO.Aff.OpenApiSpec as OpenApiSpec
import Test.RIO.Aff.SchemaSpec as SchemaSpec
import Test.RIO.Aff.SqlSpec as SqlSpec
import Test.RIO.Aff.SystemSpec as SystemSpec
import Test.RIO.Aff.Ref.SynchronizedSpec as SynchronizedRefSpec
import Test.RIO.Aff.Random.PropertiesSpec as RandomPropertiesSpec
import Test.RIO.Aff.Random.ShufflePickSpec as RandomShufflePickSpec
import Test.RIO.Aff.Random.WeightedSpec as RandomWeightedSpec
import Test.RIO.Aff.ResourceSpec as ResourceSpec
import Test.RIO.Aff.Resource.DoSpec as ResourceDoSpec
import Test.RIO.Aff.ScheduleSpec as ScheduleSpec
import Test.RIO.Aff.Schedule.CollectRepetitionsTapSpec as ScheduleCollectRepetitionsTapSpec
import Test.RIO.Aff.Schedule.ConstructorsSpec as ScheduleConstructorsSpec
import Test.RIO.Aff.Schedule.CronSpec as ScheduleCronSpec
import Test.RIO.Aff.Schedule.DimapDelaySpec as ScheduleDimapDelaySpec
import Test.RIO.Aff.Schedule.ElapsedSpec as ScheduleElapsedSpec
import Test.RIO.Aff.Schedule.EventuallySpec as ScheduleEventuallySpec
import Test.RIO.Aff.Schedule.FixedSpec as ScheduleFixedSpec
import Test.RIO.Aff.Schedule.ModifyDelayMSpec as ScheduleModifyDelayMSpec
import Test.RIO.Aff.Schedule.OutputsSpec as ScheduleOutputsSpec
import Test.RIO.Aff.Schedule.PropertiesSpec as SchedulePropertiesSpec
import Test.RIO.Aff.SemaphoreSpec as SemaphoreSpec
import Test.RIO.Aff.SinkSpec as SinkSpec
import Test.RIO.Aff.Sink.AggregateSpec as SinkAggregateSpec
import Test.RIO.Aff.Sink.PropertiesSpec as SinkPropertiesSpec
import Test.RIO.Aff.SpecHelpersSpec as SpecHelpersSpec
import Test.RIO.Aff.StreamSpec as StreamSpec
import Test.RIO.Aff.Stream.CombinatorsSpec as StreamCombinatorsSpec
import Test.RIO.Aff.Stream.FilterMapCollectSpec as StreamFilterMapCollectSpec
import Test.RIO.Aff.Stream.HaltInterruptSpec as StreamHaltInterruptSpec
import Test.RIO.Aff.Stream.HeadLastFindSpec as StreamHeadLastFindSpec
import Test.RIO.Aff.Stream.IntoSpec as StreamIntoSpec
import Test.RIO.Aff.Stream.ConcurrentSpec as StreamConcurrentSpec
import Test.RIO.Aff.Stream.ParSpec as StreamParSpec
import Test.RIO.Aff.Stream.PropertiesSpec as StreamPropertiesSpec
import Test.RIO.Aff.Stream.ResourceSpec as StreamResourceSpec
import Test.RIO.Aff.Stream.SlidingGroupConsSpec as StreamSlidingGroupConsSpec
import Test.RIO.Aff.Stream.SourcesSpec as StreamSourcesSpec
import Test.RIO.Aff.Stream.TakeDropUntilSpec as StreamTakeDropUntilSpec
import Test.RIO.Aff.Stream.TapMapAccumSpec as StreamTapMapAccumSpec
import Test.RIO.Aff.Stream.TimedSpec as StreamTimedSpec
import Test.RIO.Aff.STMSpec as STMSpec
import Test.RIO.Aff.STM.PropertiesSpec as STMPropertiesSpec
import Test.RIO.Aff.STM.TArraySpec as TArraySpec
import Test.RIO.Aff.STM.TMapSpec as TMapSpec
import Test.RIO.Aff.STM.TMap.PropertiesSpec as TMapPropertiesSpec
import Test.RIO.Aff.STM.TMap.QuerySpec as TMapQuerySpec
import Test.RIO.Aff.STM.TDeferredSpec as TDeferredSpec
import Test.RIO.Aff.STM.THubSpec as THubSpec
import Test.RIO.Aff.STM.THub.PropertiesSpec as THubPropertiesSpec
import Test.RIO.Aff.STM.TQueueSpec as TQueueSpec
import Test.RIO.Aff.STM.TQueue.BulkSpec as TQueueBulkSpec
import Test.RIO.Aff.STM.TQueue.PropertiesSpec as TQueuePropertiesSpec
import Test.RIO.Aff.STM.TSemaphoreSpec as TSemaphoreSpec
import Test.RIO.Aff.STM.TSemaphore.PropertiesSpec as TSemaphorePropertiesSpec
import Test.RIO.Aff.TagSpec as TagSpec
import Test.RIO.Aff.Test.PropertySpec as TestPropertySpec
import Test.RIO.Aff.TestHelpersSpec as TestHelpersSpec
import Test.RIO.Aff.TimeSpec as TimeSpec
import Test.RIO.Aff.TracerSpec as TracerSpec
import Test.RIO.Aff.Tracer.OTelSpec as TracerOTelSpec
import Test.RIO.Aff.Tracer.PropagationSpec as TracerPropagationSpec
import Test.RIO.Aff.ValidationSpec as ValidationSpec
import Test.RIO.Aff.WebSocketSpec as WebSocketSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  CoreSpec.spec
  EffectAndFailSpec.spec
  EnvSpec.spec
  TagSpec.spec
  ExitSpec.spec
  ErrorHandlingSpec.spec
  ErrorCombinatorsSpec.spec
  ErrorOrElseSpec.spec
  ErrorTapSpec.spec
  ErrorRefineSpec.spec
  ErrorCatchSomeSpec.spec
  ConditionalSpec.spec
  IterateReplicateSpec.spec
  FailSpec.spec
  FoldForeverSpec.spec
  ResourceSpec.spec
  ResourceDoSpec.spec
  LayerSpec.spec
  ConcurrencySpec.spec
  ConcurrencyAsyncSpec.spec
  ParSpec.spec
  ConcurrencyPropertiesSpec.spec
  ValidatePartitionSpec.spec
  NeverFilterSpec.spec
  TimeoutRaceValidateSpec.spec
  ZipWithParSpec.spec
  BrandSpec.spec
  ChunkSpec.spec
  CircuitBreakerSpec.spec
  CauseSpec.spec
  ChannelSpec.spec
  CauseCombinatorsSpec.spec
  CauseCatchSomeSpec.spec
  CauseFoldLinearizeSpec.spec
  CauseInspectionSpec.spec
  DeferredSpec.spec
  ClockSpec.spec
  ClockPartsSpec.spec
  RandomSpec.spec
  RateLimiterSpec.spec
  RandomPropertiesSpec.spec
  RandomShufflePickSpec.spec
  RandomWeightedSpec.spec
  RefSpec.spec
  SynchronizedRefSpec.spec
  ConfigSpec.spec
  RotatingSpec.spec
  ScheduleSpec.spec
  ScheduleFixedSpec.spec
  ScheduleModifyDelayMSpec.spec
  ScheduleOutputsSpec.spec
  ScheduleElapsedSpec.spec
  ScheduleEventuallySpec.spec
  ScheduleCollectRepetitionsTapSpec.spec
  ScheduleConstructorsSpec.spec
  ScheduleCronSpec.spec
  ScheduleDimapDelaySpec.spec
  SchedulePropertiesSpec.spec
  SemaphoreSpec.spec
  QueueSpec.spec
  QueueBulkSpec.spec
  QueuePropertiesSpec.spec
  QuerySpec.spec
  PoolSpec.spec
  PredicateSpec.spec
  CacheSpec.spec
  HubSpec.spec
  HubPropertiesSpec.spec
  HttpClientSpec.spec
  HttpServerSpec.spec
  HttpStreamSpec.spec
  TestHttpClientSpec.spec
  TestHttpServerSpec.spec
  WebSocketSpec.spec
  TestWebSocketSpec.spec
  StreamSpec.spec
  StreamCombinatorsSpec.spec
  StreamFilterMapCollectSpec.spec
  StreamHaltInterruptSpec.spec
  StreamHeadLastFindSpec.spec
  StreamIntoSpec.spec
  StreamConcurrentSpec.spec
  StreamParSpec.spec
  StreamPropertiesSpec.spec
  StreamResourceSpec.spec
  StreamSlidingGroupConsSpec.spec
  StreamSourcesSpec.spec
  StreamTakeDropUntilSpec.spec
  StreamTapMapAccumSpec.spec
  StreamTimedSpec.spec
  SinkSpec.spec
  SinkAggregateSpec.spec
  SinkPropertiesSpec.spec
  STMSpec.spec
  STMPropertiesSpec.spec
  TQueueSpec.spec
  TQueueBulkSpec.spec
  TQueuePropertiesSpec.spec
  TMapSpec.spec
  TMapPropertiesSpec.spec
  TMapQuerySpec.spec
  TArraySpec.spec
  TSemaphoreSpec.spec
  TSemaphorePropertiesSpec.spec
  THubSpec.spec
  THubPropertiesSpec.spec
  TDeferredSpec.spec
  TimeSpec.spec
  SchemaSpec.spec
  OpenApiSpec.spec
  SqlSpec.spec
  SystemSpec.spec
  TracerSpec.spec
  TracerPropagationSpec.spec
  TracerOTelSpec.spec
  ValidationSpec.spec
  MetricSpec.spec
  MetricsSpec.spec
  MetricsOTelSpec.spec
  FiberRefSpec.spec
  WorkerPoolSpec.spec
  RuntimeSpec.spec
  LocalSpec.spec
  LoggerSpec.spec
  LoggerCompositionSpec.spec
  MemoSpec.spec
  TestHelpersSpec.spec
  TestPropertySpec.spec
  SpecHelpersSpec.spec
  ConsoleSpec.spec
