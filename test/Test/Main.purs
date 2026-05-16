module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Clock.PartsSpec as ClockPartsSpec
import Test.RIO.ClockSpec as ClockSpec
import Test.RIO.ConcurrencySpec as ConcurrencySpec
import Test.RIO.Concurrency.AsyncSpec as ConcurrencyAsyncSpec
import Test.RIO.Concurrency.ParSpec as ParSpec
import Test.RIO.Concurrency.PropertiesSpec as ConcurrencyPropertiesSpec
import Test.RIO.Concurrency.NeverFilterSpec as NeverFilterSpec
import Test.RIO.Concurrency.TimeoutRaceValidateSpec as TimeoutRaceValidateSpec
import Test.RIO.Concurrency.ValidatePartitionSpec as ValidatePartitionSpec
import Test.RIO.Concurrency.ZipWithParSpec as ZipWithParSpec
import Test.RIO.ConditionalSpec as ConditionalSpec
import Test.RIO.ConfigSpec as ConfigSpec
import Test.RIO.Config.RotatingSpec as RotatingSpec
import Test.RIO.CacheSpec as CacheSpec
import Test.RIO.CauseSpec as CauseSpec
import Test.RIO.ChannelSpec as ChannelSpec
import Test.RIO.Cause.CombinatorsSpec as CauseCombinatorsSpec
import Test.RIO.Cause.CatchSomeSpec as CauseCatchSomeSpec
import Test.RIO.Cause.FoldLinearizeSpec as CauseFoldLinearizeSpec
import Test.RIO.Cause.InspectionSpec as CauseInspectionSpec
import Test.RIO.CoreSpec as CoreSpec
import Test.RIO.DeferredSpec as DeferredSpec
import Test.RIO.EffectAndFailSpec as EffectAndFailSpec
import Test.RIO.EnvSpec as EnvSpec
import Test.RIO.ExitSpec as ExitSpec
import Test.RIO.Error.CatchSomeSpec as ErrorCatchSomeSpec
import Test.RIO.Error.CombinatorsSpec as ErrorCombinatorsSpec
import Test.RIO.Error.OrElseSpec as ErrorOrElseSpec
import Test.RIO.Error.RefineSpec as ErrorRefineSpec
import Test.RIO.Error.TapSpec as ErrorTapSpec
import Test.RIO.ErrorHandlingSpec as ErrorHandlingSpec
import Test.RIO.FailSpec as FailSpec
import Test.RIO.FoldForeverSpec as FoldForeverSpec
import Test.RIO.HubSpec as HubSpec
import Test.RIO.IterateReplicateSpec as IterateReplicateSpec
import Test.RIO.Hub.PropertiesSpec as HubPropertiesSpec
import Test.RIO.LayerSpec as LayerSpec
import Test.RIO.LocalSpec as LocalSpec
import Test.RIO.LoggerSpec as LoggerSpec
import Test.RIO.Logger.CompositionSpec as LoggerCompositionSpec
import Test.RIO.MemoSpec as MemoSpec
import Test.RIO.MetricSpec as MetricSpec
import Test.RIO.MetricsSpec as MetricsSpec
import Test.RIO.PoolSpec as PoolSpec
import Test.RIO.QueueSpec as QueueSpec
import Test.RIO.Queue.BulkSpec as QueueBulkSpec
import Test.RIO.Queue.PropertiesSpec as QueuePropertiesSpec
import Test.RIO.RandomSpec as RandomSpec
import Test.RIO.RateLimiterSpec as RateLimiterSpec
import Test.RIO.RefSpec as RefSpec
import Test.RIO.SchemaSpec as SchemaSpec
import Test.RIO.Ref.SynchronizedSpec as SynchronizedRefSpec
import Test.RIO.Random.PropertiesSpec as RandomPropertiesSpec
import Test.RIO.Random.ShufflePickSpec as RandomShufflePickSpec
import Test.RIO.Random.WeightedSpec as RandomWeightedSpec
import Test.RIO.ResourceSpec as ResourceSpec
import Test.RIO.Resource.DoSpec as ResourceDoSpec
import Test.RIO.ScheduleSpec as ScheduleSpec
import Test.RIO.Schedule.CollectRepetitionsTapSpec as ScheduleCollectRepetitionsTapSpec
import Test.RIO.Schedule.ConstructorsSpec as ScheduleConstructorsSpec
import Test.RIO.Schedule.CronSpec as ScheduleCronSpec
import Test.RIO.Schedule.DimapDelaySpec as ScheduleDimapDelaySpec
import Test.RIO.Schedule.ElapsedSpec as ScheduleElapsedSpec
import Test.RIO.Schedule.EventuallySpec as ScheduleEventuallySpec
import Test.RIO.Schedule.FixedSpec as ScheduleFixedSpec
import Test.RIO.Schedule.ModifyDelayMSpec as ScheduleModifyDelayMSpec
import Test.RIO.Schedule.OutputsSpec as ScheduleOutputsSpec
import Test.RIO.Schedule.PropertiesSpec as SchedulePropertiesSpec
import Test.RIO.SemaphoreSpec as SemaphoreSpec
import Test.RIO.SinkSpec as SinkSpec
import Test.RIO.Sink.AggregateSpec as SinkAggregateSpec
import Test.RIO.Sink.PropertiesSpec as SinkPropertiesSpec
import Test.RIO.SpecHelpersSpec as SpecHelpersSpec
import Test.RIO.StreamSpec as StreamSpec
import Test.RIO.Stream.CombinatorsSpec as StreamCombinatorsSpec
import Test.RIO.Stream.FilterMapCollectSpec as StreamFilterMapCollectSpec
import Test.RIO.Stream.HaltInterruptSpec as StreamHaltInterruptSpec
import Test.RIO.Stream.HeadLastFindSpec as StreamHeadLastFindSpec
import Test.RIO.Stream.IntoSpec as StreamIntoSpec
import Test.RIO.Stream.ParSpec as StreamParSpec
import Test.RIO.Stream.PropertiesSpec as StreamPropertiesSpec
import Test.RIO.Stream.ResourceSpec as StreamResourceSpec
import Test.RIO.Stream.SlidingGroupConsSpec as StreamSlidingGroupConsSpec
import Test.RIO.Stream.SourcesSpec as StreamSourcesSpec
import Test.RIO.Stream.TakeDropUntilSpec as StreamTakeDropUntilSpec
import Test.RIO.Stream.TapMapAccumSpec as StreamTapMapAccumSpec
import Test.RIO.Stream.TimedSpec as StreamTimedSpec
import Test.RIO.STMSpec as STMSpec
import Test.RIO.STM.TArraySpec as TArraySpec
import Test.RIO.STM.TMapSpec as TMapSpec
import Test.RIO.STM.TMap.PropertiesSpec as TMapPropertiesSpec
import Test.RIO.STM.TMap.QuerySpec as TMapQuerySpec
import Test.RIO.STM.TDeferredSpec as TDeferredSpec
import Test.RIO.STM.THubSpec as THubSpec
import Test.RIO.STM.THub.PropertiesSpec as THubPropertiesSpec
import Test.RIO.STM.TQueueSpec as TQueueSpec
import Test.RIO.STM.TQueue.BulkSpec as TQueueBulkSpec
import Test.RIO.STM.TQueue.PropertiesSpec as TQueuePropertiesSpec
import Test.RIO.STM.TSemaphoreSpec as TSemaphoreSpec
import Test.RIO.STM.TSemaphore.PropertiesSpec as TSemaphorePropertiesSpec
import Test.RIO.TagSpec as TagSpec
import Test.RIO.Test.PropertySpec as TestPropertySpec
import Test.RIO.TestHelpersSpec as TestHelpersSpec
import Test.RIO.TimeSpec as TimeSpec
import Test.RIO.TracerSpec as TracerSpec
import Test.RIO.ValidationSpec as ValidationSpec

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
  PoolSpec.spec
  CacheSpec.spec
  HubSpec.spec
  HubPropertiesSpec.spec
  StreamSpec.spec
  StreamCombinatorsSpec.spec
  StreamFilterMapCollectSpec.spec
  StreamHaltInterruptSpec.spec
  StreamHeadLastFindSpec.spec
  StreamIntoSpec.spec
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
  TracerSpec.spec
  ValidationSpec.spec
  MetricSpec.spec
  MetricsSpec.spec
  LocalSpec.spec
  LoggerSpec.spec
  LoggerCompositionSpec.spec
  MemoSpec.spec
  TestHelpersSpec.spec
  TestPropertySpec.spec
  SpecHelpersSpec.spec
