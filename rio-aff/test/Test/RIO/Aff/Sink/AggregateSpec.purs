module Test.RIO.Aff.Sink.AggregateSpec (spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (runRIO')
import RIO.Aff.Sink as Sink
import RIO.Aff.Stream as Stream

spec :: Spec Unit
spec = describe "RIO.Aff.Sink (aggregate / transduce)" do

  describe "aggregate" do
    it "chunks an input stream into fixed-size groups with take" do
      result <- runRIO' do
        Stream.runCollect
          (Sink.aggregate (Sink.take 3) (Stream.fromArray [ 1, 2, 3, 4, 5, 6, 7 ]))
      result `shouldEqual` [ [ 1, 2, 3 ], [ 4, 5, 6 ], [ 7 ] ]

    it "emits a single chunk when the stream is shorter than the chunk size" do
      result <- runRIO' do
        Stream.runCollect
          (Sink.aggregate (Sink.take 10) (Stream.fromArray [ 1, 2, 3 ]))
      result `shouldEqual` [ [ 1, 2, 3 ] ]

    it "emits exact-fit chunks without a trailing partial chunk" do
      result <- runRIO' do
        Stream.runCollect
          (Sink.aggregate (Sink.take 2) (Stream.fromArray [ 1, 2, 3, 4 ]))
      result `shouldEqual` [ [ 1, 2 ], [ 3, 4 ] ]

    it "yields the empty stream when the input stream is empty" do
      result <- runRIO' do
        Stream.runCollect
          (Sink.aggregate (Sink.take 5) (Stream.fromArray ([] :: Array Int)))
      result `shouldEqual` []

    it "runs the sink's finish action when the stream ends mid-consumption" do
      -- count never halts on its own; it returns the running tally
      -- via `finish` when the stream ends. So aggregate count emits
      -- one element (the total length).
      result <- runRIO' do
        Stream.runCollect
          (Sink.aggregate Sink.count (Stream.fromArray [ 10, 20, 30, 40 ]))
      result `shouldEqual` [ 4 ]

    it "emits no chunks when the input stream is empty" do
      result <- runRIO' do
        Stream.runCollect
          (Sink.aggregate Sink.count (Stream.fromArray ([] :: Array Int)))
      result `shouldEqual` []

    it "composes with map: aggregate then map over each chunk" do
      result <- runRIO' do
        Stream.runCollect
          ( Stream.map (\xs -> xs <> xs)
              ( Sink.aggregate (Sink.take 2)
                  (Stream.fromArray [ 1, 2, 3, 4 ])
              )
          )
      result `shouldEqual` [ [ 1, 2, 1, 2 ], [ 3, 4, 3, 4 ] ]

  describe "transduce" do
    it "is an alias for aggregate" do
      result <- runRIO' do
        Stream.runCollect
          (Sink.transduce (Sink.take 2) (Stream.fromArray [ 1, 2, 3, 4, 5 ]))
      result `shouldEqual` [ [ 1, 2 ], [ 3, 4 ], [ 5 ] ]
