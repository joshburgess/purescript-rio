module Test.RIO.ChannelSpec (spec) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Channel
  ( Channel
  , done
  , emit
  , fromSink
  , fromStream
  , pipe
  , run
  )
import RIO.Core (runRIO')
import RIO.Sink (collect, count, foldL) as Sink
import RIO.Stream as Stream

spec :: Spec Unit
spec = describe "RIO.Channel" do

  describe "fromStream / fromSink / pipe / run" do
    it "pipes a stream into a collecting sink" do
      let
        ch :: Channel () () Void Void (Array Int)
        ch = pipe
          (fromStream (Stream.fromArray [ 1, 2, 3, 4, 5 ]))
          (fromSink Sink.collect)
      result <- runRIO' (run ch)
      result `shouldEqual` [ 1, 2, 3, 4, 5 ]

    it "pipes into a counting sink" do
      let
        ch :: Channel () () Void Void Int
        ch = pipe
          (fromStream (Stream.fromArray [ 10, 20, 30 ]))
          (fromSink Sink.count)
      result <- runRIO' (run ch)
      result `shouldEqual` 3

    it "pipes into a fold sink" do
      let
        ch :: Channel () () Void Void Int
        ch = pipe
          (fromStream (Stream.fromArray [ 1, 2, 3, 4 ]))
          (fromSink (Sink.foldL 0 (+)))
      result <- runRIO' (run ch)
      result `shouldEqual` 10

    it "handles an empty stream by running the sink's EOF branch" do
      let
        ch :: Channel () () Void Void (Array Int)
        ch = pipe
          (fromStream (Stream.fromArray []))
          (fromSink Sink.collect)
      result <- runRIO' (run ch)
      result `shouldEqual` []

  describe "primitive constructors" do
    it "done terminates immediately with the given value" do
      let
        ch :: Channel () () Void Void Int
        ch = done 42
      result <- runRIO' (run ch)
      result `shouldEqual` 42

    it "emit produces an output, then continues" do
      let
        -- A channel that emits 1 then terminates. run discards
        -- outputs from a closed channel, so the terminal value
        -- is what's returned.
        ch :: Channel () () Void Int Int
        ch = emit 1 (emit 2 (done 99))
      -- pipe with a count sink to demonstrate the emits did fire.
      let
        closed :: Channel () () Void Void Int
        closed = pipe ch (fromSink Sink.count)
      result <- runRIO' (run closed)
      result `shouldEqual` 2
