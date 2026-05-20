module Test.RIO.Aff.HttpStreamSpec (spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.HttpStream as Stream

spec :: Spec Unit
spec = describe "RIO.Aff.HttpStream (pull-based chunk streams)" do
  describe "fromString" do
    it "delivers the string in a single chunk, then ends" do
      s <- Stream.fromString "hello"
      a <- s
      b <- s
      a `shouldEqual` Just "hello"
      b `shouldEqual` Nothing

    it "drain reassembles the original string" do
      s <- Stream.fromString "abcdef"
      out <- Stream.drain s
      out `shouldEqual` "abcdef"

  describe "fromChunks" do
    it "yields each chunk in order" do
      s <- Stream.fromChunks [ "a", "b", "c" ]
      a <- s
      b <- s
      c <- s
      d <- s
      a `shouldEqual` Just "a"
      b `shouldEqual` Just "b"
      c `shouldEqual` Just "c"
      d `shouldEqual` Nothing

    it "drain concatenates every chunk" do
      s <- Stream.fromChunks [ "he", "llo, ", "world" ]
      out <- Stream.drain s
      out `shouldEqual` "hello, world"

    it "an empty array produces an empty stream" do
      s <- Stream.fromChunks []
      a <- s
      a `shouldEqual` Nothing

  describe "drainTo" do
    it "invokes the consumer once per chunk in order" do
      ref <- liftEffect (Ref.new ([] :: Array String))
      s <- Stream.fromChunks [ "one", "two", "three" ]
      Stream.drainTo
        ( \c -> liftEffect (Ref.modify_ (\xs -> xs <> [ c ]) ref)
        )
        s
      observed <- liftEffect (Ref.read ref)
      observed `shouldEqual` [ "one", "two", "three" ]

  describe "takeChunks" do
    it "returns up to N chunks and leaves the remainder pullable" do
      s <- Stream.fromChunks [ "a", "b", "c", "d" ]
      { chunks, rest } <- Stream.takeChunks 2 s
      chunks `shouldEqual` [ "a", "b" ]
      r1 <- rest
      r2 <- rest
      r1 `shouldEqual` Just "c"
      r2 `shouldEqual` Just "d"

    it "stops early when the stream ends before N" do
      s <- Stream.fromChunks [ "x", "y" ]
      { chunks } <- Stream.takeChunks 10 s
      chunks `shouldEqual` [ "x", "y" ]

  describe "map" do
    it "transforms each chunk on demand" do
      s <- Stream.fromChunks [ "a", "b", "c" ]
      let mapped = Stream.map (_ <> "!") s
      out <- Stream.drain mapped
      out `shouldEqual` "a!b!c!"

  describe "chunkSize" do
    it "returns the total length of all chunks combined" do
      s <- Stream.fromChunks [ "ab", "cde", "" ]
      n <- Stream.chunkSize s
      n `shouldEqual` 5
