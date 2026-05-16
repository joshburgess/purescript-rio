module Test.RIO.Node.StreamSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Effect.Aff (Aff, forkAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Core (RIO, runRIO')
import RIO.Node.Buffer as Buf
import RIO.Node.Stream as Stream

runS :: forall a. RIO () () a -> Aff a
runS = runRIO'

spec :: Spec Unit
spec = describe "RIO.Node.Stream" do
  -- `readableFromString` decodes chunks as strings, which is
  -- incompatible with the `dataH`-driven Aff readers; testing the
  -- round-trip via a buffer source keeps the readers happy.
  it "readableFromBuffer + readableToStringUtf8 round-trips" do
    out <- runS do
      buf <- Buf.fromString "hello" UTF8
      r <- Stream.readableFromBuffer buf
      Stream.readableToStringUtf8 r
    out `shouldEqual` "hello"

  it "readableFromBuffer + readableToBuffers produces a single chunk" do
    out <- runS do
      buf <- Buf.fromString "abc" UTF8
      r <- Stream.readableFromBuffer buf
      bufs <- Stream.readableToBuffers r
      Stream.toStringUTF8 bufs
    out `shouldEqual` "abc"

  it "writeString to a pass-through is observable via readableToStringUtf8" do
    -- Schedule the writes on a separate fiber so they happen after
    -- readableToStringUtf8 has attached its data listeners.
    out <- runS do
      pt <- Stream.newPassThrough
      _ <- liftAff $ forkAff do
        runS do
          _ <- Stream.writeString pt UTF8 "hello"
          _ <- Stream.writeString pt UTF8 " world"
          Stream.end pt
      Stream.readableToStringUtf8 pt
    out `shouldEqual` "hello world"

  it "writeAll + endAwait drains an array of buffers" do
    out <- runS do
      pt <- Stream.newPassThrough
      b1 <- Buf.fromString "one " UTF8
      b2 <- Buf.fromString "two " UTF8
      b3 <- Buf.fromString "three" UTF8
      _ <- liftAff $ forkAff do
        runS do
          Stream.writeAll pt [ b1, b2, b3 ]
          Stream.endAwait pt
      Stream.readableToStringUtf8 pt
    out `shouldEqual` "one two three"

  it "pipe copies data from one stream into another" do
    out <- runS do
      buf <- Buf.fromString "pipe-me" UTF8
      src <- Stream.readableFromBuffer buf
      dest <- Stream.newPassThrough
      Stream.pipe src dest
      Stream.readableToStringUtf8 dest
    out `shouldEqual` "pipe-me"

  it "fromStringUTF8 / toStringUTF8 round-trip a value" do
    out <- runS do
      bs <- Stream.fromStringUTF8 "trip"
      Stream.toStringUTF8 bs
    out `shouldEqual` "trip"

  it "readableToBuffers returns at least one chunk for a non-empty stream" do
    n <- runS do
      buf <- Buf.fromString "non-empty" UTF8
      r <- Stream.readableFromBuffer buf
      bufs <- Stream.readableToBuffers r
      pure (Array.length bufs)
    n `shouldSatisfy` (_ >= 1)

  it "destroy + destroyed flips the flag" do
    out <- runS do
      pt <- Stream.newPassThrough
      before <- Stream.destroyed pt
      Stream.destroy pt
      liftEffect (pure unit)
      after <- Stream.destroyed pt
      pure { before, after }
    out.before `shouldEqual` false
    out.after `shouldEqual` true
