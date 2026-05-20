module Test.RIO.Aff.Node.BufferSpec (spec) where

import Prelude

import Data.Array (length) as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import RIO.Aff.Core (RIO, runRIO')
import RIO.Aff.Node.Buffer as Buffer

runBuf :: forall a. RIO () () a -> Aff a
runBuf = runRIO'

spec :: Spec Unit
spec = describe "RIO.Aff.Node.Buffer" do
  it "alloc + size produces a zero-filled buffer of the requested size" do
    out <- runBuf do
      b <- Buffer.alloc 4
      n <- Buffer.size b
      xs <- Buffer.toArray b
      pure { n, xs }
    out.n `shouldEqual` 4
    out.xs `shouldEqual` [ 0, 0, 0, 0 ]

  it "fromArray round-trips through toArray" do
    xs <- runBuf do
      b <- Buffer.fromArray [ 1, 2, 3, 4 ]
      Buffer.toArray b
    xs `shouldEqual` [ 1, 2, 3, 4 ]

  it "fromString / toString round-trip in UTF-8" do
    s <- runBuf do
      b <- Buffer.fromString "hello" UTF8
      Buffer.toString UTF8 b
    s `shouldEqual` "hello"

  it "setAtOffset / getAtOffset reads back the written octet" do
    out <- runBuf do
      b <- Buffer.alloc 2
      Buffer.setAtOffset 42 0 b
      a <- Buffer.getAtOffset 0 b
      c <- Buffer.getAtOffset 1 b
      d <- Buffer.getAtOffset 5 b
      pure { a, c, d }
    out.a `shouldEqual` Just 42
    out.c `shouldEqual` Just 0
    out.d `shouldEqual` Nothing

  it "concat combines buffers and preserves order" do
    xs <- runBuf do
      b1 <- Buffer.fromArray [ 1, 2 ]
      b2 <- Buffer.fromArray [ 3, 4 ]
      b3 <- Buffer.fromArray [ 5 ]
      c <- Buffer.concat [ b1, b2, b3 ]
      Buffer.toArray c
    xs `shouldEqual` [ 1, 2, 3, 4, 5 ]

  it "concat' truncates / pads to the requested length" do
    out <- runBuf do
      b1 <- Buffer.fromArray [ 1, 2, 3 ]
      b2 <- Buffer.fromArray [ 4, 5, 6 ]
      c <- Buffer.concat' [ b1, b2 ] 4
      Buffer.toArray c
    Array.length out `shouldEqual` 4
    out `shouldEqual` [ 1, 2, 3, 4 ]

  it "slice is a pure view that mirrors writes back to the source" do
    out <- runBuf do
      src <- Buffer.fromArray [ 10, 20, 30, 40 ]
      let sl = Buffer.slice 1 3 src
      before <- Buffer.toArray sl
      Buffer.setAtOffset 99 0 sl
      afterSlice <- Buffer.toArray sl
      afterSrc <- Buffer.toArray src
      pure { before, afterSlice, afterSrc }
    out.before `shouldEqual` [ 20, 30 ]
    out.afterSlice `shouldEqual` [ 99, 30 ]
    out.afterSrc `shouldEqual` [ 10, 99, 30, 40 ]

  it "fill writes the same octet across a range" do
    xs <- runBuf do
      b <- Buffer.alloc 5
      Buffer.fill 7 1 4 b
      Buffer.toArray b
    xs `shouldEqual` [ 0, 7, 7, 7, 0 ]

  it "copy moves bytes between buffers and reports the count" do
    out <- runBuf do
      src <- Buffer.fromArray [ 1, 2, 3, 4 ]
      tgt <- Buffer.alloc 4
      n <- Buffer.copy 0 4 src 0 tgt
      xs <- Buffer.toArray tgt
      pure { n, xs }
    out.n `shouldEqual` 4
    out.xs `shouldEqual` [ 1, 2, 3, 4 ]

  it "freeze / thaw isolates the immutable snapshot from later writes" do
    out <- runBuf do
      src <- Buffer.fromArray [ 1, 2, 3 ]
      frozen <- Buffer.freeze src
      Buffer.setAtOffset 99 0 src
      restored <- Buffer.thaw frozen
      Buffer.toArray restored
    out `shouldEqual` [ 1, 2, 3 ]
