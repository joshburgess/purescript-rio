module Test.RIO.Node.FileSystemSpec (spec) where

import Prelude

import Data.Maybe (isJust, isNothing)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (mkdtemp, rm') as Cleanup
import Node.FS.Stats (size) as Stats
import Node.Path (concat)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Core (RIO, provideAll, runRIO')
import RIO.Node.FileSystem
  ( FileSystem
  , access
  , appendTextFile
  , liveFileSystem
  , mkdir
  , readTextFile
  , readdir
  , rename
  , stat
  , unlink
  , writeTextFile
  )

-- | Run an `RIO (fs :: FileSystem) ()` program under the live
-- | filesystem. The closed `()` error row means `runRIO'` can
-- | return the success value directly; any `Aff`-level exceptions
-- | still propagate, which is the same surface `Node.FS.Aff` has.
runFs :: forall a. RIO (fs :: FileSystem) () a -> Aff a
runFs program = runRIO' (provideAll { fs: liveFileSystem } program)

withTempDir :: forall a. (String -> Aff a) -> Aff a
withTempDir k = do
  dir <- Cleanup.mkdtemp "/tmp/rio-node-fs-"
  a <- k dir
  Cleanup.rm' dir
    { force: true, maxRetries: 0, recursive: true, retryDelay: 0 }
  pure a

spec :: Spec Unit
spec = describe "RIO.Node.FileSystem (live)" do
  it "writeTextFile / readTextFile round-trips through the service" do
    withTempDir \dir -> do
      let path = concat [ dir, "hello.txt" ]
      content <- runFs do
        writeTextFile UTF8 path "hello, rio-node"
        readTextFile UTF8 path
      content `shouldEqual` "hello, rio-node"

  it "appendTextFile concatenates content" do
    withTempDir \dir -> do
      let path = concat [ dir, "log.txt" ]
      content <- runFs do
        writeTextFile UTF8 path "first"
        appendTextFile UTF8 path " second"
        readTextFile UTF8 path
      content `shouldEqual` "first second"

  it "mkdir + readdir lists directory entries" do
    withTempDir \dir -> do
      let
        sub = concat [ dir, "sub" ]
        f1 = concat [ sub, "a.txt" ]
        f2 = concat [ sub, "b.txt" ]
      entries <- runFs do
        mkdir sub
        writeTextFile UTF8 f1 "a"
        writeTextFile UTF8 f2 "b"
        readdir sub
      entries `shouldEqual` [ "a.txt", "b.txt" ]

  it "stat reports file size" do
    withTempDir \dir -> do
      let path = concat [ dir, "size.txt" ]
      sz <- runFs do
        writeTextFile UTF8 path "abcdef"
        stats <- stat path
        pure (Stats.size stats)
      sz `shouldEqual` 6.0

  it "rename moves a file" do
    withTempDir \dir -> do
      let
        src = concat [ dir, "src.txt" ]
        dest = concat [ dir, "dest.txt" ]
      out <- runFs do
        writeTextFile UTF8 src "payload"
        rename src dest
        readTextFile UTF8 dest
      out `shouldEqual` "payload"

  it "access after unlink reports a missing file" do
    withTempDir \dir -> do
      let path = concat [ dir, "doomed.txt" ]
      gone <- runFs do
        writeTextFile UTF8 path "x"
        unlink path
        access path
      gone `shouldSatisfy` isJust

  it "access on an existing file returns Nothing" do
    withTempDir \dir -> do
      let path = concat [ dir, "present.txt" ]
      ok <- runFs do
        writeTextFile UTF8 path "."
        access path
      ok `shouldSatisfy` isNothing
