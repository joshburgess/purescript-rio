module Test.RIO.Node.PathSpec (spec) where

import Prelude

import Data.String (length) as String
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

import RIO.Core (runRIO')
import RIO.Node.Path as Path

spec :: Spec Unit
spec = describe "RIO.Node.Path" do
  it "normalize collapses `..` and `.` segments" do
    Path.normalize "/foo/bar/../baz/./qux" `shouldEqual` "/foo/baz/qux"

  it "concat joins segments and normalises" do
    Path.concat [ "a", "b", "..", "c" ] `shouldEqual` "a/c"

  it "dirname / basename / extname agree on a simple path" do
    let path = "/var/log/app.txt"
    Path.dirname path `shouldEqual` "/var/log"
    Path.basename path `shouldEqual` "app.txt"
    Path.extname path `shouldEqual` ".txt"

  it "basenameWithoutExt strips a matching suffix" do
    Path.basenameWithoutExt "/tmp/a.json" ".json" `shouldEqual` "a"

  it "isAbsolute distinguishes absolute from relative paths" do
    Path.isAbsolute "/foo" `shouldEqual` true
    Path.isAbsolute "foo" `shouldEqual` false

  it "parse exposes the standard components" do
    let
      path = "/a/b/c.txt"
      parts = Path.parse path
    parts.dir `shouldEqual` "/a/b"
    parts.base `shouldEqual` "c.txt"
    parts.name `shouldEqual` "c"
    parts.ext `shouldEqual` ".txt"

  it "relative computes the relative path between two locations" do
    Path.relative "/a/b" "/a/c/d" `shouldEqual` "../c/d"

  it "sep and delimiter are non-empty platform strings" do
    String.length Path.sep `shouldSatisfy` (_ > 0)
    String.length Path.delimiter `shouldSatisfy` (_ > 0)

  it "resolve in RIO returns an absolute path" do
    absolute <- runRIO' (Path.resolve [ "a", "b" ] "c")
    Path.isAbsolute absolute `shouldEqual` true
