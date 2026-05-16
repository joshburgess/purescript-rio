module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

import Test.RIO.Node.FileSystemSpec as FileSystemSpec
import Test.RIO.Node.OSSpec as OSSpec
import Test.RIO.Node.PathSpec as PathSpec
import Test.RIO.Node.ProcessSpec as ProcessSpec
import Test.RIO.Node.URLSpec as URLSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  FileSystemSpec.spec
  OSSpec.spec
  PathSpec.spec
  ProcessSpec.spec
  URLSpec.spec
